package nya.kitsunyan.littlechenbot.service

import nya.kitsunyan.littlechenbot.command.common.Http
import nya.kitsunyan.littlechenbot.util._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent._

object TranslateService {
  private val actor = ActorSystem("TranslateService").actorOf(Props(new TranslateActor))
  private implicit val timeout: Timeout = Timeout(1, TimeUnit.DAYS)

  private case class ObtainCalculationScript(http: Http, executionContext: ExecutionContext)
  private case class UpdateCalculationScript(calculationScript: String)

  private class TranslateActor extends Actor {
    private val lifetime = 60 * 60 * 1000L

    private var lastCalculationUpdate = 0L
    private var lastCalculationScript = ""

    def receive: Receive = {
      case ObtainCalculationScript(http, executionContext) =>
        implicit val httpImplicit: Http = http
        implicit val executionContextImplicit: ExecutionContext = executionContext

        val time = System.currentTimeMillis
        if (time > lastCalculationUpdate + lifetime) {
          sender ! obtainCalculationScript.map { calculationScript =>
            self ! UpdateCalculationScript(calculationScript)
            calculationScript
          }
        } else {
          sender ! Future.successful(lastCalculationScript)
        }
      case UpdateCalculationScript(calculationScript) =>
        lastCalculationUpdate = System.currentTimeMillis
        lastCalculationScript = calculationScript
    }
  }

  private val host = "translate.google.com"
  private val https = true
  private val origin = s"${if (https) "https" else "http"}://$host"

  class TranslateException(userMessageValue: String) extends Exception with UserMessageException {
    override val userMessage: Option[String] = Some(userMessageValue)
  }

  class UnsupportedLanguageException extends TranslateException("Unsupported language")

  private def obtainCalculationScript(implicit http: Http, executionContext: ExecutionContext): Future[String] = {
    http.http(origin).runString(Http.Filters.ok).flatMap { response =>
      @tailrec def findTkkEvalEnd(eval: String, fromIndex: Int): Option[Int] = {
        val index = eval.indexOf("')", fromIndex)
        if (index > 0) {
          if (eval(index - 1) != '\\') {
            Some(index + 2)
          } else {
            findTkkEvalEnd(eval, index + 1)
          }
        } else {
          None
        }
      }

      val tkkEvalIndex = response.body.indexOf("TKK=eval('") match {
        case i if i <= -1 => Left(new TranslateException("TKK is not found"))
        case i => Right(i)
      }

      val tkkEvalScript = tkkEvalIndex
        .flatMap(tkkEvalIndex => findTkkEvalEnd(response.body, tkkEvalIndex)
          .map(response.body.substring(tkkEvalIndex, _))
          .map(Right.apply)
          .getOrElse(Left(new TranslateException("Can not parse TKK"))))

      val desktopScriptUrl = "(?<=\")[^\"]*/desktop_module_main.js(?=\")".r
        .findFirstIn(response.body).map(Utils.appendSchemeHost(https, host))
        .map(Right.apply)
        .getOrElse(Left(new TranslateException("Can not find JS module path")))

      tkkEvalScript.flatMap(tkkEvalScript => desktopScriptUrl
        .map(desktopScriptUrl => http.http(desktopScriptUrl).runString(Http.Filters.ok).map { response =>
        val possibleOccurrences =
          "String.fromCharCode(84)" ::
          "window[b.join(c())]" ::
          "String.fromCharCode(116)" ::
          Nil

        val functionName = (for {
          occurrenceIndex <- possibleOccurrences.foldLeft[Option[Int]](None) { (a, v) =>
            a orElse {
              val index = response.body.indexOf(v)
              if (index >= 0) Some(index) else None
            }
          }
          matchData <- "(?:[;,)}\\s]+(\\w+)\\s*=\\s*)?function(?:\\s+(\\w+)(?: |\\())?".r
            .findAllIn(response.body.substring(0, occurrenceIndex))
            .matchData.map(_.subgroups).toList.lastOption
          name <- Option(matchData(0)) orElse Option(matchData(1))
        } yield name).getOrElse(throw new RuntimeException)

        val fixScript =
          """
            |window = this
            |navigator = {}
            |document = {createElement: function() {return {}}}
            |window.jstiming = {load: {tick: function() {}}}
          """.stripMargin

        s"$fixScript\nthis.$tkkEvalScript\n${response.body}\nconsole.log($functionName(process.argv[2]))"
      })) match {
        case Right(future) => future
        case Left(error) => Future.failed(error)
      }
    }
  }

  private def tkArgument(input: String)(implicit binaries: Binaries,
    http: Http, executionContext: ExecutionContext): Future[String] = {
    (actor ? ObtainCalculationScript(http, executionContext))
      .mapTo[Future[String]].flatten.map { calculationScript =>
      val file = java.io.File.createTempFile("littlechenbot", ".js")
      try {
        val output = new java.io.FileOutputStream(file)
        try {
          output.write(calculationScript.getBytes("UTF-8"))
        } finally {
          output.close()
        }

        new String(Utils.exec(None, Seq(binaries.node, file.getAbsolutePath, input)), "UTF-8")
      } finally {
        file.delete()
      }
    }
  }

  private def urlEncode(s: String): String = {
    java.net.URLEncoder.encode(s, "UTF-8")
  }

  def transliterate(input: String)(implicit binaries: Binaries,
    http: Http, executionContext: ExecutionContext): Future[String] = {
    tkArgument(input)
      .flatMap(tk => http
        .http(s"$origin/translate_a/single?sl=auto&tl=en&dt=rm&ie=UTF-8&oe=UTF-8&q=${urlEncode(input)}&client=t$tk")
        .runString(Http.Filters.ok))
      .map { response =>
        import org.json4s._
        import org.json4s.jackson.JsonMethods._

        val result = parse(response.body)(0)(0)(3) match {
          case JString(s) => Some(s)
          case _ => None
        }

        result.filter(_.nonEmpty).getOrElse(input)
      }
  }

  def guessLanguage(input: String)(implicit binaries: Binaries,
    http: Http, executionContext: ExecutionContext): Future[Option[String]] = {
    tkArgument(input)
      .flatMap(tk => http
        .http(s"$origin/translate_a/single?sl=auto&tl=en&ie=UTF-8&oe=UTF-8&q=${urlEncode(input)}&client=t$tk")
        .runString(Http.Filters.ok))
      .map { response =>
        import org.json4s._
        import org.json4s.jackson.JsonMethods._

        parse(response.body)(2) match {
          case JString(s) => Some(s)
          case _ => None
        }
      }
  }

  private def mp3ToOpus(data: Array[Byte])(implicit binaries: Binaries): Array[Byte] = {
    Utils.exec(Some(data), Seq(binaries.ffmpeg, "-f", "mp3", "-i", "-", "-f", "opus", "-"))
  }

  def textToSpeech(input: String, language: String)(implicit binaries: Binaries,
    http: Http, executionContext: ExecutionContext): Future[Array[Byte]] = {
    def request(tk: String, language: String): Future[Http.HttpResponse[Array[Byte]]] = {
      http.http(s"$origin/translate_tts?q=${urlEncode(input)}&tl=${urlEncode(language)}&client=t$tk")
        .runBytes(Http.Filters.code(200, 204))
    }

    tkArgument(input)
      .flatMap(tk => request(tk, language)
        .recoverWith {
          case e =>
            // Check language is supported
            request(tk, "en")
              .recoverWith {
                case _ => Future.failed(e)
              }
              .flatMap(_ => Future.failed(new UnsupportedLanguageException))
        })
      .map { response =>
        if (response.code == 204) {
          throw new TranslateException("Unable to obtain audio file")
        } else {
          val contentType = response.headers("Content-Type").headOption

          if (contentType.contains("audio/mpeg") || contentType.contains("audio/mp3")) {
            mp3ToOpus(response.body)
          } else {
            throw new TranslateException("Invalid result")
          }
        }
      }
  }
}
