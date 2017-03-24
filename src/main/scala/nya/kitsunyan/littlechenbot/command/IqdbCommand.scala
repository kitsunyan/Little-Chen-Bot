package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import nya.kitsunyan.littlechenbot.service.BooruService

import scala.concurrent.Future

trait IqdbCommand extends Command with ExtractImage with Http {
  private val commands = List("iqdb")

  override def handleMessage(implicit message: Message): Unit = {
    filterMessage(commands)
      .map(handleMessageInternal)
      .getOrElse(super.handleMessage)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Unit = {
    case class IqdbResult(url: String, booruService: BooruService, similarity: Int, matches: Boolean)

    def sendIqdbRequest(minSimilarity: Int)(telegramFile: TelegramFile): List[IqdbResult] = {
      http("https://iqdb.org/")
        .postMulti(telegramFile.multiPart("file"))
        .params(BooruService.list.map("service[]" -> _.iqdbId)).response(_.asString) { _ => body =>
        val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
          "<td>(\\d+)% similarity</td>.*?</table>").r
        val linkPattern = "<a href=\"(.*?)\">".r

        (for {
          table <- tablePattern.findAllIn(body).matchData
          similarity = table.group(1).toInt
          links <- linkPattern.findAllIn(table.group(0)).matchData.map(_.subgroups)
          url = links.head match {
            case s if s.startsWith("//") => "https:" + s
            case s => s
          }
          matches = similarity >= minSimilarity
          booruService = BooruService.findByUrl(url)
          if booruService.nonEmpty
        } yield IqdbResult(url, booruService.get, similarity, matches)).toList
      }
    }

    def applyPriority(priority: List[String])(iqdbResults: List[IqdbResult]): List[IqdbResult] = {
      val servicesPriority = priority.flatMap(BooruService.findByName)

      iqdbResults.sortWith { (a, b) =>
        val aindex = servicesPriority.indexOf(a.booruService)
        val bindex = servicesPriority.indexOf(b.booruService)
        if (aindex >= 0 && bindex >= 0) {
          if (aindex == bindex) {
            a.similarity > b.similarity
          } else {
            aindex < bindex
          }
        } else if (aindex >= 0 || bindex >= 0) {
          aindex >= 0
        } else {
          a.similarity > b.similarity
        }
      }
    }

    case class ImageData(url: String, pageUrl: String,
      characters: Set[String], copyrights: Set[String], artists: Set[String])

    case class ReadImageData(name: String, image: Array[Byte], pageUrl: String,
      characters: Set[String], copyrights: Set[String], artists: Set[String])

    def readBooruPage(iqdbResult: IqdbResult): ImageData = {
      val pageUrl = iqdbResult.url
      http(pageUrl, proxy = true).response(_.asString) { _ => body =>
        iqdbResult.booruService.parseHtml(body) match {
          case Some((url, characters, copyrights, artists)) =>
            ImageData(url, pageUrl, characters, copyrights, artists)
          case None => throw new Exception(s"Not parsed: $pageUrl.")
        }
      }
    }

    def readBooruImage(imageData: ImageData): ReadImageData = {
      http(imageData.url, proxy = true).response(_.asBytes) { _ => body =>
        val name = {
          val url = imageData.url
          val start = url.lastIndexOf('/') + 1
          val end = url.indexOf('?', start)
          if (end >= start) url.substring(start, end) else url.substring(start)
        }

        ReadImageData(name, body, imageData.pageUrl,
          imageData.characters, imageData.copyrights, imageData.artists)
      }
    }

    def readBooruImages(iqdbResults: List[IqdbResult]): ReadImageData = {
      case class Result(successImageData: Option[ReadImageData] = None,
        additionalIqdbResults: List[IqdbResult] = Nil, exception: Option[Exception] = None)

      val result = iqdbResults.foldLeft(Result()) { (result, iqdbResult) =>
        if (result.successImageData.isEmpty && iqdbResult.matches) {
          try {
            result.copy(successImageData = Some(readBooruImage(readBooruPage(iqdbResult))))
          } catch {
            case e: Exception => result.copy(exception = result.exception orElse Some(e))
          }
        } else {
          result.copy(additionalIqdbResults = iqdbResult :: result.additionalIqdbResults)
        }
      }

      result.successImageData.getOrElse {
        val notFoundMessage = result.exception.map { e =>
          handleException(e, messageWithImageAsCausal)
          "No images found (due to exception thrown)."
        }.getOrElse("No images found.")

        val (_, additionalResults) = result.additionalIqdbResults.reverse.map { iqdbResult =>
          val serviceName = iqdbResult.booruService.commonNames.head
          val similarity = iqdbResult.similarity
          s"$similarity% — $serviceName"
        }.foldLeft((1, "")) { (tuple, value) =>
          val (count, result) = tuple
          (count + 1, result + s"\n$count: $value")
        }

        if (additionalResults.isEmpty) {
          throw result.exception.getOrElse(new CommandException(s"$notFoundMessage"))
        } else {
          throw new CommandException(s"$notFoundMessage\n\nAdditional results:$additionalResults")
        }
      }
    }

    def replyWithImage(imageData: ReadImageData): Future[Message] = {
      def appendIterable(title: String, list: Iterable[String])(s: String): String = {
        if (list.nonEmpty) s + s"\n$title: " + list.reduceLeft(_ + ", " + _) else s
      }

      val captionOption = Some(imageData.pageUrl).map(appendIterable("Characters", imageData.characters))
        .map(appendIterable("Copyrights", imageData.copyrights)).map(appendIterable("Artists", imageData.artists))
      request(SendDocument(Left(message.sender), Left(InputFile(imageData.name, imageData.image)),
        replyToMessageId = Some(message.messageId), caption = captionOption))
    }

    (arguments.string("h", "help"), arguments.string(null, "example")) match {
      case (Some(_), _) =>
        replyMan("Fetch image from \\*booru using iqdb.org.",
          (List("-s", "--min-similarity"), Some("0-100"),
            "Set minimum allowed similarity for found images.") ::
          (List("-p", "--priority"), Some("string list"),
            "Set priority for \\*booru services.") ::
          (List("--example"), None,
            "Print examples of usage.") ::
          (List("-h", "--help"), None,
            "Display this help.") ::
          Nil)
      case (_, Some(_)) =>
        replyQuote("Examples of usage:" +
          "\n\nFetch first image with similarity >= 50%:" +
          "\n    `/iqdb --min-similarity 50`" +
          "\n    `/iqdb -s 50`" +
          "\n\nFetch first image from danbooru if possible:" +
          "\n    `/iqdb --priority danbooru`" +
          "\n    `/iqdb -p danbooru`" +
          "\n\nFetch first image from danbooru or gelbooru if possible:" +
          "\n    `/iqdb -p \"danbooru gelbooru\"`" +
          "\n\nFetch first image from danbooru with similarity >= 50%:" +
          "\n    `/iqdb -p danbooru -s 50`",
          Some(ParseMode.Markdown))
      case _ =>
        val similarity = arguments.int("s", "min-similarity")
          .map(s => if (s > 100) 100 else if (s < 0) 0 else s)
          .getOrElse(70)

        val priority = arguments.string("p", "priority")
          .map(_.split(",|\\s+").toList.filter(!_.isEmpty)).getOrElse(Nil)

        Future(obtainMessageFileId(commands.head, messageWithImage)).flatMap(readTelegramFile)
          .map(sendIqdbRequest(similarity)).map(applyPriority(priority)).map(readBooruImages)
          .flatMap(replyWithImage).recoverWith(handleError(messageWithImageAsCausal, "image request"))
    }
  }
}
