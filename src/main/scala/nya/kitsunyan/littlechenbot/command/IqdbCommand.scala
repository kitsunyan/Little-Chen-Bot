package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import nya.kitsunyan.littlechenbot.service.BooruService

import scala.concurrent.Future

trait IqdbCommand extends Command with ExtractImage with Http {
  override def handleMessage(implicit message: Message): Unit = {
    filterMessage("iqdb")
      .map(handleMessageInternal)
      .getOrElse(super.handleMessage)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Unit = {
    case class IqdbResult(url: String, similarity: Int, matches: Boolean)

    def sendIqdbRequest(minSimilarity: Int)(telegramFile: TelegramFile): List[IqdbResult] = {
      val response = http("https://iqdb.org/")
        .postMulti(telegramFile.multiPart("file"))
        .params(BooruService.list.map("service[]" -> _.iqdbId)).asString

      val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
        "<td>(\\d+)% similarity</td>.*?</table>").r
      val linkPattern = "<a href=\"(.*?)\">".r

      (for {
        table <- tablePattern.findAllIn(response.body).matchData
        similarity = table.group(1).toInt
        links <- linkPattern.findAllIn(table.group(0)).matchData.map(_.subgroups)
        url = links.head match {
          case s if s.startsWith("//") => "https:" + s
          case s => s
        }
        matches = similarity >= minSimilarity
      } yield IqdbResult(url, similarity, matches)).toList
    }

    case class ImageData(url: String, pageUrl: String, characters: Set[String], copyrights: Set[String],
      artists: Set[String], image: Option[Array[Byte]] = None)

    def readBooruPage(pageUrl: String): (ImageData, BooruService) = {
      BooruService.findByUrl(pageUrl).map { booruService =>
        val response = http(pageUrl, proxy = true).asString
        if (response.code == 200) {
          booruService.parseHtml(response.body) match {
            case Some((url, characters, copyrights, artists)) =>
              val imageData = ImageData(url, pageUrl, characters, copyrights, artists)
              (imageData, booruService)
            case None => throw new CommandException(s"Not parsed: $pageUrl.")
          }
        } else {
          val code = response.code
          throw new Exception(s"Invalid response: $code.")
        }
      }.getOrElse(throw new CommandException("Unknown service."))
    }

    def readBooruPages(iqdbResults: List[IqdbResult]): ImageData = {
      case class BooruPage(imageData: Option[ImageData], service: BooruService, similarity: Int)
      case class Result(list: List[BooruPage], exception: Option[Exception])

      val result = iqdbResults.foldLeft(Result(Nil, None)) { (result, iqdbResult) =>
        if (iqdbResult.matches) {
          try {
            val (imageData, booruService) = readBooruPage(iqdbResult.url)
            result.copy(list = BooruPage(Some(imageData), booruService, iqdbResult.similarity) :: result.list)
          } catch {
            case e: Exception => result.copy(exception = result.exception orElse Some(e))
          }
        } else {
          BooruService.findByUrl(iqdbResult.url)
            .map(s => result.copy(list = BooruPage(None, s, iqdbResult.similarity) :: result.list))
            .getOrElse(result)
        }
      }

      result.list
        .map(_.imageData).find(_.nonEmpty).flatten
        .getOrElse {
        if (result.list.nonEmpty) {
          val notFoundMessage = result.exception.map { e =>
            handleException(e)
            "No images found (due to exception thrown)."
          }.getOrElse("No images found.")

          val (_, additionalResults) = result.list.map { booruPage =>
            val serviceName = booruPage.service.commonNames.head
            val similarity = booruPage.similarity
            s"$similarity% — $serviceName"
          }.foldLeft((1, "")) { (tuple, value) =>
            val (count, result) = tuple
            (count + 1, result + s"\n$count: $value")
          }

          throw new CommandException(s"$notFoundMessage\n\nAdditional results:$additionalResults")
        } else {
          throw result.exception.getOrElse(new CommandException("No images found."))
        }
      }
    }

    def readBooruImage(imageData: ImageData): ImageData = {
      val response = http(imageData.url, proxy = true).asBytes
      if (response.code == 200) {
        imageData.copy(image = Some(response.body))
      } else {
        val code = response.code
        throw new Exception(s"Invalid response: $code.")
      }
    }

    def replyWithImage(imageData: ImageData): Future[Message] = {
      val name = Some(imageData.url).map { text =>
        if (text == null || text == "") "image.jpeg" else text
      }.map { text =>
        val index = text.lastIndexOf('/')
        if (index >= 0) text.substring(index + 1) else text
      }.map { text =>
        val index = text.indexOf('?')
        if (index >= 0) text.substring(0, index) else text
      }.get

      def appendIterable(title: String, list: Iterable[String])(s: String): String = {
        if (list.nonEmpty) s + s"\n$title: " + list.reduceLeft(_ + ", " + _) else s
      }

      val captionOption = Some(imageData.pageUrl).map(appendIterable("Characters", imageData.characters))
        .map(appendIterable("Copyrights", imageData.copyrights)).map(appendIterable("Artists", imageData.artists))
      request(SendDocument(Left(message.sender), Left(InputFile(name, imageData.image.get)),
        replyToMessageId = Some(message.messageId), caption = captionOption))
    }

    arguments.string("h", "help") match {
      case Some(_) =>
        replyQuote("Fetch image from *booru using iqdb.org." +
          "\n\n" + "-s, --min-similarity [0-100] — set minimum allowed similarity for found images." +
          "\n" + "-h, --help — display this help.")
      case None =>
        val similarity = arguments.int("s", "min-similarity")
          .map(s => if (s > 100) 100 else if (s < 0) 0 else s)
          .getOrElse(70)

        Future(obtainMessageFileId).flatMap(readTelegramFile).map(sendIqdbRequest(similarity))
          .map(readBooruPages).map(readBooruImage).flatMap(replyWithImage).recoverWith(handleError("image request"))
    }
  }
}
