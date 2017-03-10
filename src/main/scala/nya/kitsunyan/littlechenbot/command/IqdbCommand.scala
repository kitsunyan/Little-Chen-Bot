package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import nya.kitsunyan.littlechenbot.Utils
import nya.kitsunyan.littlechenbot.service.BooruService

import scala.concurrent.Future
import scalaj.http._

trait IqdbCommand extends Command {
  def http(url: String, proxy: Boolean = false): HttpRequest

  override def handleMessage(implicit message: Message): Unit = {
    if (filterMessage("iqdb")) handleMessageInternal else super.handleMessage
  }

  private def handleMessageInternal(implicit message: Message): Unit = {
    def extractMessageWithImage(message: Message): Option[Message] = {
      message.caption.map(_ => message) orElse message.replyToMessage
    }

    def extractFileId(message: Message): Option[String] = {
      message.photo.map { photos =>
        photos.reduceLeft { (photo1, photo2) =>
          // Find the largest image
          if (photo2.fileSize.getOrElse(0) >= photo1.fileSize.getOrElse(0)) photo2 else photo1
        }.fileId
      } orElse message.sticker.map(_.fileId) orElse message.document.flatMap { document =>
        document.mimeType match {
          case Some(mimeType) if mimeType.startsWith("image/") => Some(document.fileId)
          case _ => None
        }
      }
    }

    def obtainMessageFileId: String = {
      extractMessageWithImage(message).flatMap(extractFileId) match {
        case Some(fileId) => fileId
        case None =>
          throw new CommandException("Please reply to message with image or send image with command in caption.\n" +
            "Remember I can't see other bots' messages even when you reply them!")
      }
    }

    def readTelegramFile(fileId: String): Future[(Array[Byte], String)] = {
      request(GetFile(fileId)).map { file =>
        file.filePath match {
          case Some(path) =>
            val telegramImageUrl = s"https://api.telegram.org/file/bot$token/$path"
            (Some(http(telegramImageUrl).asBytes.body).map { array =>
              if (path.endsWith(".webp")) Utils.webpToPng(array) else array
            }.get, path)
          case None => throw new CommandException("Unable to fetch Telegram file.")
        }
      }
    }

    def sendIqdbRequest(minSimilarity: Int)(array: Array[Byte], path: String): List[String] = {
      val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
      val response = http("https://iqdb.org/")
        .postMulti(MultiPart("file", "filename", mimeType, array))
        .params(BooruService.list.map("service[]" -> _.iqdbId)).asString

      val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
        "<td>(\\d+)% similarity</td>.*?</table>").r
      val linkPattern = "<a href=\"(.*?)\">".r

      (for {
        table <- tablePattern.findAllIn(response.body).matchData
        similarity = table.group(1).toInt
        if similarity >= minSimilarity
        links <- linkPattern.findAllIn(table.group(0)).matchData.map(_.subgroups)
        url = links.head match {
          case s if s.startsWith("//") => "https:" + s
          case s => s
        }
      } yield url).toList
    }

    case class ImageData(url: String, pageUrl: String, characters: Set[String], copyrights: Set[String],
      artists: Set[String], image: Option[Array[Byte]] = None)

    def readBooruPage(pageUrl: String): ImageData = {
      (for {
        booruService <- BooruService.list
        imageData = if (booruService.filterUrl(pageUrl)) {
          val response = http(pageUrl, proxy = true).asString
          if (response.code == 200) {
            booruService.parseHtml(response.body) match {
              case Some((url, characters, copyrights, artists)) =>
                Some(ImageData(url, pageUrl, characters, copyrights, artists))
              case None => throw new CommandException(s"Not parsed: $pageUrl.")
            }
          } else {
            val code = response.code
            throw new Exception(s"Invalid response: $code.")
          }
        } else {
          None
        }
        if imageData.nonEmpty
      } yield imageData.get) match {
        case List(result) => result
        case _ => throw new CommandException("Unknown service.")
      }
    }

    def readBooruPages(pageUrls: List[String]): ImageData = {
      case class Result(success: Boolean, imageData: Option[ImageData], exception: Option[Exception])

      val result = pageUrls.foldLeft(Result(false, None, None)) { (result, pageUrl) =>
        if (!result.success) {
          try {
            Result(true, Some(readBooruPage(pageUrl)), null)
          } catch {
            case e: Exception => Result(false, None, result.exception orElse Some(e))
          }
        } else {
          result
        }
      }

      result match {
        case Result(true, Some(imageData), _) => imageData
        case Result(false, _, Some(exception)) => throw exception
        case _ => throw new CommandException("No images found.")
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

    val handleError: PartialFunction[Throwable, Future[Message]] = {
      case e: CommandException =>
        replyQuote(e.getMessage)
      case e: Exception =>
        e.printStackTrace()
        replyQuote("An exception was thrown during image request.")
    }

    Future(obtainMessageFileId).flatMap(readTelegramFile).map((sendIqdbRequest(70) _).tupled)
      .map(readBooruPages).map(readBooruImage).flatMap(replyWithImage).recoverWith(handleError)
  }
}
