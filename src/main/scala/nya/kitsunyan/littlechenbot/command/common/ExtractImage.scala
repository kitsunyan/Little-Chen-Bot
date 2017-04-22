package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.Utils

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scalaj.http.MultiPart

import scala.concurrent.Future

trait ExtractImage extends Command with Http {
  def messageWithImage(implicit message: Message): Option[Message] = {
    message.caption.map(_ => message) orElse message.replyToMessage
  }

  def messageWithImageAsCausal(implicit message: Message): Message = {
    messageWithImage.getOrElse(message)
  }

  def extractFile(message: Message): Option[String] = {
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
    } orElse {
      for {
        entity <- message.entities.flatMap(_.find(_.`type` == "url"))
        text <- message.text
        if text.length <= entity.offset + entity.length
        url = text.substring(entity.offset, entity.offset + entity.length)
        if url.startsWith("http://") || url.startsWith("https://")
      } yield url
    }
  }

  def obtainMessageFile(command: String, messageWithImage: Option[Message]): String = {
    messageWithImage.flatMap(extractFile) match {
      case Some(file) => file
      case None =>
        throw new CommandException("Please reply to message with image or send image with command in caption.\n\n" +
          "Remember I can't see other bots' messages even when you reply them!\n\n" +
          s"Type `/$command --help` for more information.", Some(ParseMode.Markdown))
    }
  }

  case class TelegramFile(data: Array[Byte], mimeType: String) {
    def multiPart(name: String): MultiPart = MultiPart(name, "filename", mimeType, data)
  }

  private def readExternalFile(file: String, readMeta: Boolean): Either[TelegramFile, String] = {
    http(file, proxy = true).response(_.asBytes) { response =>
      val contentType = response.headers.get("Content-Type").flatMap(_.headOption).map { contentType =>
        val index = contentType.indexOf(';')
        if (index >= 0) contentType.substring(0, index) else contentType
      }

      def unable: Nothing = throw new CommandException("Unable to fetch the file by URL.")

      contentType match {
        case Some(mimeType) if mimeType.startsWith("image/") => Left(TelegramFile(response.body, mimeType))
        case Some("text/html") if readMeta =>
          val responseString = new String(response.body, "ISO-8859-1")
          "<meta property=\"og:image\" content=\"(.*?)\".*?>".r
            .findFirstMatchIn(responseString)
            .flatMap(_.subgroups.headOption)
            .filter(url => url.startsWith("http://") || url.startsWith("https://"))
            .map(Right.apply)
            .getOrElse(unable)
        case _ => unable
      }
    }
  }

  def readTelegramFile(file: String): Future[TelegramFile] = {
    if (file.startsWith("http://") || file.startsWith("https://")) {
      Future(readExternalFile(file, true)).map {
        case Left(telegramFile) => telegramFile
        case Right(url) => readExternalFile(url, false).left.getOrElse(throw new Exception("Impossible value."))
      }
    } else {
      request(GetFile(file)).map { file =>
        file.filePath match {
          case Some(path) =>
            val data = {
              val telegramImageUrl = s"https://api.telegram.org/file/bot$token/$path"
              val data = http(telegramImageUrl).asBytes.body
              (if (path.endsWith(".webp")) Utils.webpToPng(data) else None).getOrElse(data)
            }
            val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
            TelegramFile(data, mimeType)
          case None => throw new CommandException("Unable to fetch Telegram file.")
        }
      }
    }
  }
}
