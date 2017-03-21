package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods.GetFile
import info.mukel.telegrambot4s.models.Message
import nya.kitsunyan.littlechenbot.Utils

import scala.concurrent.Future
import scalaj.http.MultiPart

trait ExtractImage extends Command with Http {
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

  def obtainMessageFileId(implicit message: Message): String = {
    extractMessageWithImage(message).flatMap(extractFileId) match {
      case Some(fileId) => fileId
      case None =>
        throw new CommandException("Please reply to message with image or send image with command in caption.\n" +
          "Remember I can't see other bots' messages even when you reply them!")
    }
  }

  case class TelegramFile(data: Array[Byte], mimeType: String) {
    def multiPart(name: String): MultiPart = MultiPart(name, "filename", mimeType, data)
  }

  def readTelegramFile(fileId: String): Future[TelegramFile] = {
    request(GetFile(fileId)).map { file =>
      file.filePath match {
        case Some(path) =>
          val data = {
            val telegramImageUrl = s"https://api.telegram.org/file/bot$token/$path"
            val data = http(telegramImageUrl).asBytes.body
            if (path.endsWith(".webp")) Utils.webpToPng(data) else data
          }
          val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
          TelegramFile(data, mimeType)
        case None => throw new CommandException("Unable to fetch Telegram file.")
      }
    }
  }

  def handleError(kind: String)(implicit message: Message): PartialFunction[Throwable, Future[Message]] = {
    case e: CommandException =>
      replyQuote(e.getMessage)
    case e: Exception =>
      e.printStackTrace()
      replyQuote(s"An exception was thrown during $kind.")
  }
}
