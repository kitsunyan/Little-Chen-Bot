package nya.kitsunyan.littlechenbot.command

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

  def obtainMessageFileId(command: String, messageWithImage: Option[Message]): String = {
    messageWithImage.flatMap(extractFileId) match {
      case Some(fileId) => fileId
      case None =>
        throw new CommandException("Please reply to message with image or send image with command in caption.\n\n" +
          "Remember I can't see other bots' messages even when you reply them!\n\n" +
          s"Type `/$command --help` for more information.", Some(ParseMode.Markdown))
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
            (if (path.endsWith(".webp")) Utils.webpToPng(data) else None).getOrElse(data)
          }
          val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
          TelegramFile(data, mimeType)
        case None => throw new CommandException("Unable to fetch Telegram file.")
      }
    }
  }

  def handleError(causalMessage: Message, kind: String)(implicit message: Message):
    PartialFunction[Throwable, Future[Any]] = {
    case e: RecoverException =>
      e.future
    case e: CommandException =>
      replyQuote(e.getMessage, e.parseMode)
    case e: Exception =>
      handleException(e, causalMessage)
      replyQuote(s"An exception was thrown during $kind.")
  }
}
