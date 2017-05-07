package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait ExtractImage {
  this: Command with Http =>

  def extractMessageWithImage(implicit message: Message): Option[Message] = {
    message.caption.map(_ => message) orElse message.replyToMessage
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

  def obtainMessageFile(command: String)(messageWithImage: Option[Message])
    (implicit locale: Locale): (Message, String) = {
    messageWithImage.flatMap(m => extractFile(m).map((m, _))).getOrElse {
      throw new CommandException(locale.PLEASE_REPLY_TO_MESSAGE_WITH_IMAGE_FORMAT.format(s"`/$command --help`"),
        Some(ParseMode.Markdown))
    }
  }

  case class TypedFile(data: Array[Byte], mimeType: String) {
    def multipart(name: String): MultipartFile = MultipartFile(name, "filename", mimeType, data)
  }

  private def readExternalFile(file: String, readMeta: Boolean)
    (implicit locale: Locale): Either[TypedFile, String] = {
    http(file, proxy = true)
      .runBytes(HttpFilters.ok && HttpFilters.contentLength(10 * 1024 * 1024)) { response =>
      val contentType = response.headers("Content-Type").headOption.map { contentType =>
        val index = contentType.indexOf(';')
        if (index >= 0) contentType.substring(0, index) else contentType
      }

      def unable: Nothing = throw new CommandException(locale.UNABLE_TO_FETCH_THE_FILE_BY_URL)

      contentType match {
        case Some(mimeType) if mimeType.startsWith("image/") => Left(TypedFile(response.body, mimeType))
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

  def readTelegramFile(file: String)(implicit locale: Locale): Future[TypedFile] = {
    if (file.startsWith("http://") || file.startsWith("https://")) {
      Future(readExternalFile(file, true)).map {
        case Left(typedFile) => typedFile
        case Right(url) => readExternalFile(url, false).left.getOrElse(throw new Exception("Impossible value."))
      }
    } else {
      request(GetFile(file)).map { file =>
        file.filePath match {
          case Some(path) =>
            val data = {
              val telegramImageUrl = s"https://api.telegram.org/file/bot$token/$path"
              val data = http(telegramImageUrl).withPrivateUrl(true)
                .runBytes(HttpFilters.ok && HttpFilters.contentLength(10 * 1024 * 1024))(_.body)
              (if (path.endsWith(".webp")) Utils.webpToPng(data) else None).getOrElse(data)
            }
            val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
            TypedFile(data, mimeType)
          case None => throw new CommandException(locale.UNABLE_TO_FETCH_TELEGRAM_FILE)
        }
      }
    }
  }
}
