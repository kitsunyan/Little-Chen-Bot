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

  case class ServiceFile(id: String, videoFormat: Option[String])

  type MessageFile = Either[ServiceFile, String]

  def extractFile(message: Message): Option[MessageFile] = {
    message.photo.map { photos =>
      val id = photos.reduceLeft { (photo1, photo2) =>
        // Find the largest image
        if (photo2.fileSize.getOrElse(0) >= photo1.fileSize.getOrElse(0)) photo2 else photo1
      }.fileId
      Left(ServiceFile(id, None))
    } orElse message.sticker.map { sticker =>
      Left(ServiceFile(sticker.fileId, None))
    } orElse message.document.flatMap { document =>
      document.mimeType match {
        case Some(mimeType) if mimeType.startsWith("image/") => Some(Left(ServiceFile(document.fileId, None)))
        case Some(mimeType) if mimeType == "video/mp4" => Some(Left(ServiceFile(document.fileId, Some("mp4"))))
        case _ => None
      }
    } orElse {
      for {
        entities <- message.entities
        entity <- entities.find(_.`type` == MessageEntityType.Url)
        text <- message.text
        if text.length >= entity.offset + entity.length
        url = text.substring(entity.offset, entity.offset + entity.length)
        if url.startsWith("http://") || url.startsWith("https://")
      } yield Right(url)
    }
  }

  def obtainMessageFile(command: String)(messageWithImage: Option[Message])
    (implicit locale: Locale): (Message, MessageFile) = {
    messageWithImage.flatMap(m => extractFile(m).map((m, _))).getOrElse {
      throw new CommandException(locale.PLEASE_REPLY_TO_MESSAGE_WITH_IMAGE_FORMAT.format(s"`/$command --help`"),
        Some(ParseMode.Markdown))
    }
  }

  private def formatFileName(name: String, mimeType: Option[String]): String = {
    val extension = mimeType.flatMap(Utils.mimeTypeMap.lift).getOrElse("jpeg")
    s"$name.$extension"
  }

  case class TypedFile(data: Array[Byte], mimeType: String, name: String) {
    def multipart(name: String): Http.MultipartFile = Http.MultipartFile(name, "filename", mimeType, data)
  }

  private def readExternalFile(url: String, readMeta: Boolean)
    (implicit locale: Locale): Future[Either[TypedFile, String]] = {
    http(url, proxy = true)
      .runBytes(Http.Filters.ok && Http.Filters.contentLength(10 * 1024 * 1024)).map { response =>
      val contentType = response.headers("Content-Type").headOption.map { contentType =>
        val index = contentType.indexOf(';')
        if (index >= 0) contentType.substring(0, index) else contentType
      }

      def unable: Nothing = throw new CommandException(locale.UNABLE_TO_FETCH_THE_FILE_BY_URL)

      contentType match {
        case Some(mimeType) if mimeType.startsWith("image/") =>
          Left(TypedFile(response.body, mimeType, Utils.extractNameFromUrl(url, Some(mimeType))))
        case Some("text/html") if readMeta =>
          val responseString = new String(response.body, "ISO-8859-1")
          "<meta\\s+property=\"og:image\"\\s+content=\"(.*?)\".*?>".r
            .findFirstMatchIn(responseString)
            .flatMap(_.subgroups.headOption)
            .filter(url => url.startsWith("http://") || url.startsWith("https://"))
            .map(Right.apply)
            .getOrElse(unable)
        case _ => unable
      }
    }
  }

  def readTelegramFile(file: MessageFile)(implicit locale: Locale): Future[TypedFile] = {
    file match {
      case Left(ServiceFile(id, videoFormat)) =>
        request(GetFile(id)).flatMap { file =>
          file.filePath match {
            case Some(path) =>
              val telegramImageUrl = s"https://api.telegram.org/file/bot$token/$path"

              http(telegramImageUrl).withPrivateUrl(true)
                .runBytes(Http.Filters.ok && Http.Filters.contentLength(10 * 1024 * 1024)).map { response =>
                val (data, mimeType) = if (path.endsWith(".webp")) {
                  (Utils.webpToPng(response.body), "image/png")
                } else if (videoFormat.nonEmpty) {
                  (Utils.extractPreviewPng(response.body, videoFormat.get), "image/png")
                } else {
                  val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) {
                    "image/jpeg"
                  } else {
                    "image/png"
                  }
                  (response.body, mimeType)
                }

                TypedFile(data, mimeType, formatFileName(file.fileId, Some(mimeType)))
              }
            case None => Future.failed(new CommandException(locale.UNABLE_TO_FETCH_TELEGRAM_FILE))
          }
        }
      case Right(url) =>
        readExternalFile(url, true).flatMap {
          case Left(typedFile) =>
            Future.successful(typedFile)
          case Right(url) =>
            readExternalFile(url, false).map(_.left.getOrElse(throw new Exception("Impossible value.")))
        }
    }
  }
}
