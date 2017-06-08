package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait GoogleCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("google")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FIND_IMAGE_WITH_GOOGLE_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    def sendGoogleRequest(typedFile: TypedFile): Future[String] = {
      http("https://images.google.com/searchbyimage/upload")
        .file(typedFile.multipart("encoded_image"))
        .field("hl" -> "en")
        .runString(HttpFilters.ok)
        .map(_.body)
    }

    case class Image(url: String, previewUrl: String, width: Option[Int], height: Option[Int])

    def parseImages(response: String): List[Image] = {
      val result = "<a href=\"(/imgres?.*?)\".*?<img src=\"(https://encrypted-.*?.gstatic.com/.*?)\"".r
        .findAllMatchIn(response).map(_.subgroups).flatMap {
        case urlPart :: previewUrl :: Nil =>
          val parameters = Utils.getUrlParameters(new java.net.URL("https://www.google.com"
            + Utils.unescapeHtml(urlPart)))
          for {
            url <- parameters.get("imgurl").flatten
            width = parameters.get("w").flatten.map(_.toInt)
            height = parameters.get("h").flatten.map(_.toInt)
          } yield Image(url, previewUrl, width, height)
        case _ => None
      }.toList.distinct

      if (result.nonEmpty) {
        def size(image: Image): Int = {
          (for {
            width <- image.width
            height <- image.height
          } yield width * height).getOrElse(0)
        }

        result.sortWith(size(_) > size(_))
      } else {
        throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")
      }
    }

    def storeResponseToWorkspace(images: List[Image]): Future[(String, List[Image])] = {
      import org.json4s.JsonDSL._
      import org.json4s.jackson.JsonMethods._

      workspace.map { workspace =>
        val json = compact(render(images.map("url" -> _.url)))
        request(SendMessage(Left(workspace), json))
          .map(_.messageId)
          .map(WorkspaceRequest(commands.head))
          .map((_, images))
      }.getOrElse(Future.failed(new CommandException(locale.SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT)))
    }

    def replyWithPreview(requestIdString: String, images: List[Image]): Future[Message] = {
      case class IndexedImage(index: Int, image: Image)

      val (_, indexedImages) = images
        .foldRight[(Int, List[IndexedImage])](images.length, Nil) { case (image, (index, result)) =>
        (index - 1, IndexedImage(index, image) :: result)
      }

      val introduction = s"${locale.HERE_ARE_THE_IMAGES_I_FOUND_FS} $requestIdString.\n" +
        s"${locale.NOW_REPLY_ME_WITH_FORMAT.format(s"/${commands.head} -i N")}\n"
      val messageText = indexedImages.foldLeft(introduction) { (result, indexedImage) =>
        val size = for {
          width <- indexedImage.image.width
          height <- indexedImage.image.height
        } yield s"$width × $height"

        val host = new java.net.URL(indexedImage.image.url).getHost
        s"$result\n${indexedImage.index}: ${size.getOrElse("Unknown size")} — $host"
      }

      indexedImages.map { indexedImage =>
        http(indexedImage.image.previewUrl).runBytes(HttpFilters.ok).map { response =>
          val mimeType = response.headers("Content-Type").headOption.getOrElse("image/jpeg")
          Utils.Preview(indexedImage.index, Some(response.body), mimeType, Utils.BlurMode.No)
        }.recover((handleException(None)(_)) -> Utils.Preview(indexedImage.index, None, "", Utils.BlurMode.No))
      }.foldRight[Future[List[Utils.Preview]]](Future.successful(Nil)) { (future, result) =>
        result.flatMap(list => future.map(_ :: list))
      }.flatMap { list =>
        val preview = Utils.drawPreview(list)
        preview.map { preview =>
          request(SendPhoto(Left(message.source), Left(InputFile("preview.png", preview)),
            replyToMessageId = Some(message.messageId), caption = Some(trimCaption(messageText))))
        }.getOrElse(replyQuote(messageText))
      }
    }

    def extractUrlsListFromWorkspace: Future[List[String]] = {
      workspace.map { workspace =>
        bot.flatMap { bot =>
          message.replyToMessage.flatMap { message =>
            if ((message.forwardFrom orElse message.from).map(_.id.toLong).contains(bot.id)) {
              (message.text orElse message.caption)
                .flatMap(WorkspaceRequest.parse(commands.head))
                .map { messageId =>
                request(SendMessage(Left(workspace), "query", replyToMessageId = Some(messageId)))
                  .map(_.replyToMessage.flatMap(_.text).map { text =>
                  import org.json4s._
                  import org.json4s.jackson.JsonMethods._

                  parse(text) match {
                    case JArray(list) => list.map(_ \ "url").map {
                      case JString(s) => s
                      case _ => throw new Exception
                    }
                    case _ => throw new Exception
                  }
                }.get)
              }
            } else {
              None
            }
          }.getOrElse(throw new CommandException(locale.ARE_YOU_KIDDING_ME))
        }
      }.getOrElse(Future.failed(new CommandException(locale.SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT)))
    }

    case class ImageData(url: String, name: String, image: Array[Byte])

    def fetchImageByIndex(index: Int)(list: List[String]): Future[ImageData] = {
      list.lift(index - 1).map { url =>
        val refererUrl = {
          val index = url.indexOf("//")
          val nextIndex = url.indexOf('/', index + 2)
          if (nextIndex >= 0) url.substring(0, nextIndex) else url
        }
        http(url, proxy = true).header("Referer", refererUrl)
          .runBytes(HttpFilters.ok && HttpFilters.contentLength(10 * 1024 * 1024))
          .map(r => ImageData(url, Utils.extractNameFromUrl(url), r.body))
      }.getOrElse(Future.failed(new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")))
    }

    def replyWithImage(asDocument: Boolean)(imageData: ImageData): Future[Message] = {
      if (asDocument) {
        request(SendDocument(Left(message.source), Left(InputFile(imageData.name, imageData.image)),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      } else {
        request(SendPhoto(Left(message.source), Left(InputFile(imageData.name, imageData.image)),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      }
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.SEARCH_IMAGE_USING_IMAGES_GOOGLE_COM,
          (List("-i", "--index"), Some("integer"),
            locale.FETCH_IMAGE_BY_INDEX) ::
          (List("-d", "--as-document"), None,
            locale.FETCH_IMAGE_AS_DOCUMENT) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val indexOption = arguments("i", "index").asInt
      val asDocument = arguments("d", "as-document").nonEmpty

      indexOption.map { index =>
        checkArguments(arguments, "i", "index", "d", "as-document")
          .unitFlatMap(extractUrlsListFromWorkspace)
          .flatMap(fetchImageByIndex(index))
          .flatMap(replyWithImage(asDocument))
          .statusMap(Status.Success)
          .recoverWith(handleError(Some(locale.IMAGE_REQUEST_FV_FS))(message))
      }.getOrElse {
        checkArguments(arguments)
          .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
          .scopeFlatMap((_, file) => readTelegramFile(file)
            .flatMap(sendGoogleRequest)
            .map(parseImages)
            .flatMap(storeResponseToWorkspace)
            .flatMap((replyWithPreview _).tupled)
            .statusMap(Status.Success))
          .recoverWith(message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
      }
    }
  }
}
