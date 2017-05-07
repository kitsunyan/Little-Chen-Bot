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

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def sendGoogleRequest(telegramFile: TelegramFile): String = {
      http("https://images.google.com/searchbyimage/upload")
        .postMulti(telegramFile.multiPart("encoded_image"))
        .params("hl" -> "en").runString(HttpFilters.ok)(_.body)
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
          .map(m => (s"[request ${m.messageId}]", images))
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
        Future {
          http(indexedImage.image.previewUrl).runBytes(HttpFilters.ok) { response =>
            val mimeType = response.headers.get("Content-Type").flatMap(_.headOption).getOrElse("image/jpeg")
            Utils.Preview(indexedImage.index, Some(response.body), mimeType, Utils.BlurMode.No)
          }
        }.recover { case e =>
          handleException(e, None)
          Utils.Preview(indexedImage.index, None, "", Utils.BlurMode.No)
        }
      }.foldRight[Future[List[Utils.Preview]]](Future.successful(Nil)) { (future, result) =>
        result.flatMap(list => future.map(_ :: list))
      }.flatMap { list =>
        val preview = Utils.drawPreview(list)
        preview.map { preview =>
          request(SendPhoto(Left(message.sender), Left(InputFile("preview.png", preview)),
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
                .flatMap("\\[request (-?\\d+)\\]".r.findFirstMatchIn)
                .flatMap(_.subgroups.headOption)
                .map(_.toLong)
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

    def fetchImageByIndex(index: Int)(list: List[String]): ImageData = {
      list.lift(index - 1).map { url =>
        val refererUrl = {
          val index = url.indexOf("//")
          val nextIndex = url.indexOf('/', index + 2)
          if (nextIndex >= 0) url.substring(0, nextIndex) else url
        }
        http(url, proxy = true).header("Referer", refererUrl)
          .runBytes(HttpFilters.ok)(response => ImageData(url, Utils.extractNameFromUrl(url), response.body))
      }.getOrElse(throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}."))
    }

    def replyWithImage(asDocument: Boolean)(imageData: ImageData): Future[Message] = {
      if (asDocument) {
        request(SendDocument(Left(message.sender), Left(InputFile(imageData.name, imageData.image)),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      } else {
        request(SendPhoto(Left(message.sender), Left(InputFile(imageData.name, imageData.image)),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      }
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.SEARCH_IMAGES_USING_IMAGES_GOOGLE_COM,
          (List("-i", "--index"), Some("integer"),
            locale.FETCH_IMAGE_BY_INDEX) ::
          (List("-d", "--as-document"), None,
            locale.FETCH_IMAGE_AS_DOCUMENT) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      val indexOption = arguments("i", "index").asInt
      val asDocument = arguments("d", "as-document").nonEmpty

      indexOption.map { index =>
        checkArguments(arguments, "i", "index", "d", "as-document")
          .unitFlatMap(extractUrlsListFromWorkspace)
          .map(fetchImageByIndex(index))
          .flatMap(replyWithImage(asDocument))
          .recoverWith(handleError(Some(locale.IMAGE_REQUEST_FV_FS))(message))
      } getOrElse {
        checkArguments(arguments)
          .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
          .scopeFlatMap((_, file) => readTelegramFile(file)
            .map(sendGoogleRequest)
            .map(parseImages)
            .flatMap(storeResponseToWorkspace)
            .flatMap((replyWithPreview _).tupled))
          .recoverWith[Any](message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
      }
    }
  }
}
