package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.service.ReverseService
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait ReverseCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("reverse")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FIND_IMAGE_WITH_REVERSE_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    def handleImages(images: List[ReverseService.Image]): List[ReverseService.Image] = {
      if (images.nonEmpty) {
        def size(image: ReverseService.Image): Int = {
          (for {
            width <- image.width
            height <- image.height
          } yield width * height).getOrElse(0)
        }

        images.distinct.sortWith(size(_) > size(_))
      } else {
        throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")
      }
    }

    def storeResponseToWorkspace(images: List[ReverseService.Image]): Future[(String, List[ReverseService.Image])] = {
      import org.json4s.JsonDSL._
      import org.json4s.jackson.JsonMethods._

      workspace.map { workspace =>
        val json = compact(render(images.map("url" -> _.url)))
        request(SendMessage(workspace, json))
          .map(_.messageId)
          .map(WorkspaceRequest(commands.head))
          .map((_, images))
      }.getOrElse(Future.failed(new CommandException(locale.SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT)))
    }

    def replyWithPreview(requestIdString: String, images: List[ReverseService.Image]): Future[Message] = {
      case class IndexedImage(index: Int, image: ReverseService.Image)

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
        http(indexedImage.image.previewUrl).runBytes(Http.Filters.ok).map { response =>
          val mimeType = response.headers("Content-Type").headOption.getOrElse("image/jpeg")
          Utils.Preview(indexedImage.index, Some(response.body), mimeType, Utils.BlurMode.No)
        }.recover((handleException(None)(_)) -> Utils.Preview(indexedImage.index, None, "", Utils.BlurMode.No))
      }.foldRight[Future[List[Utils.Preview]]](Future.successful(Nil)) { (future, result) =>
        result.flatMap(list => future.map(_ :: list))
      }.flatMap { list =>
        val preview = Utils.drawPreview(list)
        preview.map { preview =>
          request(SendPhoto(message.source, InputFile("preview.png", preview),
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
                request(SendMessage(workspace, "query", replyToMessageId = Some(messageId)))
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

    def fetchImageByIndex(index: Int, list: List[String]): Future[ImageData] = {
      list.lift(index - 1).map { url =>
        val refererUrl = {
          val index = url.indexOf("//")
          val nextIndex = url.indexOf('/', index + 2)
          if (nextIndex >= 0) url.substring(0, nextIndex) else url
        }
        http(url, proxy = true).header("Referer", refererUrl)
          .runBytes(Http.Filters.ok && Http.Filters.contentLength(10 * 1024 * 1024))
          .map(r => ImageData(url, Utils.extractNameFromUrl(url, None), r.body))
      }.getOrElse(Future.failed(new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")))
    }

    def replyWithImage(asDocument: Boolean)(imageData: ImageData): Future[Message] = {
      if (asDocument) {
        request(SendDocument(message.source, InputFile(imageData.name, imageData.image),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      } else {
        request(SendPhoto(message.source, InputFile(imageData.name, imageData.image),
          replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
      }
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.SEARCH_IMAGE_USING_REVERSE_SEARCH_ENGINES,
          (List("-i", "--indices"), Some("integer list"),
            locale.FETCH_IMAGE_BY_INDEX) ::
          (List("-d", "--as-document"), None,
            locale.FETCH_IMAGE_AS_DOCUMENT) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val indicesOption = arguments("i", "indices").asIntList
      val asDocument = arguments("d", "as-document").nonEmpty

      indicesOption.map { indices =>
        checkArguments(arguments, "i", "indices", "d", "as-document")
          .unitFlatMap(extractUrlsListFromWorkspace)
          .flatMap(list => indices
            .foldLeft(Future.unit)((f, i) => f
              .unitFlatMap(fetchImageByIndex(i, list))
              .flatMap(replyWithImage(asDocument))
              .map(_ => ())))
          .statusMap(Status.Success)
          .recoverWith(handleError(Some(locale.IMAGE_REQUEST_FV_FS))(message))
      }.getOrElse {
        implicit val http: Http = this

        checkArguments(arguments)
          .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
          .scopeFlatMap((_, file) => readTelegramFile(file)
            .flatMap(file => ReverseService.list.foldLeft(Future.successful[List[ReverseService.Image]](Nil))
              ((future, service) => future.flatMap(images => service.getImages(file.multipart).map(images ::: _))))
            .map(handleImages)
            .flatMap(storeResponseToWorkspace)
            .flatMap((replyWithPreview _).tupled)
            .statusMap(Status.Success))
          .recoverWith(message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
      }
    }
  }
}
