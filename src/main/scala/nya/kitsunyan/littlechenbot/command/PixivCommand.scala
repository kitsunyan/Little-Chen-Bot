package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait PixivCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("pixiv")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FIND_IMAGE_WITH_SAUCENAO_FD) :: list, locale)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def sendSaucenaoRequest(typedFile: TypedFile): Future[List[Long]] = {
      http("https://saucenao.com/search.php")
        .file(typedFile.multipart("file"))
        .runString(HttpFilters.ok)
        .map(response => "<strong>Pixiv ID: </strong><a .*?>(\\d+)</a>".r
          .findAllMatchIn(response.body)
          .flatMap(_.subgroups.headOption)
          .map(_.toLong)
          .toList)
    }

    case class PixivResult(imageUrlNoExtension: String, previewUrlNoExtension: String, pageUrl: String)

    def readPixivResult(pixivId: Long): Future[List[PixivResult]] = {
      val mediumPageUrl = s"https://www.pixiv.net/member_illust.php?mode=medium&illust_id=$pixivId"
      val mediumResponse = http(mediumPageUrl).runString(HttpFilters.ok).map(_.body)

      case class Response(iterator: Iterator[List[String]], pageUrl: String)

      val responseFuture = mediumResponse.flatMap { mediumResponse =>
        val imagePattern = "/img/(\\d+/\\d+/\\d+/\\d+/\\d+/\\d+)/(\\d+)_p(\\d+)(\\w*?)\\.\\w+\"".r
        val multipleImages = "pixiv.tracking.URL = \".*?\\\\u0026multi=true\";".r.findFirstIn(mediumResponse).nonEmpty

        if (multipleImages) {
          val mangaPageUrl = s"https://www.pixiv.net/member_illust.php?mode=manga&illust_id=$pixivId"
          val mangaResponse = http(mangaPageUrl).runString(HttpFilters.ok).map(_.body)

          mangaResponse.map(r => Response(imagePattern.findAllMatchIn(r).map(_.subgroups), mangaPageUrl))
        } else {
          Future.successful(Response(imagePattern.findAllMatchIn(mediumResponse).map(_.subgroups), mediumPageUrl))
        }
      }

      responseFuture.map { response =>
        case class Result(part: String, pixivId: Long, index: Int, suffix: String)

        val results = response.iterator.map {
          case part :: id :: index :: suffix :: Nil => Result(part, id.toLong, index.toInt, suffix)
          case e => throw new MatchError(e)
        }.filter(_.pixivId == pixivId).toList.distinct

        results.map { result =>
          PixivResult(s"https://i.pximg.net/img-original/img/${result.part}/${pixivId}_p${result.index}.",
            s"https://i.pximg.net/c/150x150/img-master/img/${result.part}/${pixivId}_p${result.index}${result.suffix}.",
            response.pageUrl)
        }
      }
    }

    def readPixivResults(pixivIds: List[Long]): Future[List[PixivResult]] = {
      val resultsFuture = pixivIds.foldLeft[Future[List[PixivResult]]](Future.successful(Nil))((f, id) => f
        .flatMap(results => readPixivResult(id).map(results ::: _)))

      resultsFuture.map {
        case Nil => throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")
        case list => list.take(20)
      }
    }

    def storeResponseToWorkspace(pixivResults: List[PixivResult]): Future[(String, List[PixivResult])] = {
      import org.json4s.JsonDSL._
      import org.json4s.jackson.JsonMethods._

      workspace.map { workspace =>
        val json = compact(render(pixivResults.map(r => ("urlNoExtension" -> r.imageUrlNoExtension) ~
          ("pageUrl" -> r.pageUrl))))
        request(SendMessage(Left(workspace), json))
          .map(_.messageId)
          .map(WorkspaceRequest(commands.head))
          .map((_, pixivResults))
      }.getOrElse(Future.failed(new CommandException(locale.SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT)))
    }

    case class PixivImage(image: Array[Byte], url: String, mimeType: String)

    def readPixivImage(urlNoExtension: String): Future[PixivImage] = {
      def readWithExtension(extensions: List[(String, String)], exception: Option[Throwable]): Future[PixivImage] = {
        if (extensions.nonEmpty) {
          val (extension, mimeType) = extensions.head
          val url = s"$urlNoExtension$extension"

          http(url)
            .header("Referer", "https://www.pixiv.net")
            .runBytes(HttpFilters.ok && HttpFilters.contentLength(10 * 1024 * 1024))
            .map(_.body)
            .map(PixivImage(_, url, mimeType))
            .recoverWith { case e =>
            readWithExtension(extensions.tail, Some(e))
          }
        } else {
          Future.failed(exception.getOrElse(new Exception("No extensions provided")))
        }
      }

      readWithExtension(List("png" -> "image/png", "jpg" -> "image/jpeg",
        "jpeg" -> "image/jpeg", "gif" -> "image/gid"), None)
    }

    def replyWithPreview(requestIdString: String, pixivResults: List[PixivResult]): Future[Message] = {
      case class IndexedImage(index: Int, pixivResult: PixivResult)

      val (_, indexedImages) = pixivResults
        .foldRight[(Int, List[IndexedImage])](pixivResults.length, Nil) { case (image, (index, result)) =>
        (index - 1, IndexedImage(index, image) :: result)
      }

      val messageText = s"${locale.HERE_ARE_THE_IMAGES_I_FOUND_FS} $requestIdString.\n" +
        s"${locale.NOW_REPLY_ME_WITH_FORMAT.format(s"/${commands.head} -i N")}"

      indexedImages.map { indexedImage =>
        readPixivImage(indexedImage.pixivResult.previewUrlNoExtension)
          .map(i => Utils.Preview(indexedImage.index, Some(i.image), i.mimeType, Utils.BlurMode.No))
          .recover { case e =>
          handleException(e, None)
          Utils.Preview(indexedImage.index, None, "", Utils.BlurMode.No)
        }
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

    def extractUrlsListFromWorkspace: Future[List[(String, String)]] = {
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
                        case JArray(list) => list.map(s => (s \ "urlNoExtension", s \ "pageUrl")).map {
                          case (JString(s1), JString(s2)) => (s1, s2)
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

    def fetchImageByIndex(index: Int)(list: List[(String, String)]): Future[ImageData] = {
      list.lift(index - 1).map { case (urlNoExtension, pageUrl) =>
        readPixivImage(urlNoExtension)
          .map(i => ImageData(pageUrl, Utils.extractNameFromUrl(i.url), i.image))
      }.getOrElse(Future.failed(new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")))
    }

    def replyWithImage(imageData: ImageData): Future[Message] = {
      request(SendDocument(Left(message.source), Left(InputFile(imageData.name, imageData.image)),
        replyToMessageId = Some(message.messageId), caption = Some(imageData.url)))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.FETCH_IMAGE_FROM_PIXIV_USING_SAUCENAO,
          (List("-i", "--index"), Some("integer"),
            locale.FETCH_IMAGE_BY_INDEX) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      val indexOption = arguments("i", "index").asInt

      indexOption.map { index =>
        checkArguments(arguments, "i", "index", "d", "as-document")
          .unitFlatMap(extractUrlsListFromWorkspace)
          .flatMap(fetchImageByIndex(index))
          .flatMap(replyWithImage)
          .recoverWith(handleError(Some(locale.IMAGE_REQUEST_FV_FS))(message))
      } getOrElse {
        checkArguments(arguments)
          .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
          .scopeFlatMap((_, file) => readTelegramFile(file)
            .flatMap(sendSaucenaoRequest)
            .flatMap(readPixivResults)
            .flatMap(storeResponseToWorkspace)
            .flatMap((replyWithPreview _).tupled))
          .recoverWith[Any](message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
      }
    }
  }
}
