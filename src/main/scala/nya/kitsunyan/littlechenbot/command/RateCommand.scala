package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait RateCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("rate", "r8")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.RATE_IMAGE_FD) :: list, locale)
  }

  private val ratings =
    "CAADAgADtQEAAiX3NwilKgTAwYCiHAI" ::
    "CAADAgADtgEAAiX3NwhWCJAp-ZtVRwI" ::
    "CAADAgADtwEAAiX3NwipbSEbFipOkAI" ::
    "CAADAgADuAEAAiX3NwgqcIN3I7FpOQI" ::
    "CAADAgADuQEAAiX3NwjzQqTuBYEE0gI" ::
    "CAADAgADugEAAiX3NwgkU08Rfqne_AI" ::
    "CAADAgADuwEAAiX3NwinrL2Hzb3eswI" ::
    "CAADAgADvAEAAiX3NwjfcYlO4i9glwI" ::
    "CAADAgADvQEAAiX3NwjWF5kVFi1kIQI" ::
    "CAADAgADvgEAAiX3NwjxCFfNfFfOmgI" ::
    "CAADAgADxgEAAiX3NwgpkLZRMe1h0wI" ::
    Nil

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def obtainEverypixelToken: Future[String] = {
      val url = "https://everypixel.com/aesthetics"

      http(url)
        .runString(HttpFilters.ok)
        .map(response => "<input .*?id=\"access_token\".*?value=\"(.*?)\".*?/>".r
          .findFirstMatchIn(response.body)
          .flatMap(_.subgroups.headOption)
          .getOrElse(throw new CommandException(s"${locale.NOT_PARSED_FS}: $url.")))
    }

    def obtainEverypixelTags(typedFile: TypedFile, token: String): Future[List[String]] = {
      http("https://keywording.api.everypixel.com/v1/keywords")
        .header("Authorization", s"Bearer $token")
        .file(typedFile.multipart("data")).runString(HttpFilters.ok).map { response =>
        import org.json4s._
        import org.json4s.jackson.JsonMethods._

        parse(response.body) \ "keywords" match {
          case JArray(array) =>
            array.map(_ \ "keyword" match {
              case JString(s) => s
              case _ => throw new CommandException(locale.INVALID_SERVER_RESPONSE)
            })
          case _ => throw new CommandException(locale.INVALID_SERVER_RESPONSE)
        }
      }
    }

    case class EverypixelData(quality: Float, tags: List[String])

    def sendEverypixelRequest(typedFile: TypedFile, token: String): Future[EverypixelData] = {
      val qualityFuture = obtainEverypixelQuality(typedFile, token)
      val tagsFuture = obtainEverypixelTags(typedFile, token)

      for {
        quality <- qualityFuture
        tags <- tagsFuture
      } yield EverypixelData(quality, tags)
    }

    def obtainEverypixelQuality(typedFile: TypedFile, token: String): Future[Float] = {
      http("https://quality.api.everypixel.com/v1/quality")
        .header("Authorization", s"Bearer $token")
        .file(typedFile.multipart("data")).runString(HttpFilters.ok).map { response =>
        import org.json4s._
        import org.json4s.jackson.JsonMethods._

        parse(response.body) \ "quality" \ "score" match {
          case JDouble(rating) => rating.toFloat
          case _ => throw new CommandException(locale.INVALID_SERVER_RESPONSE)
        }
      }
    }

    def replyWithRating(everypixelData: EverypixelData): Future[Message] = {
      val quality = {
        val quality = (everypixelData.quality * 10).toInt
        if (quality > 10) 10 else if (quality < 0) 0 else quality
      }

      val messageFuture = request(SendSticker(Left(message.source), Right(ratings(quality)),
        replyToMessageId = Some(message.replyToMessage.getOrElse(message).messageId)))

      if (everypixelData.tags.nonEmpty) {
        messageFuture.flatMap { message =>
          val tags = everypixelData.tags.reduce(_ + ", " + _).toLowerCase(java.util.Locale.US)
          val text = s"${locale.TAGS_FS}: $tags"
          request(SendMessage(Left(message.source), text, replyToMessageId = Some(message.messageId)))
        }
      } else {
        messageFuture
      }
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.RATE_IMAGE_USING_EVERYPIXEL_COM,
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments)
        .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
        .scopeFlatMap((_, file) => readTelegramFile(file)
          .zip(obtainEverypixelToken)
          .flatMap((sendEverypixelRequest _).tupled)
          .flatMap(replyWithRating))
        .recoverWith[Any](message)(handleError(Some(locale.RATING_REQUEST_FV_FS)))
    }
  }
}
