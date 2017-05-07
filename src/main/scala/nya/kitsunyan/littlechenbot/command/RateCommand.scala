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
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def sendEverypixelRequest(typedFile: TypedFile): Float = {
      http("https://quality.api.everypixel.com/v1/quality")
        .file(typedFile.multipart("data")).runString(HttpFilters.ok) { response =>
        import org.json4s._
        import org.json4s.jackson.JsonMethods._

        parse(response.body) \ "quality" \ "score" match {
          case JDouble(rating) => rating.toFloat
          case _ => throw new CommandException(locale.INVALID_SERVER_RESPONSE)
        }
      }
    }

    def replyWithRating(rating: Float): Future[Message] = {
      val value = {
        val tmp = (rating * 10).toInt
        if (tmp > 10) 10 else if (tmp < 0) 0 else tmp
      }

      request(SendSticker(Left(message.sender), Right(ratings(value)),
        replyToMessageId = Some(message.replyToMessage.getOrElse(message).messageId)))
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
          .map(sendEverypixelRequest)
          .flatMap(replyWithRating))
        .recoverWith[Any](message)(handleError(Some(locale.RATING_REQUEST_FV_FS)))
    }
  }
}
