package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._

import org.json4s._
import org.json4s.jackson.JsonMethods._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait RateCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("rate", "r8")

  override def prependDescription(list: List[Description]): List[Description] = {
    super.prependDescription(Description(commands, "rate image") :: list)
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

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    def sendEverypixelRequest(telegramFile: TelegramFile): Float = {
      http("https://services2.microstock.pro/aesthetics/quality")
        .postMulti(telegramFile.multiPart("data")).runString(HttpFilters.ok) { response =>
        parse(response.body) \ "quality" \ "score" match {
          case JDouble(rating) => rating.toFloat
          case _ => throw new CommandException("Invalid server response.")
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

    if (arguments.string("h", "help").nonEmpty) {
      replyMan("Rate image using everypixel.com.",
        (List("-h", "--help"), None,
          "Display this help.") ::
        Nil)
    } else {
      Future(obtainMessageFile(commands.head)(extractMessageWithImage))
        .scopeFlatMap((_, file) => readTelegramFile(file)
          .map(sendEverypixelRequest)
          .flatMap(replyWithRating))
        .recoverWith[Any](message)(handleError("rating request"))
    }
  }
}
