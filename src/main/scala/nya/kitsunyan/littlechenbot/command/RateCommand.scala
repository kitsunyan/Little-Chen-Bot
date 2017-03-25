package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods.SendSticker
import info.mukel.telegrambot4s.models.Message

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.concurrent.Future

trait RateCommand extends Command with ExtractImage with Http {
  private val commands = List("rate", "r8")

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

  override def handleMessage(implicit message: Message): Future[_] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[_] = {
    def sendEverypixelRequest(telegramFile: TelegramFile): Float = {
      http("https://services2.microstock.pro/aesthetics/quality")
        .postMulti(telegramFile.multiPart("data")).response(_.asString) { _ => body =>
        parse(body) \ "quality" \ "score" match {
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
      Future(obtainMessageFileId(commands.head, messageWithImage)).flatMap(readTelegramFile)
        .map(sendEverypixelRequest).flatMap(replyWithRating)
        .recoverWith(handleError(messageWithImageAsCausal, "rating request"))
    }
  }
}
