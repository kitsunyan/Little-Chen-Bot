package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.service.TranslateService
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait BakaCommand extends Command {
  this: Http =>

  private val commands = List("baka")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.BAKA_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, 0, "h", "help").unitFlatMap {
        replyMan(locale.CHAOMIAN_BAKA,
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val targetMessage = message.replyToMessage
        .getOrElse(message)

      val name = targetMessage.from
        .map(from => from.lastName
          .map(lastName => s"${from.firstName} $lastName")
          .getOrElse(from.firstName))

      checkArguments(arguments, 0)
        .unitFlatMap(name
          .map(TranslateService.transliterate)
          .map(_.map(name => s"$name baka"))
          .getOrElse(Future.successful("baka")))
        .flatMap(TranslateService.textToSpeech(_, "ja"))
        .flatMap(audio => request(SendVoice(targetMessage.chat.id,
          InputFile(s"${System.currentTimeMillis}.opus", audio),
          replyToMessageId = Some(targetMessage.messageId))))
        .statusMap(Status.Success)
        .recoverWith {
          case _: TranslateService.UnsupportedLanguageException =>
            Future.failed(new CommandException(locale.INVALID_LANGUAGE))
        }
        .recoverWith(handleError(Some(locale.TRANSLATION_REQUEST_FV_FS))(message))
    }
  }
}
