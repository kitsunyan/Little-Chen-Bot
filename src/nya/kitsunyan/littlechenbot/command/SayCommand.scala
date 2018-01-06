package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.service.TranslateService
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait SayCommand extends Command {
  this: Http =>

  private val commands = List("say")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.BAKA_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, 0, "h", "help").unitFlatMap {
        replyMan(locale.TEXT_TO_SPEECH,
          (List("-l", "--language"), Some("string"),
            locale.SET_TARGET_LANGUAGE) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val language = arguments("l", "language").asString.getOrElse("en")
      val list = arguments.free.flatMap(_.asString.filter(_.trim.nonEmpty))
      val text = if (list.isEmpty) "nyan" else list.reduce(_ + " " + _)

      checkArguments(arguments, 100, "l", "language")
        .unitFlatMap(TranslateService.guessLanguage(text))
        .flatMap { l =>
          if (l.getOrElse(language) == language) {
            Future.successful(text)
          } else {
            TranslateService.transliterate(text)
          }
        }
        .flatMap(TranslateService.textToSpeech(_, language))
        .flatMap(audio => request(SendVoice(message.chat.id,
          InputFile(s"${System.currentTimeMillis}.opus", audio),
          replyToMessageId = Some(message.messageId))))
        .statusMap(Status.Success)
        .recoverWith {
          case _: TranslateService.UnsupportedLanguageException =>
            Future.failed(new CommandException(locale.INVALID_LANGUAGE))
        }
        .recoverWith(handleError(Some(locale.TRANSLATION_REQUEST_FV_FS))(message))
    }
  }
}
