package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait DocumentCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("document")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FETCH_IMAGE_AS_DOCUMENT_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    def replyWithImage(typedFile: TypedFile): Future[Message] = {
      request(SendDocument(message.source, InputFile(typedFile.name, typedFile.data),
        replyToMessageId = Some(message.messageId)))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.FETCH_IMAGE_AS_DOCUMENT,
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments)
        .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
        .scopeFlatMap((_, file) => readTelegramFile(file)
          .flatMap(replyWithImage)
          .statusMap(Status.Success))
        .recoverWith(message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
    }
  }
}
