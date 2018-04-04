package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait AttachCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("attach")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FETCH_IMAGE_AND_ATTACH_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    def replyWithImage(asDocument: Boolean)(typedFile: TypedFile): Future[Message] = {
      if (asDocument) {
        request(SendDocument(message.source, InputFile(typedFile.name, typedFile.data),
          replyToMessageId = Some(message.messageId)))
      } else {
        sendPhotoOrDocument(message.source, InputFile(typedFile.name, typedFile.data),
          replyToMessageId = Some(message.messageId))
      }
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, 0, "h", "help").unitFlatMap {
        replyMan(locale.DOWNLOAD_AND_ATTACH_IMAGE,
          (List("-d", "--as-document"), None,
            locale.FETCH_IMAGE_AS_DOCUMENT) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val asDocument = arguments("d", "as-document").nonEmpty

      checkArguments(arguments, 0, "d", "as-document")
        .unitMap(obtainMessageFile(commands.head)(extractMessageWithImage))
        .scopeFlatMap((_, file) => readTelegramFile(file)
          .flatMap(replyWithImage(asDocument))
          .statusMap(Status.Success))
        .recoverWith(message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
    }
  }
}
