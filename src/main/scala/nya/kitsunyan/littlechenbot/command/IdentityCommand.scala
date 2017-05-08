package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait IdentityCommand extends Command {
  private val commands = List("identity", "id")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.WHO_ARE_YOU_FD) :: list, locale)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage, filterChat, _.hard)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.GET_INFORMATION_ABOUT_QUOTED_USER_OR_YOURSELF,
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments).unitFlatMap {
        val targetMessage = message.replyToMessage.getOrElse(message)
        val name = targetMessage.from
          .map(u => u.firstName + u.lastName.map(" " + _).getOrElse(""))
          .getOrElse("unknown")
        val username = targetMessage.from
          .flatMap(_.username.map("@" + _))
          .getOrElse("unknown")
        val chatId = targetMessage.chat.id
        val userId = targetMessage.from
          .map(_.id.toString)
          .getOrElse("unknown")
        replyQuote(s"Name: $name\nUsername: $username\nChat ID: $chatId\nUser ID: $userId")
      }.recoverWith(handleError(None)(message))
    }
  }
}
