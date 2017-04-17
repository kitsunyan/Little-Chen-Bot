package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._

import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait IdentityCommand extends Command with Describable {
  private val commands = List("identity")

  override def prependDescription(list: List[Description]): List[Description] = {
    super.prependDescription(Description(commands, "who are you?") :: list)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.hard)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    if (arguments.string("h", "help").nonEmpty) {
      replyMan("Get information about quoted user or yourself.", Nil)
    } else {
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
    }
  }
}
