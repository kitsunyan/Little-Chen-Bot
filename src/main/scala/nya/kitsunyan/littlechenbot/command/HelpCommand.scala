package nya.kitsunyan.littlechenbot.command
import info.mukel.telegrambot4s.models.Message

import scala.concurrent.Future

trait HelpCommand extends Command with Describable {
  private val commands = List("help")

  override def prependDescription(list: List[Description]): List[Description] = {
    super.prependDescription(Description(commands, "show this help") :: list)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.hard)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    replyQuote(prependDescription(Nil).foldRight("List of supported commands:\n") { (v, a) =>
      s"$a\n/${v.commands.head} — ${v.text}"
    })
  }
}
