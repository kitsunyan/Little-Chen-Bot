package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait HelpCommand extends Command {
  private val commands = List("help")

  override def prependDescription(list: List[Description]): List[Description] = {
    super.prependDescription(Description(commands, "show this help") :: list)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.hard)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    if (arguments("h", "help").nonEmpty) {
      replyMan("Display list of supported commands.",
        (List("-h", "--help"), None,
          "Display this help.") ::
        Nil)
    } else {
      val listOfCommands = prependDescription(Nil).foldRight("List of supported commands:\n") { (v, a) =>
        s"$a\n/${v.commands.head} — ${clearMarkup(v.text)}"
      }
      replyQuote(s"$listOfCommands\n\nYou can view a help for each command using `/command --help`.",
        Some(ParseMode.Markdown))
    }
  }
}
