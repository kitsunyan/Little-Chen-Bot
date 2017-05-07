package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait HelpCommand extends Command {
  private val commands = List("help")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.DISPLAY_THIS_HELP_FD) :: list, locale)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.DISPLAY_LIST_OF_SUPPORTED_COMMANDS,
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments).unitFlatMap {
        val listOfCommands = prependDescription(Nil, locale)
          .foldRight(s"${locale.LIST_OF_SUPPORTED_COMMANDS_FS}:\n") { (v, a) =>
          s"$a\n/${v.commands.head} — ${clearMarkup(v.text)}"
        }

        replyQuote(s"$listOfCommands\n\n${locale.YOU_CAN_VIEW_A_HELP_FORMAT.format("`/command --help`")}",
          Some(ParseMode.Markdown))
      }.recoverWith(handleError(None)(message))
    }
  }
}
