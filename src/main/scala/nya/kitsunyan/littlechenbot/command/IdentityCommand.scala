package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait IdentityCommand extends Command {
  private val commands = List("identity")

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.hard)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    val chatId = message.chat.id
    val userId = message.from.map(_.id.toString).getOrElse("unknown")
    replyQuote(s"Chat ID: _${chatId}_\nUser ID: _${userId}_", Some(ParseMode.Markdown))
  }
}