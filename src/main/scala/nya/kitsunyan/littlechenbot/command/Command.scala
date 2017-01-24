package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.duration.Duration
import scala.concurrent._

trait Command extends BotBase with AkkaDefaults {
  val botNickname: Future[String]

  def filterChat(message: Message): Boolean = true

  final def filterMessage(command: String)(implicit message: Message): Boolean = {
    if (filterChat(message)) {
      (message.text match {
        case Some(text) => Some(text)
        case None =>
          message.caption match {
            case Some(caption) => Some(caption)
            case None => None
          }
      }) match {
        case Some(text) =>
          val shortCommand = "/" + command
          val longCommand = shortCommand + "@" + Await.result(botNickname, Duration.Inf)
          Seq(shortCommand, longCommand).map { fullCommand =>
            text.equals(fullCommand) || text.startsWith(fullCommand) && text.charAt(fullCommand.length) <= ' '
          }.reduceLeft(_ || _)
        case None => false
      }
    } else {
      false
    }
  }

  class CommandException(message: String) extends Exception(message)

  final override def onMessage(message: Message): Unit = handleMessage(message)

  def handleMessage(implicit message: Message): Unit = ()

  def reply(text: String, replyToMessageId: Option[Long], message: Message): Future[Message] = {
    request(SendMessage(Left(message.sender), text, replyToMessageId = replyToMessageId))
  }

  def replyQuote(text: String)(implicit message: Message): Future[Message] = {
    reply(text, Some(message.messageId), message)
  }

  def reply(text: String)(implicit message: Message): Future[Message] = {
    reply(text, None, message)
  }
}
