package nya.kitsunyan.littlechenbot

import info.mukel.telegrambot4s.api.BotBase
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.Message

import scala.collection.mutable
import scala.concurrent.Future

trait Commands extends BotBase {
  private val commands = mutable.Map[Message => Boolean, Message => Unit]()
  def on(filter: Message => Boolean)(action: Message => Unit): Unit = commands += filter -> action

  override def onMessage(message: Message): Unit = {
    for ((filter, action) <- commands) {
      if (filter(message)) {
        action(message)
      }
    }
  }

  def filterChat(message: Message): Boolean = true

  def command(command: String)(message: Message): Boolean = {
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
          val extended = "/" + command
          text match {
            case text if text == extended || text.startsWith(extended + "@") => true
            case _ => false
          }
        case None => false
      }
    } else {
      false
    }
  }

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
