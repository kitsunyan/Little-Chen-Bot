package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util.UserMessageException

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait Command extends BotBase with AkkaDefaults {
  case class Bot(nickname: String, id: Long)

  val bot: Future[Bot]
  val workspace: Option[Long]

  case class FilterChat(soft: Boolean, hard: Boolean)

  def filterChat(message: Message): FilterChat = FilterChat(true, true)

  case class Description(commands: List[String], text: String)

  def prependDescription(list: List[Description]): List[Description] = list

  def handleException(e: Throwable, causalMessage: Message): Unit

  def handleError(kind: String)(causalMessage: Message)(implicit message: Message):
    PartialFunction[Throwable, Future[Any]] = {
    case e: RecoverException =>
      e.future
    case e: CommandException =>
      replyQuote(e.getMessage, e.parseMode)
    case e: Exception =>
      handleErrorCommon(e, causalMessage, kind)
  }

  def handleErrorCommon(e: Exception, causalMessage: Message, kind: String)(implicit message: Message): Future[Any] = {
    handleException(e, causalMessage)
    val userMessage = (e match {
      case e: UserMessageException => e.userMessage
      case _ => None
    }).map("\n\n" + _).getOrElse("")
    replyQuote(s"An exception was thrown during $kind.$userMessage")
  }

  private def filterCommands(commands: List[String], botNickname: String)(text: String): Option[Arguments] = {
    commands.foldLeft[Option[Arguments]](None) { (a, command) =>
      a orElse {
        val shortCommand = "/" + command
        val longCommand = shortCommand + "@" + botNickname

        Seq(shortCommand, longCommand).map { fullCommand =>
          if (text.equals(fullCommand)) {
            Some("")
          } else if (text.startsWith(fullCommand) && text.charAt(fullCommand.length) <= ' ') {
            Some(text.substring(fullCommand.length + 1))
          } else {
            None
          }
        }.reduceLeft(_ orElse _).map(new Arguments(_))
      }
    }
  }

  final def filterMessage(commands: List[String], success: Arguments => Future[Any], fail: => Future[Any],
    allow: Boolean)(implicit message: Message): Future[Any] = {
    bot.flatMap { bot =>
      if (allow) {
        (message.text orElse message.caption)
          .flatMap(filterCommands(commands, bot.nickname))
          .map(success).getOrElse(fail)
      } else {
        fail
      }
    }
  }

  class RecoverException(val future: Future[Any]) extends Exception

  class CommandException(message: String, val parseMode: Option[ParseMode.ParseMode] = None) extends Exception(message)

  final override def onMessage(message: Message): Unit = {
    handleMessage(filterChat(message))(message).recover {
      case e => handleException(e, message)
    }
  }

  def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = Future.unit

  def replyQuote(text: String, parseMode: Option[ParseMode.ParseMode] = None)
    (implicit message: Message): Future[Message] = {
    request(SendMessage(Left(message.sender), text, parseMode, replyToMessageId = Some(message.messageId)))
  }

  def reply(text: String, parseMode: Option[ParseMode.ParseMode] = None)
    (implicit message: Message): Future[Message] = {
    request(SendMessage(Left(message.sender), text, parseMode))
  }

  def replyMan(description: String, list: List[(List[String], Option[String], String)])
    (implicit message: Message): Future[Message] = {
    val argumentsListText = list.map { case (parameters, values, description) =>
      val parametersFull = "`" + parameters.reduce(_ + "`, `" + _) + "`"
      val maxLength = 30
      val space = "    "

      val (_, descriptionFull) = description.split(" +").foldLeft((maxLength, "")) { (acc, value) =>
        val (line, result) = acc
        val length = value.length + (if (line > 0) 1 else 0)
        if (line + length > maxLength && line > 0) {
          (value.length, s"$result\n$space$value")
        } else {
          (line + length, s"$result $value")
        }
      }

      val valuesFull = values.map(v => s" `[$v]`").getOrElse("")
      s"$parametersFull$valuesFull$descriptionFull"
    }.foldLeft("")(_ + "\n" + _)

    val text = if (argumentsListText.nonEmpty) s"$description\n$argumentsListText" else description
    replyQuote(text, Some(ParseMode.Markdown))
  }

  def clearMarkup(text: String): String = {
    text.replaceAll("[`*_\\[\\]()]", "")
  }

  def trimCaption(caption: String): String = {
    // 200 is maximum caption length for Telegram
    if (caption.length > 200) {
      val index = caption.lastIndexOf('\n', 200)
      if (index >= 0) {
        caption.substring(0, index)
      } else {
        caption
      }
    } else {
      caption
    }
  }
}
