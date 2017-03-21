package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.duration.Duration
import scala.concurrent._

trait Command extends BotBase with AkkaDefaults {
  val botNickname: Future[String]

  def filterChat(message: Message): Boolean = true

  class Arguments(data: String) {
    private def parseArguments(data: String): Map[String, String] = {
      case class CharFold(quote: Boolean = false, escape: Boolean = false,
        chars: List[Char] = List(), arguments: List[String] = List()) {
        def close(): CharFold = {
          if (chars.nonEmpty) {
            copy(chars = List(), arguments = chars.reverse.mkString :: arguments)
          } else {
            this
          }
        }
      }

      val arguments = data.foldLeft(CharFold()) { (f, c) =>
        if (c <= ' ' && !f.quote && !f.escape) {
          f.close()
        } else {
          if (f.escape) {
            f.copy(escape = false, chars = c :: f.chars)
          } else {
            if (c == '\\') {
              f.copy(escape = true)
            } else if (c == '"') {
              f.copy(quote = !f.quote)
            } else {
              f.copy(chars = c :: f.chars)
            }
          }
        }
      }.close().arguments.reverse

      case class ArgumentsFold(what: Option[String] = None, arguments: Map[String, String] = Map()) {
        def append(argument: String): ArgumentsFold = {
          if (argument.startsWith("-") || argument.startsWith("—")) {
            what.map(w => copy(arguments = arguments + (w -> ""))).getOrElse(this).copy(what = Some(argument))
          } else {
            what.map(w => copy(what = None, arguments + (w -> argument))).getOrElse(this)
          }
        }
      }

      arguments.foldLeft(ArgumentsFold())(_.append(_)).append("").arguments
    }

    private val arguments = parseArguments(data)

    def string(shortKey: String, longKey: String): Option[String] = {
      arguments.get("-" + shortKey) orElse arguments.get("--" + longKey) orElse arguments.get("—" + longKey)
    }

    def int(shortKey: String, longKey: String): Option[Int] = {
      string(shortKey, longKey).flatMap { v =>
        try {
          Some(v.toInt)
        } catch {
          case _: NumberFormatException => None
        }
      }
    }
  }

  final def filterMessage(command: String)(implicit message: Message): Option[Arguments] = {
    if (filterChat(message)) {
      (message.text orElse message.caption).flatMap { text =>
        val shortCommand = "/" + command
        val longCommand = shortCommand + "@" + Await.result(botNickname, Duration.Inf)
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
    } else {
      None
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
