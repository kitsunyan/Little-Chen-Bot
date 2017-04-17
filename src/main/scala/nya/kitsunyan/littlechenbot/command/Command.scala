package nya.kitsunyan.littlechenbot.command

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait Command extends BotBase with AkkaDefaults {
  val botNickname: Future[String]

  case class FilterChat(soft: Boolean, hard: Boolean)

  def filterChat(message: Message): FilterChat = FilterChat(true, true)

  def handleException(e: Throwable, causalMessage: Message): Unit

  def handleError(causalMessage: Message, kind: String)(implicit message: Message):
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
    replyQuote(s"An exception was thrown during $kind.")
  }

  class Arguments(data: String) {
    private def parseArguments(data: String): Map[String, String] = {
      case class CharFold(quote: Boolean = false, escape: Boolean = false,
        chars: List[Char] = Nil, arguments: List[String] = Nil) {
        def close(): CharFold = {
          if (chars.nonEmpty) {
            copy(chars = Nil, arguments = chars.reverse.mkString :: arguments)
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

    private def option(prefix: String, key: String): Option[String] = {
      Option(key).map(prefix + _).flatMap(arguments.get)
    }

    def string(shortKey: String, longKey: String): Option[String] = {
      option("-", shortKey) orElse option("--", longKey) orElse option("—", longKey)
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
    botNickname.flatMap { botNickname =>
      if (allow) {
        (message.text orElse message.caption)
          .flatMap(filterCommands(commands, botNickname))
          .map(success).getOrElse(fail)
      } else {
        fail
      }
    }
  }

  class RecoverException(val future: Future[Any]) extends Exception

  class CommandException(message: String, val parseMode: Option[ParseMode.ParseMode] = None) extends Exception(message)

  final override def onMessage(message: Message): Unit = {
    handleMessage(filterChat(message))(message)
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
    }.reduce(_ + "\n" + _)

    val text = if (argumentsListText.nonEmpty) s"$description\n\n$argumentsListText" else description
    replyQuote(text, Some(ParseMode.Markdown))
  }
}
