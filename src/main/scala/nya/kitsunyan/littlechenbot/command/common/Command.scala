package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.database.LocaleConfigurationData
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future
import scala.language.implicitConversions

trait Command extends BotBase with AkkaDefaults {
  case class Bot(nickname: String, id: Long)

  val bot: Future[Bot]
  val workspace: Option[Long]
  val botOwner: Option[Long]

  case class FilterChat(soft: Boolean, hard: Boolean, filtered: Boolean = false)

  def filterChat(message: Message): FilterChat = FilterChat(true, true)

  case class Description(commands: List[String], text: String)

  def prependDescription(list: List[Description], locale: Locale): List[Description] = list

  def handleException(e: Throwable, causalMessage: Option[Message]): Unit

  def handleError(during: Option[String])(causalMessage: Message)(implicit message: Message, locale: Locale):
    PartialFunction[Throwable, Future[Any]] = {
    case e: RecoverException =>
      e.future
    case e: CommandException =>
      replyQuote(e.getMessage, e.parseMode)
    case e: Exception =>
      handleErrorCommon(e, causalMessage, during)
  }

  def handleErrorCommon(e: Exception, causalMessage: Message, during: Option[String])
    (implicit message: Message, locale: Locale): Future[Any] = {
    handleException(e, Some(causalMessage))
    val anExceptionWasThrown = during
      .map(locale.AN_EXCEPTION_WAS_THROWN_FORMAT.format(_))
      .getOrElse(locale.AN_EXCEPTION_WAS_THROWN)
    replyQuote(s"$anExceptionWasThrown\n${userMessageForException(e)}")
  }

  def userMessageForException(e: Exception): String = {
    e match {
      case e: UserMessageException =>
        e.userMessage.getOrElse(e.getCause.getClass.getName)
      case e: java.io.IOException =>
        val message = Option(e.getMessage)
          .filter(s => !s.contains("http://") && !s.contains("https://"))
          .filter("\\d+\\.\\d+\\.\\d+\\.\\d+".r.findFirstIn(_).isEmpty)
          .map(": " + _)
          .getOrElse("")
        s"${e.getClass.getName}$message"
      case e =>
        e.getClass.getName
    }
  }

  private def getLocale(chatId: Long): Future[Locale] = {
    LocaleConfigurationData.get(chatId)
      .map(_.getOrElse(Locale.English))
  }

  private def filterCommands(commands: List[String], botNickname: String)(text: String): Option[String] = {
    commands.foldLeft[Option[String]](None) { (a, command) =>
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
        }.reduceLeft(_ orElse _)
      }
    }
  }

  final def filterMessage(commands: List[String], success: (Arguments, Locale) => Future[Any],
    fail: FilterChat => Future[Any], filterChat: FilterChat, allow: FilterChat => Boolean)
    (implicit message: Message): Future[Any] = {
    bot.flatMap { bot =>
      (message.text orElse message.caption)
        .flatMap(filterCommands(commands, bot.nickname)).map { commandLine =>
        if (allow(filterChat)) {
          getLocale(message.chat.id).flatMap(success(new Arguments(commandLine), _))
        } else {
          fail(filterChat.copy(filtered = true))
        }
      }.getOrElse(fail(filterChat))
    }
  }

  class RecoverException(val future: Future[Any]) extends Exception

  class CommandException(message: String, val parseMode: Option[ParseMode.ParseMode] = None) extends Exception(message)

  final override def onMessage(message: Message): Unit = {
    handleMessage(filterChat(message))(message).recover {
      case e => handleException(e, Some(message))
    }
  }

  def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    if (filterChat.hard && !filterChat.soft && filterChat.filtered) {
      getLocale(message.chat.id).flatMap(handleNotPermittedWarning)
    } else {
      Future.unit
    }
  }

  def handleNotPermittedWarning(locale: Locale)(implicit message: Message): Future[Any] = Future.unit

  def checkArguments(arguments: Arguments, possibleArguments: String*)(implicit locale: Locale): Future[Unit] = {
    val invalidArguments = arguments.keySet.diff(possibleArguments.toSet[String])

    if (invalidArguments.nonEmpty) {
      val printInvalidArgument = clearMarkup(invalidArguments.find(!_.isEmpty)
        .getOrElse(arguments.freeValue.asString.flatMap(_.split("\n").headOption).getOrElse("")))

      Future.failed(new CommandException(s"${locale.INVALID_ARGUMENT_FS}: $printInvalidArgument."))
    } else {
      Future.unit
    }
  }

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

  object WorkspaceRequest {
    def apply(command: String)(id: Long): String = {
      s"[request:$command:$id]"
    }

    def parse(command: String)(message: String)(implicit locale: Locale): Option[Long] = {
      "\\[request:(\\w+?):(-?\\d+)\\]".r.findFirstMatchIn(message)
        .map(_.subgroups)
        .map(g => (g(0), g(1).toLong))
        .map { case (parsedCommand, id) =>
        if (parsedCommand == command) {
          id
        } else {
          throw new CommandException(locale.DIFFERENT_COMMANDS_FORMAT.format(s"`/$command`", s"`/$parsedCommand`"),
            Some(ParseMode.Markdown))
        }
      }
    }
  }

  class RecoverableFuture[A, B, R](future: Future[(A, B)], callback: (A, B) => Future[R]) {
    def recoverWith[T >: R](defaultValue: A)(recover: A => PartialFunction[Throwable, Future[T]]): Future[T] = {
      future.flatMap { case (a, b) =>
        callback(a, b).recoverWith(recover(a))
      }.recoverWith(recover(defaultValue))
    }
  }

  class ScopeFuture[A, B](future: Future[(A, B)]) {
    def scopeFlatMap[R](callback: (A, B) => Future[R]): RecoverableFuture[A, B, R] = {
      new RecoverableFuture(future, callback)
    }
  }

  implicit def scopeFuture[A, B](future: Future[(A, B)]): ScopeFuture[A, B] = {
    new ScopeFuture(future)
  }

  class UnitFuture(future: Future[Unit]) {
    def unitMap[T](f: => T): Future[T] = future.map(_ => f)
    def unitFlatMap[T](f: => Future[T]): Future[T] = future.flatMap(_ => f)
  }

  implicit def unitFuture(future: Future[Unit]): UnitFuture = {
    new UnitFuture(future)
  }
}
