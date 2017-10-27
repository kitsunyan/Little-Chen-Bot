package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.database.LocaleConfigurationData
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future
import scala.language.implicitConversions

trait Command extends BotBase with AkkaDefaults with GlobalExecutionContext {
  case class Bot(nickname: String, id: Long)

  val bot: Future[Bot]
  val workspace: Option[Long]
  val botOwner: Option[Long]

  case class FilterChat(soft: Boolean, hard: Boolean, filtered: Boolean = false)

  def filterChat(message: Message): FilterChat = FilterChat(true, true)

  case class Description(commands: List[String], text: String)

  def prependDescription(list: List[Description], locale: Locale): List[Description] = list

  def handleException(causalMessage: Option[Message])(e: Throwable): Unit

  def handleError(during: Option[String])(causalMessage: Message)
    (implicit message: Message, arguments: Arguments, locale: Locale): PartialFunction[Throwable, Future[Status]] = {
    case e: RecoverException =>
      e.future
    case e: CommandException =>
      replyQuote(e.getMessage, e.parseMode)
      Future.successful(Status.Fail)
    case e: Exception =>
      handleErrorCommon(e, causalMessage, during)
      Future.successful(Status.Fail)
  }

  def handleErrorCommon(e: Exception, causalMessage: Message, during: Option[String])
    (implicit message: Message, locale: Locale): Future[Any] = {
    handleException(Some(causalMessage))(e)
    val anExceptionWasThrown = during
      .map(locale.AN_EXCEPTION_WAS_THROWN_FORMAT.format(_))
      .getOrElse(locale.AN_EXCEPTION_WAS_THROWN)
    replyQuote(s"$anExceptionWasThrown\n${userMessageForException(e)}")
  }

  def userMessageForException(e: Throwable): String = {
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

  private def filterCommands(commands: List[String], botNickname: String, text: String): Option[String] = {
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

  final def filterMessage(message: ExtendedMessage, commands: List[String],
    success: (Message, Arguments, Locale) => Future[Status], fail: (ExtendedMessage, FilterChat) => Future[Status],
    filterChat: FilterChat, allow: FilterChat => Boolean): Future[Status] = {
    bot.flatMap { bot =>
      filterCommands(commands, bot.nickname, message.commandText).map { commandLine =>
        if (allow(filterChat)) {
          getLocale(message.initial.chat.id).flatMap(success(message.initial, Arguments(commandLine), _))
        } else {
          fail(message, filterChat.copy(filtered = true))
        }
      }.getOrElse(fail(message, filterChat))
    }
  }

  class RecoverException(val future: Future[Status]) extends Exception

  class CommandException(message: String, val parseMode: Option[ParseMode.ParseMode] = None) extends Exception(message)

  case class ExtendedMessage(initial: Message, firstCommand: Boolean, commandText: String)

  sealed trait Status

  object Status {
    case object Cancel extends Status
    private[Command] case class SuccessMatch(arguments: Arguments) extends Status
    private[Command] case class FailMatch(arguments: Arguments) extends Status

    def Success(implicit arguments: Arguments): Status = SuccessMatch(arguments)
    def Fail(implicit arguments: Arguments): Status = FailMatch(arguments)
  }

  final override def receiveMessage(message: Message): Unit = {
    onMessageExtend(message, false, message.text orElse message.caption)
  }

  private def onMessageExtend(message: Message, firstCommand: Boolean, commandText: Option[String]): Future[Status] = {
    commandText
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(t => handleMessage(ExtendedMessage(message, firstCommand, t), filterChat(message))
        .recover((handleException(Some(message))(_)) -> Status.Cancel)).getOrElse(Future.successful(Status.Cancel))
      .flatMap {
      case Status.SuccessMatch(arguments) => arguments.nextCommand match {
        case Some((Arguments.NextMode.OnSuccess, commandText)) => onMessageExtend(message, false, Some(commandText))
        case _ => Future.successful(Status.Cancel)
      }
      case Status.FailMatch(arguments) => arguments.nextCommand match {
        case Some((Arguments.NextMode.OnFail, commandText)) => onMessageExtend(message, false, Some(commandText))
        case _ => Future.successful(Status.Cancel)
      }
      case Status.Cancel => Future.successful(Status.Cancel)
    }
  }

  def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    if (filterChat.hard && !filterChat.soft && filterChat.filtered) {
      getLocale(message.initial.chat.id)
        .flatMap(handleNotPermittedWarning(message.initial, _))
        .statusMap(Status.Cancel)
    } else {
      Future.successful(Status.Cancel)
    }
  }

  def handleNotPermittedWarning(implicit message: Message, locale: Locale): Future[Any] = Future.unit

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
    request(SendMessage(message.source, text, parseMode, replyToMessageId = Some(message.messageId)))
  }

  def reply(text: String, parseMode: Option[ParseMode.ParseMode] = None)
    (implicit message: Message): Future[Message] = {
    request(SendMessage(message.source, text, parseMode))
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
    def apply(command: String)(id: Int): String = {
      s"[request:$command:$id]"
    }

    def parse(command: String)(message: String)(implicit locale: Locale): Option[Int] = {
      "\\[request:(\\w+?):(-?\\d+)\\]".r.findFirstMatchIn(message)
        .map(_.subgroups)
        .map(g => (g(0), g(1).toInt))
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

  implicit def anyThrowable[T](function: Throwable => T): PartialFunction[Throwable, T] = {
    case e => function(e)
  }

  implicit def anyThrowableWithFallback[T](functionAndFallback: (Throwable => Unit, T)):
  PartialFunction[Throwable, T] = {
    case e => functionAndFallback match {
      case (function, result) =>
        function(e)
        result
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

  class StatusFuture[T](future: Future[T]) {
    def statusMap(status: Status): Future[Status] = future.map(_ => status)
  }

  implicit def statusFuture[T](future: Future[T]): StatusFuture[T] = {
    new StatusFuture(future)
  }
}
