package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.database.LocaleConfigurationData
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait ControlCommand extends Command {
  this: Http =>

  private val commands = List("chenctl")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.BOT_CONTROL_FD) :: list, locale)
  }

  val restartProxyCommand: Option[Seq[String]]

  def chatForAlias(alias: String): Option[Long]

  val startTime: Long

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(filterChat.soft)(_, _, _),
      super.handleMessage, filterChat, _.hard)
  }

  private def handleMessageInternal(softFiltered: Boolean)
    (implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    val ownerMessage = botOwner.exists(id => message.from.exists(_.id == id))

    def checkAdministratorOrOwner: Future[Unit] = {
      message.from.map { user =>
        if (message.chat.`type` == ChatType.Private || botOwner.contains(user.id)) {
          Future.unit
        } else {
          request(GetChatAdministrators(message.chat.id)).map { administrators =>
            if (!administrators.map(_.user.id).contains(user.id)) {
              throw new CommandException(locale.ONLY_ADMINISTRATOR_CAN_DO_IT)
            }
          }
        }
      }.getOrElse(Future.failed(new Exception("Can not obtain user ID.")))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, 0, "h", "help").unitFlatMap {
        val commands =
          (true, false, List("--check-proxy"), None,
            locale.CHECK_PROXY_AVAILABLE) ::
          (true, true, List("--restart-proxy"), None,
            locale.RESTART_PROXY) ::
          (true, true, List("-m", "--send-message"), Some("string"),
            locale.SEND_MESSAGE_FROM_BOT) ::
          (true, true, List("-t", "--target-chat"), Some("long or string"),
            locale.TARGET_CHAT_ID_OR_ALIAS_FOR_FORMAT.format("`--send-message`")) ::
          (true, false, List("-d", "--delete-message"), Some("string"),
            locale.DELETE_QUOTED_MESSAGE) ::
          (true, false, List("--set-locale"), Some("string"),
            locale.SET_LOCALE_FOR_THIS_CHAT) ::
          (false, false, List("--request-permission"), Some("string"),
            locale.REQUEST_PERMISSION_TO_INTERACT_WITH_BOT) ::
          (true, false, List("--uptime"), None,
            locale.SHOW_UPTIME) ::
          (false, false, List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil

        replyMan(locale.BOT_CONTROL_AND_ADMINISTRATION,
          commands.flatMap { case (softFilteredOnly, ownerOnly, parameters, values, description) =>
          if ((!ownerOnly || ownerMessage) && (!softFilteredOnly || softFiltered)) {
            Some(parameters, values, description)
          } else {
            None
          }
        })
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else if (softFiltered && arguments("check-proxy").nonEmpty) {
      checkArguments(arguments, 0, "check-proxy").unitFlatMap {
        if (proxy.nonEmpty) {
          http("https://gelbooru.com", proxy = true)
            .runString(Http.Filters.ok)
            .flatMap(_ => replyQuote(locale.IT_WORKS))
            .statusMap(Status.Success)
            .recoverWith((e: Throwable) => replyQuote(s"${locale.EVERYTHING_IS_BROKEN}\n${userMessageForException(e)}")
              .statusMap(Status.Fail))
        } else {
          replyQuote(locale.PROXY_IS_NOT_PRESENT)
            .statusMap(Status.Fail)
        }
      }.recoverWith(handleError(None)(message))
    } else if (softFiltered && ownerMessage && arguments("restart-proxy").nonEmpty) {
      checkArguments(arguments, 0, "restart-proxy").unitFlatMap {
        if (proxy.nonEmpty) {
          restartProxyCommand.map { restartProxyCommand =>
            Future(Utils.exec(None, restartProxyCommand))
              .flatMap(_ => replyQuote(locale.READY))
              .statusMap(Status.Success)
              .recoverWith((e: Throwable) =>
                replyQuote(s"${locale.SOMETHING_WENT_WRONG}\n${userMessageForException(e)}")
                  .statusMap(Status.Fail))
          }.getOrElse(replyQuote(locale.I_DONT_KNOW_HOW)
            .statusMap(Status.Fail))
        } else {
          replyQuote(locale.PROXY_IS_NOT_PRESENT)
            .statusMap(Status.Fail)
        }
      }.recoverWith(handleError(None)(message))
    } else if (softFiltered && ownerMessage && arguments("m", "send-message").nonEmpty) {
      checkArguments(arguments, 0, "m", "send-message", "t", "target-chat").unitFlatMap {
        val targetChatValue = arguments("t", "target-chat")
        val targetChat = (targetChatValue.asLong orElse targetChatValue.asString.flatMap(chatForAlias))
          .getOrElse(message.chat.id)

        request(SendMessage(targetChat, arguments("m", "send-message").asString.getOrElse("")))
          .flatMap(_ => replyQuote(locale.MESSAGE_SENT)
            .statusMap(Status.Success))
          .recoverWith(handleError(Some(locale.SENDING_THE_MESSAGE_FL_FS))(message))
      }.recoverWith(handleError(None)(message))
    } else if (softFiltered && arguments("d", "delete-message").nonEmpty) {
      checkArguments(arguments, 0, "d", "delete-message").unitFlatMap {
        checkAdministratorOrOwner.unitFlatMap {
          val messageIdFuture = bot.map { bot =>
            message.replyToMessage.flatMap { replyToMessage =>
              if (replyToMessage.from.map(_.id.toLong).contains(bot.id)) {
                Some(replyToMessage.messageId)
              } else {
                None
              }
            }
          }

          messageIdFuture.flatMap(_.map(i => request(DeleteMessage(message.source, i)))
            .getOrElse(throw new CommandException(locale.ARE_YOU_KIDDING_ME)))
        }.statusMap(Status.Success)
          .recoverWith(handleError(Some(locale.CONFIGURATION_HANDLING_FV_FS))(message))
      }.recoverWith(handleError(None)(message))
    } else if (softFiltered && arguments("set-locale").nonEmpty) {
      checkArguments(arguments, 0, "set-locale").unitFlatMap {
        checkAdministratorOrOwner.unitFlatMap {
          val localeString = arguments("set-locale").asString
          localeString.flatMap(Locale.get).map { newLocale =>
            LocaleConfigurationData.set(message.chat.id, newLocale)
              .unitFlatMap(replyQuote(newLocale.LOCALE_INSTALLED))
          }.getOrElse {
            val locales = Locale.locales.map(_.name).reduceLeft(_ + ", " + _)
            throw new CommandException(locale.INVALID_LOCALE_LIST_FORMAT.format(localeString.getOrElse(""), locales))
          }
        }.statusMap(Status.Success)
          .recoverWith(handleError(Some(locale.CONFIGURATION_HANDLING_FV_FS))(message))
      }.recoverWith(handleError(None)(message))
    } else if (arguments("request-permission").nonEmpty) {
      checkArguments(arguments, 0, "request-permission").unitFlatMap {
        if (softFiltered) {
          replyQuote(locale.PERMISSION_IS_ALREADY_GRANTED)
            .statusMap(Status.Fail)
        } else {
          botOwner.map { botOwner =>
            val chatId = message.chat.id
            val username = message.chat.username.map("@" + _).getOrElse("unknown")
            val requester = message.from.map(u => u.username
              .getOrElse(u.lastName.map(u.firstName + " " + _).getOrElse(u.firstName)))
              .getOrElse("unknown")
            val requestMessage = arguments("request-permission").asString.filter(!_.isEmpty).getOrElse("empty message")
            val ownerMessage = s"Permission request:\nChat ID: $chatId\nUsername: $username\n" +
              s"Requester: $requester\nMessage: $requestMessage"

            request(SendMessage(botOwner, ownerMessage))
              .flatMap(_ => replyQuote(locale.MESSAGE_SENT))
              .statusMap(Status.Success)
          }.getOrElse {
            replyQuote(locale.SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT)
              .statusMap(Status.Fail)
          }
        }
      }.recoverWith(handleError(None)(message))
    } else if (arguments("uptime").nonEmpty) {
      val uptime = (System.currentTimeMillis - startTime) / 1000
      val seconds = uptime % 60
      val minutes = uptime / 60 % 60
      val hours = uptime / 60 / 60 % 24
      val days = uptime / 60 / 60 / 24
      val uptimeString = s"${days}d ${hours}h ${minutes}m ${seconds}s"

      checkArguments(arguments, 0, "uptime")
        .unitFlatMap(replyQuote(uptimeString)
          .statusMap(Status.Success))
        .recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments, 0).unitFlatMap {
        replyQuote(locale.UNKNOWN_COMMAND_TYPE_TO_VIEW_HELP_FORMAT.format(s"`/${commands.head} --help`"),
          Some(ParseMode.Markdown))
          .statusMap(Status.Fail)
      }.recoverWith(handleError(None)(message))
    }
  }

  override def handleNotPermittedWarning(implicit message: Message, locale: Locale): Future[Any] = {
    implicit val argumentsImplicit: Arguments = Arguments.empty

    replyQuote(locale.YOU_ARE_NOT_PERMITTED_CONTACT_OWNER_FORMAT
      .format("`/chenctl --request-permission \"Your message\"`"), Some(ParseMode.Markdown))
      .recoverWith(handleError(None)(message))
  }
}
