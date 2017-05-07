package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
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

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def isOwnerMessage(implicit message: Message): Boolean = {
      botOwner.exists(id => message.from.exists(_.id == id))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        val commands =
          (false, List("--check-proxy"), None,
            locale.CHECK_PROXY_AVAILABLE) ::
          (true, List("--restart-proxy"), None,
            locale.RESTART_PROXY) ::
          (true, List("-m", "--send-message"), Some("string"),
            locale.SEND_MESSAGE_FROM_BOT) ::
          (true, List("-t", "--target-chat"), Some("long or string"),
            locale.TARGET_CHAT_ID_OR_ALIAS_FOR_FORMAT.format("`--send-message`")) ::
          (false, List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil

        replyMan(locale.BOT_CONTROL_AND_ADMINISTRATION,
          commands.flatMap { case (owner, parameters, values, description) =>
          if (!owner || isOwnerMessage) {
            Some(parameters, values, description)
          } else {
            None
          }
        })
      }.recoverWith(handleError(None)(message))
    } else if (arguments("check-proxy").nonEmpty) {
      checkArguments(arguments, "check-proxy").unitFlatMap {
        if (proxy.nonEmpty) {
          Future(http("https://gelbooru.com", proxy = true).runString(HttpFilters.ok)(identity))
            .flatMap(_ => replyQuote(locale.IT_WORKS)).recoverWith {
            case e: Exception => replyQuote(s"${locale.EVERYTHING_IS_BROKEN}\n${userMessageForException(e)}")
          }
        } else {
          replyQuote(locale.PROXY_IS_NOT_PRESENT)
        }
      }.recoverWith(handleError(None)(message))
    } else if (arguments("restart-proxy").nonEmpty && isOwnerMessage) {
      checkArguments(arguments, "restart-proxy").unitFlatMap {
        if (proxy.nonEmpty) {
          restartProxyCommand.map { restartProxyCommand =>
            Future(Utils.exec(None, restartProxyCommand)).flatMap(_ => replyQuote(locale.READY)).recoverWith {
              case e: Exception => replyQuote(s"${locale.SOMETHING_WENT_WRONG}\n${userMessageForException(e)}")
            }
          }.getOrElse(replyQuote(locale.I_DONT_KNOW_HOW))
        } else {
          replyQuote(locale.PROXY_IS_NOT_PRESENT)
        }
      }.recoverWith(handleError(None)(message))
    } else if (arguments("m", "send-message").nonEmpty && isOwnerMessage) {
      checkArguments(arguments, "m", "send-message", "t", "target-chat").unitFlatMap {
        val targetChatValue = arguments("t", "target-chat")
        val targetChat = (targetChatValue.asLong orElse targetChatValue.asString.flatMap(chatForAlias))
          .getOrElse(message.chat.id)

        request(SendMessage(Left(targetChat), arguments("m", "send-message").asString.getOrElse("")))
          .recoverWith(handleError(Some(locale.SENDING_THE_MESSAGE_FL_FS))(message))
      }.recoverWith(handleError(None)(message))
    } else {
      checkArguments(arguments).unitFlatMap {
        replyQuote(locale.UNKNOWN_COMMAND_TYPE_TO_VIEW_HELP_FORMAT.format(s"`/${commands.head} --help`"),
          Some(ParseMode.Markdown))
      }.recoverWith(handleError(None)(message))
    }
  }
}
