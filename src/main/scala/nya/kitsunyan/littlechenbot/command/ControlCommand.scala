package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util.Utils

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait ControlCommand extends Command {
  this: Http =>

  private val commands = List("chenctl")

  val restartProxyCommand: Option[Seq[String]]

  def chatForAlias(alias: String): Option[Long]

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    def isOwnerMessage(implicit message: Message): Boolean = {
      botOwner.exists(id => message.from.exists(_.id == id))
    }

    if (arguments("h", "help").nonEmpty) {
      val commands =
        (false, List("--check-proxy"), None,
          "Check proxy available.") ::
        (true, List("--restart-proxy"), None,
          "Restart proxy.") ::
        (true, List("-m", "--send-message"), Some("string"),
          "Send message from bot.") ::
        (true, List("-t", "--target-chat"), Some("long or string"),
          "Target chat ID or alias for `--send-message`.") ::
        (false, List("-h", "--help"), None,
          "Display this help.") ::
        Nil

      replyMan("Bot control and administration.", commands.flatMap { case (owner, parameters, values, description) =>
        if (!owner || isOwnerMessage) {
          Some(parameters, values, description)
        } else {
          None
        }
      })
    } else if (arguments("check-proxy").nonEmpty) {
      if (proxy.nonEmpty) {
        Future(http("https://gelbooru.com", proxy = true).runString(HttpFilters.ok)(identity))
          .flatMap(_ => replyQuote("It works!")).recoverWith {
          case e: Exception => replyQuote(s"Everything is broken!\n${userMessageForException(e)}")
        }
      } else {
        replyQuote("Proxy is not present.")
      }
    } else if (arguments("restart-proxy").nonEmpty && isOwnerMessage) {
      if (proxy.nonEmpty) {
        restartProxyCommand.map { restartProxyCommand =>
          Future(Utils.exec(None, restartProxyCommand)).flatMap(_ => replyQuote("Ready!")).recoverWith {
            case e: Exception => replyQuote(s"Something went wrong!\n${userMessageForException(e)}")
          }
        }.getOrElse(replyQuote("I don't know how!"))
      } else {
        replyQuote("Proxy is not present.")
      }
    } else if (arguments("m", "send-message").nonEmpty && isOwnerMessage) {
      val targetChatValue = arguments("t", "target-chat")
      val targetChat = (targetChatValue.asLong orElse targetChatValue.asString.flatMap(chatForAlias))
        .getOrElse(message.chat.id)

      request(SendMessage(Left(targetChat), arguments("m", "send-message").asString.getOrElse("")))
        .recoverWith(handleError("sending a message")(message))
    } else {
      replyQuote(s"Unknown command.\nType `/${commands.head} --help` to view help.", Some(ParseMode.Markdown))
    }
  }
}
