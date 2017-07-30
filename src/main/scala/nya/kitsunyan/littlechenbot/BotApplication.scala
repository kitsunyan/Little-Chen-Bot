package nya.kitsunyan.littlechenbot

import nya.kitsunyan.littlechenbot.command._
import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.util.Configuration

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

object BotApplication extends App {
  private val configuration = Configuration("littlechenbot.conf")

  object ShikigamiBot extends TelegramBot with CustomPolling with Http with ForemanCommand with HelpCommand
    with ControlCommand with PixivCommand with IqdbCommand with RateCommand with GoogleCommand
    with GuessCommand with IdentityCommand {
    override def token: String = configuration.string("bot.token").get

    override val workspace: Option[Long] = configuration.long("bot.workspace")
    override val bot: Future[Bot] = getMe.map(m => Bot(m.username.getOrElse(""), m.id))
    override val botOwner: Option[Long] = configuration.long("bot.owner")

    override val startTime: Long = executionStart

    private case class Chat(id: Long, alias: Option[String], foreignCommands: Option[List[String]])

    private val chats = configuration.configurationList("bot.chats")
      .flatMap(c => c.long("id").map(Chat(_, c.string("alias"), c.stringList("foreignCommands"))))

    private val chatsAnyPrivate = configuration.boolean("bot.chatsAnyPrivate").getOrElse(false)
    private val chatsAnyGroup = configuration.boolean("bot.chatsAnyGroup").getOrElse(false)

    override val proxy: Option[java.net.Proxy] = {
      for {
        host <- configuration.string("bot.proxy.host")
        port <- configuration.int("bot.proxy.port")
        typeString <- configuration.string("bot.proxy.type")
        proxyType <- try {
          Some(java.net.Proxy.Type.valueOf(typeString.toUpperCase))
        } catch {
          case _: Exception => None
        }
      } yield new java.net.Proxy(proxyType, java.net.InetSocketAddress.createUnresolved(host, port))
    }

    override val restartProxyCommand: Option[Seq[String]] = configuration.stringList("bot.proxy.restart")

    override def chatForAlias(alias: String): Option[Long] = {
      chats.find(_.alias.contains(alias)).map(_.id)
    }

    override def foreignCommands(chatId: Long): Option[List[String]] = {
      chats.find(_.id == chatId).flatMap(_.foreignCommands)
    }

    override def foremanImage: Option[String] = configuration.string("bot.foremanImage")

    override def filterChat(message: Message): FilterChat = {
      if (message.date < startTime / 1000) {
        FilterChat(false, false)
      } else {
        val soft = chats.map(_.id).contains(message.chat.id) ||
          chatsAnyPrivate && Set(ChatType.Private).contains(message.chat.`type`) ||
          chatsAnyGroup && Set(ChatType.Group, ChatType.Supergroup).contains(message.chat.`type`)
        FilterChat(soft, true)
      }
    }

    override def handleException(causalMessage: Option[Message])(e: Throwable): Unit = {
      e.printStackTrace()
      causalMessage.foreach { causalMessage =>
        botOwner.map { botOwner =>
          request(ForwardMessage(botOwner, causalMessage.chat.id,
            Some(true), causalMessage.messageId)).flatMap { sentMessage =>
            val writer = new java.io.StringWriter
            e.printStackTrace(new java.io.PrintWriter(writer))
            request(SendMessage(botOwner, "```\n" + writer.toString + "\n```", Some(ParseMode.Markdown),
              Some(true), Some(true), Some(sentMessage.messageId)))
          }.recover {
            case e => e.printStackTrace()
          }
        }
      }
    }
  }

  ShikigamiBot.run()
}
