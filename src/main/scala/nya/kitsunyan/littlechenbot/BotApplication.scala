package nya.kitsunyan.littlechenbot

import nya.kitsunyan.littlechenbot.command._
import nya.kitsunyan.littlechenbot.command.common.Http
import nya.kitsunyan.littlechenbot.util.Configuration

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

object BotApplication extends App {
  private val configuration = Configuration("littlechenbot.conf")

  object ShikigamiBot extends TelegramBot with Polling with Http with HelpCommand with IqdbCommand with RateCommand
    with GoogleCommand with GuessCommand with IdentityCommand {
    override def token: String = config.getString("bot.token")
    override val bot: Future[Bot] = request(GetMe).map(m => Bot(m.username.getOrElse(""), m.id))
    override val workspace: Option[Long] = configuration.long("bot.workspace")

    private val botOwner = configuration.long("bot.owner")

    private case class Chat(id: Long, alias: Option[String])

    private val chats = configuration.configurationList("bot.chats")
      .flatMap(c => c.long("id").map(Chat(_, c.string("alias"))))

    private val chatsAnyPrivate = configuration.boolean("bot.chatsAnyPrivate").getOrElse(false)
    private val chatsAnyGroup = configuration.boolean("bot.chatsAnyGroup").getOrElse(false)
    private val startTime = executionStart / 1000

    override val proxy: Option[(String, Int, java.net.Proxy.Type)] = {
      for {
        host <- configuration.string("bot.proxy.host")
        port <- configuration.int("bot.proxy.port")
        typeString <- configuration.string("bot.proxy.type")
      } yield (host, port, java.net.Proxy.Type.valueOf(typeString.toUpperCase))
    }

    override def filterChat(message: Message): FilterChat = {
      if (message.date < startTime) {
        FilterChat(false, false)
      } else {
        val soft = chats.map(_.id).contains(message.chat.id) ||
          chatsAnyPrivate && Set("private").contains(message.chat.`type`) ||
          chatsAnyGroup && Set("group", "supergroup").contains(message.chat.`type`)
        FilterChat(soft, true)
      }
    }

    override def handleException(e: Throwable, causalMessage: Option[Message]): Unit = {
      e.printStackTrace()
      causalMessage.foreach { causalMessage =>
        botOwner.map { botOwner =>
          request(ForwardMessage(Left(botOwner), Left(causalMessage.chat.id),
            Some(true), causalMessage.messageId)).flatMap { sentMessage =>
            val writer = new java.io.StringWriter
            e.printStackTrace(new java.io.PrintWriter(writer))
            request(SendMessage(Left(botOwner), "```\n" + writer.toString + "\n```", Some(ParseMode.Markdown),
              Some(true), Some(true), Some(sentMessage.messageId)))
          }.recover {
            case e => e.printStackTrace()
          }
        }
      }
    }
  }

  // noinspection ScalaDeprecation
  java.net.URL.setURLStreamHandlerFactory(new okhttp3.OkUrlFactory(new okhttp3.OkHttpClient))

  ShikigamiBot.run()
}
