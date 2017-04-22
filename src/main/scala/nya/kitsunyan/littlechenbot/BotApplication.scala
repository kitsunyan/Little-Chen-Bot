package nya.kitsunyan.littlechenbot

import nya.kitsunyan.littlechenbot.command._

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import com.typesafe.config._

import java.io._

import scalaj.http._

import scala.concurrent.Future

object BotApplication extends App {
  private val config = ConfigFactory.parseFile(new java.io.File("littlechenbot.conf"))

  private def config[T](getter: Config => String => T, key: String): Option[T] = {
    try {
      Some(getter(config)(key))
    } catch {
      case _: ConfigException => None
    }
  }

  object ShikigamiBot extends TelegramBot with Polling with HelpCommand with IqdbCommand with RateCommand
    with GuessCommand with IdentityCommand {
    override def token: String = config.getString("bot.token")
    override val bot: Future[Bot] = request(GetMe).map(m => Bot(m.username.getOrElse(""), m.id))
    override val workspace: Option[Long] = config(_.getLong, "bot.workspace")

    private val botOwner = config(_.getLong, "bot.owner")
    private val chats = config(_.getLongList, "bot.chats").getOrElse(java.util.Collections.emptyList)
    private val chatsAnyPrivate = config(_.getBoolean, "bot.chatsAnyPrivate").getOrElse(false)
    private val chatsAnyGroup = config(_.getBoolean, "bot.chatsAnyGroup").getOrElse(false)
    private val startTime = executionStart / 1000

    private val proxy = {
      for {
        host <- config(_.getString, "bot.proxy.host")
        port <- config(_.getInt, "bot.proxy.port")
        typeString <- config(_.getString, "bot.proxy.type")
      } yield (host, port, java.net.Proxy.Type.valueOf(typeString.toUpperCase))
    }

    override def filterChat(message: Message): FilterChat = {
      if (message.date < startTime) {
        FilterChat(false, false)
      } else {
        val soft = chats.contains(message.chat.id) ||
          chatsAnyPrivate && Set("private").contains(message.chat.`type`) ||
          chatsAnyGroup && Set("group", "supergroup").contains(message.chat.`type`)
        FilterChat(soft, true)
      }
    }

    override def handleException(e: Throwable, causalMessage: Message): Unit = {
      e.printStackTrace()
      botOwner.map { botOwner =>
        request(ForwardMessage(Left(botOwner), Left(causalMessage.chat.id),
          Some(true), causalMessage.messageId)).flatMap { sentMessage =>
          val writer = new StringWriter()
          e.printStackTrace(new PrintWriter(writer))
          request(SendMessage(Left(botOwner), "```\n" + writer.toString + "\n```", Some(ParseMode.Markdown),
            Some(true), Some(true), Some(sentMessage.messageId)))
        }.recover {
          case e => e.printStackTrace()
        }
      }
    }

    override def http(url: String, proxy: Boolean): HttpRequest = {
      Some(Http(url).timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
        .option(HttpOptions.followRedirects(true))).map { request =>
        (if (proxy) ShikigamiBot.proxy else None)
          .map((request.proxy(_: String, _: Int, _: java.net.Proxy.Type)).tupled)
          .getOrElse(request)
      }.get
    }
  }

  ShikigamiBot.run()
}
