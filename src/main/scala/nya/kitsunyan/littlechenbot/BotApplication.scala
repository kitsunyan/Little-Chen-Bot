package nya.kitsunyan.littlechenbot

import com.typesafe.config._

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import nya.kitsunyan.littlechenbot.command._

import scala.concurrent.Future
import scalaj.http._

object BotApplication extends App {
  private val config = ConfigFactory.parseFile(new java.io.File("littlechenbot.conf"))

  private def config[T](getter: String => T, key: String, fallback: T): T = {
    try {
      getter(key)
    } catch {
      case _: ConfigException => fallback
    }
  }

  object ShikigamiBot extends TelegramBot with Polling with IqdbCommand {
    override def token: String = config.getString("bot.token")
    override val botNickname: Future[String] = request(GetMe).map(_.username.getOrElse(""))

    private val chats = config(config.getLongList, "bot.chats", java.util.Collections.emptyList)
    private val chatsAnyPrivate = config(config.getBoolean, "bot.chatsAnyPrivate", false)
    private val chatsAnyGroup = config(config.getBoolean, "bot.chatsAnyGroup", false)
    private val startTime = System.currentTimeMillis / 1000

    override def filterChat(message: Message): Boolean = {
      message.date >= startTime && (chats.contains(message.chat.id) ||
        chatsAnyPrivate && Set("private").contains(message.chat.`type`) ||
        chatsAnyGroup && Set("group", "supergroup").contains(message.chat.`type`))
    }

    override def http(url: String, proxy: Boolean): HttpRequest = {
      Some(Http(url).timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
        .option(HttpOptions.followRedirects(true))).map { request =>
        if (proxy) request.proxy("localhost", 9050, java.net.Proxy.Type.SOCKS) else request
      }.get
    }
  }

  ShikigamiBot.run()
}
