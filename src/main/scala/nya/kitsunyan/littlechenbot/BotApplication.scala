package nya.kitsunyan.littlechenbot

import com.typesafe.config.ConfigFactory
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
import nya.kitsunyan.littlechenbot.booru._
import nya.kitsunyan.littlechenbot.util.Utils

import scala.concurrent.Future
import scalaj.http._

object BotApplication extends App {
  private val config = ConfigFactory.parseFile(new java.io.File("littlechenbot.conf"))

  object ShikigamiBot extends TelegramBot with Polling with Commands {
    override def token: String = config.getString("bot.token")
    private val chats = config.getLongList("bot.chats")
    private val startTime = System.currentTimeMillis / 1000

    private def http(url: String, proxy: Boolean = false): HttpRequest = {
      Some(Http(url).timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
        .option(HttpOptions.followRedirects(true))).map { request =>
        if (proxy) request.proxy("localhost", 9050, java.net.Proxy.Type.SOCKS) else request
      }.get
    }

    override def filterChat(message: Message): Boolean = message.date >= startTime && chats.contains(message.chat.id)

    private val booruServices = List(DanbooruService, GelbooruService)

    on(command("iqdb")) { implicit message =>
      (message.caption match {
        case Some(caption) => Some(message)
        case None => message.replyToMessage
      }) match {
        case Some(replyToMessage) =>
          Some(replyToMessage).flatMap { replyToMessage =>
            replyToMessage.photo match {
              case Some(photos) =>
                Some(photos.reduceLeft { (photo1, photo2) =>
                  if (photo2.fileSize.getOrElse(0) >= photo1.fileSize.getOrElse(0)) photo2 else photo1
                }.fileId)
              case None =>
                replyToMessage.sticker match {
                  case Some(sticker) => Some(sticker.fileId)
                  case None =>
                    replyToMessage.document match {
                      case Some(document) =>
                        document.mimeType match {
                          case Some(mimeType) if mimeType.startsWith("image/") => Some(document.fileId)
                          case None => None
                        }
                      case None => None
                    }
                }
            }
          } match {
            case Some(fileId) =>
              request(GetFile(fileId)).flatMap { file =>
                file.filePath match {
                  case Some(path) =>
                    Future {
                      val array = Some(http(s"https://api.telegram.org/file/bot$token/$path").asBytes.body)
                        .map { array =>
                        if (path.endsWith(".webp")) Utils.webpToPng(array) else array
                      }.get
                      val mimeType = if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg" else "image/png"
                      val response = http("https://iqdb.org/")
                        .postMulti(MultiPart("file", "filename", mimeType, array))
                        .params(booruServices.map("service[]" -> _.iqdbId)).asString
                      val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
                        "<td>(\\d+)% similarity</td>.*?</table>").r
                      val linkPattern = "<a href=\"(.*?)\">".r
                      (for {
                        table <- tablePattern.findAllIn(response.body).matchData
                        urls <- for {
                          link <- linkPattern.findAllIn(table.group(0)).matchData
                          url = (link.subgroups.head match {
                            case s if s.startsWith("//") => "https:" + s
                            case s => s
                          }, table.group(1).toInt)
                        } yield url
                      } yield urls).toList.filter(_._2 >= 70).map(_._1) match {
                        case url :: _ =>
                          try {
                            (for {
                              booruService <- booruServices
                              (filterResult, success, data, characters, artists) = if (booruService.filterUrl(url)) {
                                val response = http(url, proxy = true).asString
                                if (response.code == 200) {
                                  booruService.parseHtml(response.body) match {
                                    case Some((url, characters, artists)) => (true, true, url, characters, artists)
                                    case None => (true, false, s"Not parsed: $url.", null, null)
                                  }
                                } else {
                                  val code = response.code
                                  throw new Exception(s"Invalid response: $code.")
                                }
                              } else {
                                (false, false, null, null, null)
                              }
                              if filterResult
                            } yield (success, data, url, characters, artists)) match {
                              case List(result) => result
                              case _ => (false, "Unknown service.", null, null, null)
                            }
                          } catch {
                            case e: Exception =>
                              e.printStackTrace()
                              (false, "An exception was thrown during image request.", null, null, null)
                          }
                        case _ => (false, "No images found.", null, null, null)
                      }
                    }.flatMap { case (success, data, url, characters, artists) =>
                      if (success) {
                        Future {
                          try {
                            val response = http(data, proxy = true).asBytes
                            if (response.code == 200) {
                              (true, response.body, data)
                            } else {
                              val code = response.code
                              throw new Exception(s"Invalid response: $code.")
                            }
                          } catch {
                            case e: Exception =>
                              e.printStackTrace()
                              (false, null, "An exception was thrown during image request.")
                          }
                        }.flatMap { case (success, bytes, data) =>
                          if (success) {
                            val name = Some(data).map { text =>
                              if (text == null || text == "") "image.jpeg" else text
                            }.map { text =>
                              val index = text.lastIndexOf('/')
                              if (index >= 0) text.substring(index + 1) else text
                            }.map { text =>
                              val index = text.indexOf('?')
                              if (index >= 0) text.substring(0, index) else text
                            }.get
                            def appendIterable(title: String, list: Iterable[String])(s: String): String = {
                              if (list.nonEmpty) s + s"\n$title: " + list.reduceLeft(_ + ", " + _) else s
                            }
                            val captionOption = Some(url).map(appendIterable("Characters", characters))
                              .map(appendIterable("Artists", artists))
                            request(SendDocument(Left(message.sender), Left(InputFile(name, bytes)),
                              replyToMessageId = Some(message.messageId), caption = captionOption))
                          } else {
                            replyQuote(data)
                          }
                        }
                      } else {
                        replyQuote(data)
                      }
                    }
                  case None => Future { None }
                }
              }
            case _ => replyQuote("The message should contain an image.")
          }
        case None => replyQuote("Please reply to message with image.")
      }
    }
  }

  ShikigamiBot.run()
}
