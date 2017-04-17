package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.service.GelbooruService

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Random

trait GuessCommand extends Command with Http {
  private val commands = List("guess")

  private case class Instance(future: Future[Option[Game]], messageIds: Set[Long], lastUpdateTime: Long) {
    def notExpired: Boolean = System.currentTimeMillis <= lastUpdateTime + 5 * 60 * 1000
  }

  private case class Game(characters: List[String], knownTags: List[String], hiddenTags: List[String],
    url: String, pageUrl: String)

  private val instances = mutable.HashMap[Long, Instance]()

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    if (filterChat.soft) {
      instances.synchronized {
        instances.retain { case (_, instance) =>
          instance.notExpired
        }
      }
      message.replyToMessage.flatMap { replyToMessage =>
        instances.synchronized {
          instances.find { case (_, instance) =>
            instance.messageIds.contains(replyToMessage.messageId)
          }.map { case (key, instance) =>
            val newInstance = instance.copy(future = instance.future.flatMap(handleRunningGame(key)),
              lastUpdateTime = System.currentTimeMillis)
            instances += key -> newInstance
            newInstance.future
          }
        }
      }.getOrElse(filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft))
    } else {
      super.handleMessage(filterChat)
    }
  }

  private def handleRunningGame(key: Long)(game: Option[Game])(implicit message: Message): Future[Option[Game]] = {
    def parseMessage(game: Game): Boolean = {
      def transformText(text: String): Set[String] = {
        text.replace('_', ' ').split(" +").toSet.filter(_.length >= 2).map(_.toLowerCase)
      }

      val text = (message.text orElse message.caption).getOrElse("")
      val set = transformText(text)

      game.characters.exists { character =>
        val characterSet = transformText(character)
        val diff = (set & characterSet).size
        diff * 3 / 2 >= characterSet.size && set.size * 2 <= characterSet.size * 3
      }
    }

    def readBooruImage(url: String): Option[(String, Array[Byte])] = {
      try {
        http(url, proxy = true).response(_.asBytes) { _ =>
          body =>
            val name = {
              val start = url.lastIndexOf('/') + 1
              val end = url.indexOf('?', start)
              if (end >= start) url.substring(start, end) else url.substring(start)
            }

            Some(name, body)
        }
      } catch {
        case e: Exception =>
          handleException(e, message)
          None
      }
    }

    def replySuccessWithImage(game: Game): Future[Option[Nothing]] = {
      Future(readBooruImage(game.url)).flatMap { imageData =>
        val characters = game.characters.reduce(_ + ", " + _)
        val text = s"A winner is you!\n\n${game.pageUrl}\n\nCharacters: $characters"

        imageData.map { case (name, image) =>
          request(SendPhoto(Left(message.sender), Left(InputFile(name, image)),
            replyToMessageId = Some(message.messageId), caption = Some(text)))
        }.getOrElse {
          replyQuote(text)
        }.map { _ =>
          instances.synchronized {
            instances.remove(key)
          }
          None
        }
      }
    }

    def replyFail(game: Game): Future[Some[Game]] = {
      val (tag, nextGame) = game.hiddenTags.headOption.map { tag =>
        (Some(tag), game.copy(knownTags = tag :: game.knownTags, hiddenTags = game.hiddenTags.tail))
      }.getOrElse(None, game)

      val text = "You are wrong!"
      val fullText = tag.map(tag => s"$text\n\nNext tag: $tag").getOrElse(text)

      replyQuote(fullText).map { resultMessage =>
        instances.synchronized {
          instances.get(key).foreach { instance =>
            instances += key -> instance.copy(messageIds = instance.messageIds + resultMessage.messageId)
          }
        }
        Some(nextGame)
      }
    }

    def replyOptional(game: Game)(success: Boolean): Future[Option[Game]] = {
      if (success) {
        replySuccessWithImage(game)
      } else {
        replyFail(game)
      }
    }

    val handleErrorInternal: PartialFunction[Throwable, Future[Option[Game]]] = {
      case e: Exception => handleErrorCommon(e, message, "handling the session").map(_ => game)
    }

    game.map { game =>
      Future(parseMessage(game)).flatMap(replyOptional(game))
        .recoverWith(handleErrorInternal)
    }.getOrElse(Future.successful(None))
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    def queryImages(tags: List[String]): List[String] = {
      val fullTags = ("-rating:explicit" ::
        "-rating:questionable" ::
        "-alternat*" ::
        "solo" ::
        tags.filter(!_.startsWith("rating:")))
        .distinct

      val url = "http://gelbooru.com/index.php?page=post&s=list&tags=" +
        java.net.URLEncoder.encode(fullTags.reduce(_ + " " + _), "UTF-8")

      http(url, proxy = true).response(_.asString) { _ => body =>
        val maxPage = "<a href=\".*?pid=(\\d+)\">\\d+</a>".r
          .findAllIn(body).matchData.map(_.subgroups)
          .map(_.head.toInt).fold(0)(math.max)
        val page = Random.nextInt(maxPage + 1)

        val pageUrls = if (page > 0) {
          val newBody = http(s"$url&pid=$page", proxy = true).response(_.asString)(_ => body => body)
          GelbooruService.parseListHtml(newBody)
        } else {
          Nil
        }

        val urls = if (pageUrls.nonEmpty) pageUrls else GelbooruService.parseListHtml(body)
        if (urls.nonEmpty) {
          urls
        } else {
          throw new CommandException("No images found.")
        }
      }
    }

    def readRandomImage(tags: List[String])(urls: List[String]): (String, String, List[GelbooruService.Tag]) = {
      case class Result(success: Option[(String, String, List[GelbooruService.Tag])] = None,
        exception: Option[Exception] = None)

      val exclude = "solo" :: "1girl" :: "1boy" :: tags

      val result = Random.shuffle(urls).foldLeft(Result()) { (result, pageUrl) =>
        if (result.success.isEmpty) {
          http(pageUrl, proxy = true).response(_.asString) { _ => body =>
            try {
              val data = GelbooruService.parseHtml(body).flatMap { case (url, tags) =>
                val workTags = tags.filter(t => !exclude.contains(t.title))
                if (workTags.exists(_.character) && workTags.exists(_.other)) {
                  Some(url, pageUrl, workTags)
                } else {
                  None
                }
              }
              result.copy(success = data)
            } catch {
              case e: Exception => result.copy(exception = result.exception orElse Some(e))
            }
          }
        } else {
          result
        }
      }

      result.success.getOrElse {
        throw result.exception.getOrElse(new CommandException("No images found."))
      }
    }

    def createSession(url: String, pageUrl: String, tags: List[GelbooruService.Tag]): Future[Message] = {
      val startCount = 10
      val shuffledTags = Random.shuffle(tags.filter(_.other).map(_.title))
      val knownTags = shuffledTags.take(startCount)
      val hiddenTags = shuffledTags.drop(startCount)

      val game = Game(tags.filter(_.character).map(_.title), knownTags, hiddenTags, url, pageUrl)
      replyQuote("Tags:\n\n" + game.knownTags.reduce(_ + "\n" + _)).map { resultMessage =>
        instances.synchronized {
          instances += resultMessage.messageId -> Instance(Future.successful(Some(game)),
            Set(resultMessage.messageId), System.currentTimeMillis)
        }
        resultMessage
      }
    }

    if (arguments.string("h", "help").nonEmpty) {
      replyMan("A small game in guessing a character by booru tags.",
        (List("-t", "--tags"), None,
          "A list of tags to puzzle a character.") ::
        Nil)
    } else {
      val tags = arguments.string("t", "tags")
        .map(_.split(",|\\s+").toList.filter(!_.isEmpty))
        .getOrElse(Nil)

      Future(queryImages(tags)).map(readRandomImage(tags)).flatMap((createSession _).tupled)
        .recoverWith(handleError(message, "creating a session"))
    }
  }
}
