package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.service.BooruService
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Random

trait GuessCommand extends Command {
  this: Http =>

  private val commands = List("guess")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.A_SMALL_GAME_IN_GUESSING_A_CHARACTER_FD) :: list, locale)
  }

  private case class Instance(future: Future[Option[Game]], messageIds: Set[Long],
    lastUpdateTime: Long, locale: Locale) {
    def notExpired: Boolean = System.currentTimeMillis <= lastUpdateTime + 5 * 60 * 1000
  }

  private case class Game(characters: List[String], knownTags: List[String], hiddenTags: List[String],
    url: String, pageUrl: String)

  private case class CreateGame(messageId: Long, game: Game, locale: Locale)
  private case class UpdateGame(key: Long, messageId: Long)
  private case class RemoveGame(key: Long)
  private case class HandleRunningGame(replyToMessageId: Long,
    next: (Long, Locale) => Option[Game] => Future[Option[Game]])
  private case object CleanupInstances

  private val actor = ActorSystem("GuessCommand").actorOf(Props(new GuessActor))
  private implicit val timeout = Timeout(1, TimeUnit.DAYS)

  private class GuessActor extends Actor {
    private val instances = mutable.HashMap[Long, Instance]()

    def receive: PartialFunction[Any, Unit] = {
      case CreateGame(messageId, game, locale) =>
        instances += messageId -> Instance(Future.successful(Some(game)), Set(messageId),
          System.currentTimeMillis, locale)
        sender ! {}
      case UpdateGame(key, messageId) =>
        instances.get(key).foreach { instance =>
          instances += key -> instance.copy(messageIds = instance.messageIds + messageId)
        }
        sender ! {}
      case RemoveGame(key) =>
        instances.remove(key)
        sender ! {}
      case HandleRunningGame(replyToMessageId, next) =>
        val futureOption = instances.find { case (_, instance) =>
          instance.messageIds.contains(replyToMessageId)
        }.map { case (key, instance) =>
          val newInstance = instance.copy(future = instance.future.flatMap(next(key, instance.locale)),
            lastUpdateTime = System.currentTimeMillis)
          instances += key -> newInstance
          newInstance.future
        }
        sender ! futureOption
      case CleanupInstances =>
        instances.retain { case (_, instance) =>
          instance.notExpired
        }
        sender ! {}
    }
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    if (filterChat.soft) {
      (actor ? CleanupInstances).flatMap { _ =>
        message.replyToMessage.map { replyToMessage =>
          (actor ? HandleRunningGame(replyToMessage.messageId, handleRunningGame)).mapTo[Option[Future[Any]]]
        }.getOrElse(Future.successful(None)).flatMap(_.getOrElse {
          filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
        })
      }
    } else {
      super.handleMessage(filterChat)
    }
  }

  private def handleRunningGame(key: Long, locale: Locale)(game: Option[Game])
    (implicit message: Message): Future[Option[Game]] = {
    implicit val localeImplicit = locale

    def parseMessage(game: Game): Boolean = {
      def transformText(text: String): Set[String] = {
        text.replace('_', ' ')
          .replaceAll("\\(.*?\\)", "")
          .split(" +")
          .toSet[String]
          .map(_.trim.toLowerCase)
          .filter(_.length >= 2)
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
        http(url, proxy = true)
          .runBytes(HttpFilters.ok && HttpFilters.contentLength(10 * 1024 * 1024)) { response =>
          val name = {
            val start = url.lastIndexOf('/') + 1
            val end = url.indexOf('?', start)
            if (end >= start) url.substring(start, end) else url.substring(start)
          }

          Some(name, response.body)
        }
      } catch {
        case e: Exception =>
          handleException(e, Some(message))
          None
      }
    }

    def replySuccessWithImage(game: Game): Future[Option[Nothing]] = {
      Future(readBooruImage(game.url)).flatMap { imageData =>
        val characters = game.characters.reduce(_ + ", " + _)
        val text = s"${locale.A_WINNER_IS_YOU}\n\n${game.pageUrl}\n\n${locale.CHARACTERS_FS}: $characters"

        imageData.map { case (name, image) =>
          request(SendPhoto(Left(message.sender), Left(InputFile(name, image)),
            replyToMessageId = Some(message.messageId), caption = Some(text)))
        }.getOrElse {
          replyQuote(text)
        }.flatMap(_ => actor ? RemoveGame(key))
          .map(_ => None)
      }
    }

    def replyFail(game: Game): Future[Some[Game]] = {
      val (tag, nextGame) = game.hiddenTags.headOption.map { tag =>
        (Some(tag), game.copy(knownTags = tag :: game.knownTags, hiddenTags = game.hiddenTags.tail))
      }.getOrElse(None, game)

      val fullText = tag
        .map(tag => s"${locale.YOU_ARE_WRONG}\n\n${locale.NEXT_TAG_FS}: $tag")
        .getOrElse(locale.YOU_ARE_WRONG)

      replyQuote(fullText)
        .flatMap(m => actor ? UpdateGame(key, m.messageId))
        .map(_ => Some(nextGame))
    }

    def replyOptional(game: Game)(success: Boolean): Future[Option[Game]] = {
      if (success) {
        replySuccessWithImage(game)
      } else {
        replyFail(game)
      }
    }

    val handleErrorInternal: PartialFunction[Throwable, Future[Option[Game]]] = {
      case e: Exception => handleErrorCommon(e, message, Some(locale.HANDLING_THE_SESSION_FV_FS)).map(_ => game)
    }

    game.map { game =>
      Future(parseMessage(game)).flatMap(replyOptional(game))
        .recoverWith(handleErrorInternal)
    }.getOrElse(Future.successful(None))
  }

  private def handleMessageInternal(arguments: Arguments, locale: Locale)(implicit message: Message): Future[Any] = {
    implicit val localeImplicit = locale

    def queryImages(tags: List[String]): List[String] = {
      val fullTags = ("-rating:explicit" ::
        "-rating:questionable" ::
        "-alternat*" ::
        "solo" ::
        tags.filter(!_.startsWith("rating:")))
        .distinct

      val url = "http://gelbooru.com/index.php?page=post&s=list&tags=" +
        java.net.URLEncoder.encode(fullTags.reduce(_ + " " + _), "UTF-8")

      http(url, proxy = true).runString(HttpFilters.ok) { response =>
        val maxPage = "<a href=\".*?pid=(\\d+)\">\\d+</a>".r
          .findAllIn(response.body).matchData.map(_.subgroups)
          .map(_.head.toInt).fold(0)(math.max)
        val page = Random.nextInt(maxPage + 1)

        val pageUrls = if (page > 0) {
          val newBody = http(s"$url&pid=$page", proxy = true).runString(HttpFilters.ok)(_.body)
          BooruService.Gelbooru.parseListHtml(newBody)
        } else {
          Nil
        }

        val urls = if (pageUrls.nonEmpty) pageUrls else BooruService.Gelbooru.parseListHtml(response.body)
        if (urls.nonEmpty) {
          urls
        } else {
          throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")
        }
      }
    }

    def readRandomImage(tags: List[String])(urls: List[String]): (String, String, List[BooruService.Tag]) = {
      case class Result(success: Option[(String, String, List[BooruService.Tag])] = None,
        exception: Option[Exception] = None)

      val exclude = "solo" :: "1girl" :: "1boy" :: tags

      val result = Random.shuffle(urls).foldLeft(Result()) { (result, pageUrl) =>
        if (result.success.isEmpty) {
          http(pageUrl, proxy = true).runString(HttpFilters.ok) { response =>
            try {
              val data = BooruService.Gelbooru.parseHtml(response.body).flatMap { case (url, tags) =>
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
        throw result.exception.getOrElse(new CommandException(s"${locale.NO_IMAGES_FOUND_FS}."))
      }
    }

    def createSession(url: String, pageUrl: String, tags: List[BooruService.Tag]): Future[Message] = {
      val startCount = 10
      val shuffledTags = Random.shuffle(tags.filter(_.other).map(_.title))
      val knownTags = shuffledTags.take(startCount)
      val hiddenTags = shuffledTags.drop(startCount)

      val game = Game(tags.filter(_.character).map(_.title), knownTags, hiddenTags, url, pageUrl)
      replyQuote(s"${locale.TAGS_FS}:\n\n" + game.knownTags.reduce(_ + "\n" + _))
        .flatMap(m => (actor ? CreateGame(m.messageId, game, locale)).map(_ => m))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.A_SMALL_GAME_IN_GUESSING_A_CHARACTER,
          (List("-t", "--tags"), Some("string list"),
            locale.A_LIST_OF_TAGS_TO_PUZZLE_A_CHARACTER) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.recoverWith(handleError(None)(message))
    } else {
      val tags = arguments("t", "tags").asString
        .map(_.split(",|\\s+").toList.filter(!_.isEmpty))
        .getOrElse(Nil)

      checkArguments(arguments, "t", "tags")
        .unitMap(queryImages(tags))
        .map(readRandomImage(tags))
        .flatMap((createSession _).tupled)
        .recoverWith(handleError(Some(locale.CREATING_A_SESSION_FV_FS))(message))
    }
  }
}
