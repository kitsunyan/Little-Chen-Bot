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
  private case class HandleRunningGame(message: Message, replyToMessageId: Long,
    next: (Message, Locale, Long) => Option[Game] => Future[Option[Game]])
  private case object CleanupInstances

  private val actor = ActorSystem("GuessCommand").actorOf(Props(new GuessActor))
  private implicit val timeout: Timeout = Timeout(1, TimeUnit.DAYS)

  private class GuessActor extends Actor {
    private val instances = mutable.HashMap[Long, Instance]()

    def receive: Receive = {
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
      case HandleRunningGame(message, replyToMessageId, next) =>
        val futureOption = instances.find { case (_, instance) =>
          instance.messageIds.contains(replyToMessageId)
        }.map { case (key, instance) =>
          val newInstance = instance.copy(future = instance.future.flatMap(next(message, instance.locale, key)),
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

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    if (filterChat.soft) {
      (actor ? CleanupInstances).flatMap { _ =>
        message.initial.replyToMessage.map { replyToMessage =>
          (actor ? HandleRunningGame(message.initial, replyToMessage.messageId, handleRunningGame))
            .mapTo[Option[Future[Any]]]
        }.getOrElse(Future.successful(None)).flatMap(_
          .map(_ => Future.successful(Status.Cancel))
          .getOrElse(filterMessage(message, commands, handleMessageInternal(_, _, _),
            super.handleMessage, filterChat, _.soft)))
      }
    } else {
      super.handleMessage(message, filterChat)
    }
  }

  private def handleRunningGame(message: Message, locale: Locale, key: Long)(game: Option[Game]):
    Future[Option[Game]] = {
    implicit val messageImplicit: Message = message
    implicit val localeImplicit: Locale = locale

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

    case class BooruImage(name: String, data: Array[Byte])

    def readBooruImage(url: String): Future[Option[BooruImage]] = {
      http(url, proxy = true)
        .runBytes(Http.Filters.ok && Http.Filters.contentLength(10 * 1024 * 1024))
        .map { response =>
        val name = {
          val start = url.lastIndexOf('/') + 1
          val end = url.indexOf('?', start)
          if (end >= start) url.substring(start, end) else url.substring(start)
        }

        Some(BooruImage(name, response.body))
      }.recover((handleException(Some(message))(_)) -> None)
    }

    def replySuccessWithImage(game: Game): Future[Option[Nothing]] = {
      readBooruImage(game.url).flatMap { booruImageOption =>
        val characters = game.characters.reduce(_ + ", " + _)
        val text = s"${locale.A_WINNER_IS_YOU}\n\n${game.pageUrl}\n\n${locale.CHARACTERS_FS}: $characters"

        booruImageOption.map { booruImage =>
          request(SendPhoto(message.source, InputFile(booruImage.name, booruImage.data),
            replyToMessageId = Some(message.messageId), caption = Some(trimCaption(text))))
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

    game.map(game =>
      Future(parseMessage(game))
        .flatMap(replyOptional(game))
        .recoverWith(handleErrorInternal))
      .getOrElse(Future.successful(None))
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    def queryImages(tags: List[String]): Future[List[String]] = {
      val fullTags = ("-rating:explicit" ::
        "-rating:questionable" ::
        "-alternat*" ::
        "solo" ::
        tags.filter(!_.startsWith("rating:")))
        .distinct

      val url = "http://gelbooru.com/index.php?page=post&s=list&tags=" +
        java.net.URLEncoder.encode(fullTags.reduce(_ + " " + _), "UTF-8")

      http(url, proxy = true).runString(Http.Filters.ok).flatMap { response =>
        val maxPage = "<a href=\".*?pid=(\\d+)\">\\d+</a>".r
          .findAllIn(response.body).matchData.map(_.subgroups)
          .map(_.head.toInt).fold(0)(math.max)
        val page = Random.nextInt(maxPage + 1)

        val pageUrlsFuture = if (page > 0) {
          http(s"$url&pid=$page", proxy = true)
            .runString(Http.Filters.ok)
            .map(_.body)
            .map(BooruService.Gelbooru.parseListHtml)
        } else {
          Future.successful(Nil)
        }

        pageUrlsFuture.map { pageUrls =>
          if (pageUrls.nonEmpty) {
            pageUrls
          } else {
            BooruService.Gelbooru.parseListHtml(response.body)
          }
        }
      }.map { urls =>
        if (urls.nonEmpty) {
          urls
        } else {
          throw new CommandException(s"${locale.NO_IMAGES_FOUND_FS}.")
        }
      }
    }

    case class Image(url: String, pageUrl: String, tags: List[BooruService.Tag])

    def readRandomImage(tags: List[String])(urls: List[String]): Future[Image] = {
      case class Result(success: Option[Image] = None, exception: Option[Throwable] = None)

      val exclude = "solo" :: "1girl" :: "1boy" :: tags

      def handleResult(pageUrl: String)(result: Result): Future[Result] = {
        if (result.success.isEmpty) {
          http(pageUrl, proxy = true)
            .runString(Http.Filters.ok)
            .map { response =>
              val data = BooruService.Gelbooru.parseHtml(response.body).flatMap { image =>
                val workTags = image.tags.filter(t => !exclude.contains(t.title))
                if (workTags.exists(_.character) && workTags.exists(_.other)) {
                  Some(Image(image.url, pageUrl, workTags))
                } else {
                  None
                }
              }
              result.copy(success = data)
            }.recover((e: Throwable) => result.copy(exception = result.exception orElse Some(e)))
        } else {
          Future.successful(result)
        }
      }

      Random.shuffle(urls)
        .foldLeft(Future.successful(Result()))((r, u) => r.flatMap(handleResult(u)))
        .map(r => r.success.getOrElse(throw r.exception
          .getOrElse(new CommandException(s"${locale.NO_IMAGES_FOUND_FS}."))))
    }

    def createSession(image: Image): Future[Message] = {
      val startCount = 10
      val shuffledTags = Random.shuffle(image.tags.filter(_.other).map(_.title))
      val knownTags = shuffledTags.take(startCount)
      val hiddenTags = shuffledTags.drop(startCount)

      val game = Game(image.tags.filter(_.character).map(_.title), knownTags, hiddenTags, image.url, image.pageUrl)
      replyQuote(s"${locale.TAGS_FS}:\n\n" + game.knownTags.reduce(_ + "\n" + _))
        .flatMap(m => (actor ? CreateGame(m.messageId, game, locale)).map(_ => m))
    }

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, 0, "h", "help").unitFlatMap {
        replyMan(locale.A_SMALL_GAME_IN_GUESSING_A_CHARACTER,
          (List("-t", "--tags"), Some("string list"),
            locale.A_LIST_OF_TAGS_TO_PUZZLE_A_CHARACTER) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else {
      val tags = arguments("t", "tags").asStringList.getOrElse(Nil)

      checkArguments(arguments, 0, "t", "tags")
        .unitFlatMap(queryImages(tags))
        .flatMap(readRandomImage(tags))
        .flatMap(createSession)
        .statusMap(Status.Success)
        .recoverWith(handleError(Some(locale.CREATING_A_SESSION_FV_FS))(message))
    }
  }
}
