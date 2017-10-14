package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.database.IqdbConfigurationData
import nya.kitsunyan.littlechenbot.service.BooruService
import nya.kitsunyan.littlechenbot.util._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait IqdbCommand extends Command with ExtractImage {
  this: Http =>

  private val commands = List("iqdb")

  override def prependDescription(list: List[Description], locale: Locale): List[Description] = {
    super.prependDescription(Description(commands, locale.FIND_IMAGE_WITH_IQDB_FD) :: list, locale)
  }

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    filterMessage(message, commands, handleMessageInternal(_, _, _), super.handleMessage, filterChat, _.soft)
  }

  private def handleMessageInternal(implicit message: Message, arguments: Arguments, locale: Locale): Future[Status] = {
    case class IqdbResult(index: Int, url: String, previewUrl: Option[String], blurMode: Utils.BlurMode,
      booruService: BooruService, alias: Option[String], similarity: Int, matches: Boolean)

    def sendIqdbRequest(minSimilarity: Int)(typedFile: TypedFile): Future[List[IqdbResult]] = {
      http("https://iqdb.org/")
        .file(typedFile.multipart("file"))
        .fields(BooruService.list.map("service[]" -> _.iqdbId)).runString(Http.Filters.ok).map { response =>
        val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
          "(?:<img src='(.*?)'.*?)?(?:<td>\\d+×\\d+ \\[(\\w+)\\]</td>.*?)?<td>(\\d+)% similarity</td>.*?</table>").r
        val linkPattern = "<a href=\"(.*?)\">".r

        case class Result(url: String, previewUrl: Option[String], blurMode: Utils.BlurMode,
          booruService: BooruService, similarity: Int, matches: Boolean)

        val results = for {
          table <- tablePattern.findAllIn(response.body).matchData
          similarity = table.group(3).toInt
          previewUrl = Option(table.group(1)).map {
            case i if i.startsWith("//") => "https:" + i
            case i if i.startsWith("/") => "https://iqdb.org" + i
            case i => i
          }
          links <- linkPattern.findAllIn(table.group(0)).matchData.map(_.subgroups)
          url = links.head match {
            case s if s.startsWith("//") => "https:" + s
            case s => s
          }
          matches = similarity >= minSimilarity
          booruService <- BooruService.findByUrl(url)
          blurMode = Option(table.group(2)).flatMap {
            case "Safe" => Some(Utils.BlurMode.No)
            case "Ero" => Some(Utils.BlurMode.Soft)
            case _ => None
          }.getOrElse(Utils.BlurMode.Hard)
        } yield Result(url, previewUrl, blurMode, booruService, similarity, matches)

        results.foldLeft[List[IqdbResult]](Nil) { (list, result) =>
          IqdbResult(list.length, result.url, result.previewUrl, result.blurMode,
            result.booruService, None, result.similarity, result.matches) :: list
        }.reverse
      }
    }

    def applyPriority(priority: List[String])(iqdbResults: List[IqdbResult]): List[IqdbResult] = {
      // Priority equality depends on BooruService equality
      case class Priority(booruService: BooruService)(val alias: String)

      val distinctPriority = priority.flatMap(p => BooruService.findByName(p).map(Priority(_)(p))).distinct
      val priorityMap = distinctPriority.map(p => p.booruService -> p.alias).toMap
      val servicesPriority = distinctPriority.map(_.booruService)

      iqdbResults.sortWith { (a, b) =>
        val aindex = servicesPriority.indexOf(a.booruService)
        val bindex = servicesPriority.indexOf(b.booruService)
        if (aindex >= 0 && bindex >= 0) {
          if (aindex == bindex) {
            a.similarity > b.similarity
          } else {
            aindex < bindex
          }
        } else if (aindex >= 0 || bindex >= 0) {
          aindex >= 0
        } else {
          a.similarity > b.similarity
        }
      }.map(r => r.copy(alias = priorityMap.get(r.booruService)))
    }

    def filterByIndex(index: Option[Int])(iqdbResults: List[IqdbResult]): List[IqdbResult] = {
      index.map(_ - 1).map(i => iqdbResults.find(_.index == i))
        .map(_.map(_.copy(matches = true)).map(List(_)).getOrElse(Nil))
        .getOrElse(iqdbResults)
    }

    case class ImageData(url: String)(val pageUrlFunction: () => String, val tags: List[BooruService.Tag])

    case class ReadImageData(name: String, image: Array[Byte])(pageUrlFunction: () => String,
      val tags: List[BooruService.Tag]) {
      def pageUrl: String = pageUrlFunction()
    }

    case class PreviewBlank(index: Int, url: Option[String], blurMode: Utils.BlurMode)

    def readBooruPage(iqdbResult: IqdbResult): Future[ImageData] = {
      val pageUrl = iqdbResult.url
      val pageUrlFunction = () => iqdbResult.alias
        .map(iqdbResult.booruService.replaceDomain(pageUrl, _))
        .getOrElse(pageUrl)

      http(pageUrl, proxy = true).runString(Http.Filters.ok).map { response =>
        iqdbResult.booruService.parseHtml(response.body) match {
          case Some((url, tags)) => ImageData(url)(pageUrlFunction, tags)
          case None => throw new CommandException(s"${locale.NOT_PARSED_FS}: $pageUrl.")
        }
      }
    }

    def readBooruImage(imageData: ImageData): Future[ReadImageData] = {
      http(imageData.url, proxy = true)
        .runBytes(Http.Filters.ok && Http.Filters.contentLength(10 * 1024 * 1024))
        .map(response => ReadImageData(Utils.extractNameFromUrl(imageData.url),
          response.body)(imageData.pageUrlFunction, imageData.tags))
    }

    def readBooruImages(messageWithImage: Message, query: Boolean)
      (iqdbResults: List[IqdbResult]): Future[ReadImageData] = {
      case class Result(successImageData: Option[ReadImageData] = None,
        additionalIqdbResults: List[IqdbResult] = Nil, exception: Option[Throwable] = None)

      def handleResult(iqdbResult: IqdbResult)(result: Result): Future[Result] = {
        if (result.successImageData.isEmpty && iqdbResult.matches && !query) {
          readBooruPage(iqdbResult)
            .flatMap(readBooruImage)
            .map(i => result.copy(successImageData = Some(i)))
            .recover((e: Throwable) => result.copy(exception = result.exception orElse Some(e)))
        } else {
          Future.successful(result.copy(additionalIqdbResults = iqdbResult :: result.additionalIqdbResults))
        }
      }

      iqdbResults
        .foldLeft(Future.successful(Result()))((r, i) => r.flatMap(handleResult(i)))
        .map(result => result.successImageData.getOrElse {
        val notFoundMessage = result.exception
          .map(_ => locale.NO_IMAGES_FOUND_DUE_TO_EXCEPTION_THROWN_FS)
          .getOrElse(locale.NO_IMAGES_FOUND_FS)

        val (additionalResults, previewBlanks) = result.additionalIqdbResults.reverse.map { iqdbResult =>
          val index = iqdbResult.index + 1
          val serviceName = iqdbResult.booruService.displayName
          val similarity = iqdbResult.similarity
          (s"$index: $similarity% — $serviceName", index, iqdbResult.previewUrl, iqdbResult.blurMode)
        }.foldRight("", List[PreviewBlank]()) { case ((text, index, previewUrl, blurMode), (results, previewBlanks)) =>
          val newResult = if (results.isEmpty) text else s"$text\n$results"
          val newPreviewBlanks = PreviewBlank(index, previewUrl, blurMode) :: previewBlanks
          (newResult, newPreviewBlanks)
        }

        val messageTextFuture = if (additionalResults.isEmpty) {
          Future.successful(s"$notFoundMessage.")
        } else {
          storeMessageToWorkspace(messageWithImage).map { requestIdString =>
            val insert = requestIdString.map(" " + _).getOrElse("")
            if (query) {
              s"${locale.RESULTS_FS}$insert:\n$additionalResults"
            } else {
              s"$notFoundMessage$insert.\n\n${locale.ADDITIONAL_RESULTS_FS}:\n$additionalResults"
            }
          }
        }

        throw (if (additionalResults.isEmpty) result.exception else None).getOrElse {
          result.exception.map(handleException(Some(messageWithImage)))
          new RecoverException(messageTextFuture
            .flatMap(replyWithQueryList(messageWithImage, _, previewBlanks))
            .statusMap(if (query) Status.Success else Status.Fail))
        }
      })
    }

    def collectTags(short: Boolean, tags: List[BooruService.Tag]): Option[String] = {
      def appendIterable(title: String, skip: Boolean, list: Iterable[BooruService.Tag],
        filter: BooruService.Tag => Boolean)(s: String): String = {
        val filteredList = list.filter(filter)
        if (!skip && filteredList.nonEmpty) {
          val tags = (if (short) filteredList.take(3) else filteredList)
            .map(_.title).reduce(_ + ", " + _)
          val formatted = if (short) s"$title: $tags" else s"$title:\n$tags"
          if (s.isEmpty) {
            formatted
          } else if (short) {
            s"$s\n$formatted"
          } else {
            s"$s\n\n$formatted"
          }
        } else {
          s
        }
      }

      Some("")
        .map(appendIterable(locale.CHARACTERS_FS, false, tags, _.character))
        .map(appendIterable(locale.COPYRIGTHS_FS, false, tags, _.copyright))
        .map(appendIterable(locale.ARTISTS_FS, false, tags, _.artist))
        .map(appendIterable(locale.TAGS_FS, short, tags, _.other))
        .filter(_.nonEmpty)
    }

    def replyWithImage(displayTags: Boolean)(imageData: ReadImageData): Future[(ReadImageData, Message)] = {
      val captionOption = (if (displayTags) collectTags(true, imageData.tags) else None)
        .map(imageData.pageUrl + "\n" + _) orElse Some(imageData.pageUrl)

      request(SendDocument(message.source, InputFile(imageData.name, imageData.image),
        replyToMessageId = Some(message.messageId), caption = captionOption))
        .map((imageData, _))
    }

    def replyWithTags(displayTags: Boolean)(imageData: ReadImageData, messageWithImage: Message):
      Future[(ReadImageData, Message)] = {
      (if (displayTags) collectTags(false, imageData.tags) else None)
        .map(replyQuote(_)(messageWithImage).map((imageData, _)))
        .getOrElse(Future.successful(imageData, messageWithImage))
    }

    def replyWithQueryList(messageWithImage: Message, messageText: String,
      previewBlanks: List[PreviewBlank]): Future[Message] = {
      if (previewBlanks.nonEmpty) {
        previewBlanks.map { previewBlank =>
          previewBlank.url.map { previewUrl =>
            http(previewUrl)
              .runBytes(Http.Filters.ok)
              .map(r => Utils.Preview(previewBlank.index, Some(r.body), "image/jpeg", previewBlank.blurMode))
              .recover((handleException(Some(messageWithImage))(_)) ->
                Utils.Preview(previewBlank.index, None, "", Utils.BlurMode.No))
          }.getOrElse(Future.successful(Utils.Preview(previewBlank.index, None, "", Utils.BlurMode.No)))
        }.foldRight[Future[List[Utils.Preview]]](Future.successful(Nil)) { (future, result) =>
          result.flatMap(list => future.map(_ :: list))
        }.flatMap { list =>
          val preview = Utils.drawPreview(list)
          preview.map { preview =>
            request(SendPhoto(message.source, InputFile("preview.png", preview),
              replyToMessageId = Some(message.messageId), caption = Some(trimCaption(messageText))))
          }.getOrElse(replyQuote(messageText))
        }
      } else {
        replyQuote(messageText)
      }
    }

    def storeMessageToWorkspace(messageWithImage: Message): Future[Option[String]] = {
      workspace.map { workspace =>
        request(ForwardMessage(workspace, messageWithImage.chat.id, None, messageWithImage.messageId))
          .map(_.messageId)
          .map(WorkspaceRequest(commands.head) _ andThen Some.apply)
          .recover((handleException(Some(message))(_)) -> None)
      }.getOrElse(Future.successful(None))
    }

    def extractMessageFromWorkspace(message: Message): Future[Option[Message]] = {
      workspace.map { workspace =>
        bot.flatMap { bot =>
          if ((message.forwardFrom orElse message.from).map(_.id.toLong).contains(bot.id)) {
            (message.text orElse message.caption)
              .flatMap(WorkspaceRequest.parse(commands.head))
              .map { messageId =>
              request(SendMessage(workspace, "query", replyToMessageId = Some(messageId)))
                .map(_.replyToMessage)
                .recover((handleException(Some(message))(_)) -> None)
            }.getOrElse(Future.successful(None))
          } else {
            Future.successful(None)
          }
        }
      }.getOrElse(Future.successful(None))
    }

    def configureItem(userId: Long, minSimilarity: Option[Int], priority: Option[List[String]], reset: Boolean)
      (item: Option[IqdbConfigurationData.Item]): (IqdbConfigurationData.Item, Boolean) = {
      val oldItem = (if (reset) None else item).getOrElse(IqdbConfigurationData.Item(userId, None, None))
      val newItem = oldItem.copy(minSimilarity = minSimilarity orElse oldItem.minSimilarity,
        priority = priority orElse oldItem.priority)
      (newItem, newItem != oldItem || item.exists(_ != newItem))
    }

    def storeConfiguration(item: IqdbConfigurationData.Item, update: Boolean): Future[IqdbConfigurationData.Item] = {
      if (update) {
        (item.minSimilarity orElse item.priority)
          .map(_ => IqdbConfigurationData.set(item))
          .getOrElse(IqdbConfigurationData.delete(item.userId))
          .map(_ => item)
      } else {
        Future.successful(item)
      }
    }

    def printConfiguration(item: IqdbConfigurationData.Item): Future[Message] = {
      val minSimilarity = item.minSimilarity.map(_.toString).getOrElse("_default_")
      val priority = item.priority.map(_.reduce(_ + ", " + _)).map(clearMarkup).getOrElse("_default_")
      replyQuote(s"${locale.CURRENT_CONFIGURATION_FS}:\n" +
        s"\n`--min-similarity`: $minSimilarity" +
        s"\n`--priority`: $priority", Some(ParseMode.Markdown))
    }

    def getConfiguration[T](argument: Option[T], callback: IqdbConfigurationData.Item => Option[T],
      default: T): Future[T] = {
      argument.map(Future.successful).getOrElse {
        message.from.map(_.id.toLong)
          .map(IqdbConfigurationData.get)
          .map(_.map(_.flatMap(callback).getOrElse(default)))
          .getOrElse(Future.successful(default))
      }
    }

    def withConfiguration[A, B, C](next: B => A => C, future: Future[B])(value: A): Future[C] = {
      future.map(next).map(_(value))
    }

    def withConfigurationFuture[A, B, C](next: B => A => Future[C], future: Future[B])(value: A): Future[C] = {
      future.map(next).flatMap(_(value))
    }

    val similarityOption = arguments("s", "min-similarity").asInt
      .map(s => if (s > 100) 100 else if (s < 0) 0 else s)

    val priorityOption = arguments("p", "priority").asStringList

    if (arguments("h", "help").nonEmpty) {
      checkArguments(arguments, "h", "help").unitFlatMap {
        replyMan(locale.FETCH_IMAGE_FROM_BOORU_USING_IQDB_ORG,
          (List("-i", "--index"), Some("integer"),
            locale.FETCH_IMAGE_BY_SPECIFIED_INDEX) ::
          (List("-q", "--query"), None,
            locale.QUERY_LIST_OF_IMAGES_WITHOUT_RESULT) ::
          (List("-t", "--tags"), None,
            locale.ATTACH_A_COMPLETE_LIST_OF_TAGS_TO_REPLY) ::
          (List("-s", "--min-similarity"), Some("0-100"),
            locale.SET_MINIMUM_ALLOWED_SIMILARITY_FOR_FOUND_IMAGES) ::
          (List("-p", "--priority"), Some("string list"),
            locale.SET_PRIORITY_FOR_BOORU_SERVICES) ::
          (List("-c", "--configure"), None,
            locale.SET_DEFAULT_ARGUMENTS_FOR_USER_FORMAT.format("`--priority`", "`--min-similarity`")) ::
          (List("--reset"), None,
            locale.RESET_ALL_DEFAULT_ARGUMENTS_FORMAT.format("`--configure`")) ::
          (List("--example"), None,
            locale.PRINT_EXAMPLES_OF_USAGE) ::
          (List("--list"), None,
            locale.PRINT_ALL_SUPPORTED_BOORU_SERVICES) ::
          (List("-h", "--help"), None,
            locale.DISPLAY_THIS_HELP) ::
          Nil)
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else if (arguments("example").nonEmpty) {
      checkArguments(arguments, "example").unitFlatMap {
        replyQuote(s"${locale.EXAMPLES_OF_USAGE_FS}:" +
          s"\n\n${locale.FETCH_IMAGE_BY_INDEX_FS}:" +
          "\n    `/iqdb --index 2`" +
          "\n    `/iqdb -i 2`" +
          s"\n\n${locale.QUERY_LIST_OF_IMAGES_FS}:" +
          "\n    `/iqdb --query`" +
          "\n    `/iqdb -q`" +
          s"\n\n${locale.QUERY_IMAGE_WITH_TAGS_FS}:" +
          "\n    `/iqdb --tags`" +
          "\n    `/iqdb -t`" +
          s"\n\n${locale.FETCH_WITH_SIMILARITY_50_FS}:" +
          "\n    `/iqdb --min-similarity 50`" +
          "\n    `/iqdb -s 50`" +
          s"\n\n${locale.FETCH_FROM_DANBOORU_IF_POSSIBLE_FS}:" +
          "\n    `/iqdb --priority danbooru`" +
          "\n    `/iqdb -p danbooru`" +
          s"\n\n${locale.FETCH_FROM_DANBOORU_OR_GELBOORU_IF_POSSIBLE_FS}:" +
          "\n    `/iqdb -p \"danbooru gelbooru\"`" +
          s"\n\n${locale.FETCH_FROM_DANBOORU_WITH_SIMILARITY_50_FS}:" +
          "\n    `/iqdb -p danbooru -s 50`" +
          s"\n\n${locale.VIEW_CONFIGURATION_FS}:" +
          "\n    `/iqdb --configure`" +
          "\n    `/iqdb -c`" +
          s"\n\n${locale.UPDATE_CONFIGURATION_FS}:" +
          "\n    `/iqdb -c -s 50`" +
          "\n    `/iqdb -c -p \"danbooru gelbooru\"`" +
          "\n    `/iqdb -c -s 40 -p danbooru`" +
          s"\n\n${locale.RESET_CONFIGURATION_FS}:" +
          "\n    `/iqdb -c --reset`" +
          "\n    `/iqdb -c --reset -s 50`",
          Some(ParseMode.Markdown))
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else if (arguments("list").nonEmpty) {
      checkArguments(arguments, "list").unitFlatMap {
        replyQuote(s"${locale.SUPPORTED_BOORU_SERVICES_FS}:" + BooruService.list.foldLeft(1, "\n") { case ((i, a), v) =>
          val primaryDomain = v.aliases.find(_.primaryDomain).get.name
          val aliases = v.aliases.filter(!_.primaryDomain).map(_.name).reduce(_ + "_, _" + _)
          val aliasesText = if (aliases.isEmpty) "" else s" (aliases: _${aliases}_)"
          (i + 1, s"$a\n$i: $primaryDomain$aliasesText")
        }._2, Some(ParseMode.Markdown))
      }.statusMap(Status.Success)
        .recoverWith(handleError(None)(message))
    } else if (arguments("c", "configure").nonEmpty) {
      val reset = arguments("reset").nonEmpty

      message.from.map(_.id.toLong).map { userId =>
        checkArguments(arguments, "c", "configure", "reset", "s", "min-similarity", "p", "priority")
          .unitFlatMap(IqdbConfigurationData.get(userId))
          .map(configureItem(userId, similarityOption, priorityOption, reset))
          .flatMap((storeConfiguration _).tupled).map(printConfiguration)
          .statusMap(Status.Success)
          .recoverWith(handleError(Some(locale.CONFIGURATION_HANDLING_FV_FS))(message))
      }.getOrElse(Future.successful(Status.Fail))
    } else {
      val indexOption = arguments("i", "index").asInt
      val query = arguments("q", "query").nonEmpty
      val tags = arguments("t", "tags").nonEmpty
      val similarity = getConfiguration(similarityOption, _.minSimilarity, 70)
      val priority = getConfiguration(priorityOption, _.priority, Nil)

      val messageWithImageFuture = message.replyToMessage
        .map(extractMessageFromWorkspace)
        .getOrElse(Future.successful(None))
        .map(_ orElse extractMessageWithImage)

      checkArguments(arguments, "i", "index", "q", "query", "t", "tags", "s", "min-similarity", "p", "priority")
        .unitFlatMap(messageWithImageFuture)
        .map(obtainMessageFile(commands.head))
        .scopeFlatMap((messageWithImage, file) => readTelegramFile(file)
          .flatMap(withConfigurationFuture(sendIqdbRequest, similarity))
          .flatMap(withConfiguration(applyPriority, priority))
          .map(filterByIndex(indexOption))
          .flatMap(readBooruImages(messageWithImage, query))
          .flatMap(replyWithImage(!tags))
          .flatMap((replyWithTags(tags) _).tupled)
          .statusMap(Status.Success))
        .recoverWith(message)(handleError(Some(locale.IMAGE_REQUEST_FV_FS)))
    }
  }
}
