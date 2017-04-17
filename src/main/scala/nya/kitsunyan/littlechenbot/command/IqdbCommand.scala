package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.Utils
import nya.kitsunyan.littlechenbot.command.common._
import nya.kitsunyan.littlechenbot.database.IqdbConfigurationData
import nya.kitsunyan.littlechenbot.service.BooruService

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.concurrent.Future

trait IqdbCommand extends Command with Describable with ExtractImage with Http {
  private val commands = List("iqdb")

  override def prependDescription(list: List[Description]): List[Description] = {
    super.prependDescription(Description(commands, "find image with iqdb") :: list)
  }

  override def handleMessage(filterChat: FilterChat)(implicit message: Message): Future[Any] = {
    filterMessage(commands, handleMessageInternal, super.handleMessage(filterChat), filterChat.soft)
  }

  private def handleMessageInternal(arguments: Arguments)(implicit message: Message): Future[Any] = {
    case class IqdbResult(index: Int, url: String, previewUrl: Option[String], booruService: BooruService,
      alias: Option[String], similarity: Int, matches: Boolean)

    def sendIqdbRequest(minSimilarity: Int)(telegramFile: TelegramFile): List[IqdbResult] = {
      http("https://iqdb.org/")
        .postMulti(telegramFile.multiPart("file"))
        .params(BooruService.list.map("service[]" -> _.iqdbId)).response(_.asString) { _ => body =>
        val tablePattern = ("<table><tr><th>(?:Best|Additional|Possible) match</th></tr><tr>.*?" +
          "(?:<img src='(.*?)'.*?)?<td>(\\d+)% similarity</td>.*?</table>").r
        val linkPattern = "<a href=\"(.*?)\">".r

        val result = for {
          table <- tablePattern.findAllIn(body).matchData
          similarity = table.group(2).toInt
          previewUrl = {
            Option(table.group(1)).map {
              case i if i.startsWith("//") => "https:" + i
              case i if i.startsWith("/") => "https://iqdb.org" + i
              case i => i
            }
          }
          links <- linkPattern.findAllIn(table.group(0)).matchData.map(_.subgroups)
          url = links.head match {
            case s if s.startsWith("//") => "https:" + s
            case s => s
          }
          matches = similarity >= minSimilarity
          booruService <- BooruService.findByUrl(url)
        } yield (url, previewUrl, booruService, similarity, matches)

        result.foldLeft[List[IqdbResult]](Nil) { case ((list), (url, previewUrl, booruService, similarity, matches)) =>
          IqdbResult(list.length, url, previewUrl, booruService, None, similarity, matches) :: list
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

    case class ImageData(url: String)(val pageUrlFunction: () => String, val tags: List[BooruService#Tag])

    case class ReadImageData(name: String, image: Array[Byte])(pageUrlFunction: () => String,
      val tags: List[BooruService#Tag]) {
      def pageUrl: String = pageUrlFunction()
    }

    def readBooruPage(iqdbResult: IqdbResult): ImageData = {
      val pageUrl = iqdbResult.url
      val pageUrlFunction = () => iqdbResult.alias
        .map(iqdbResult.booruService.replaceDomain(pageUrl, _))
        .getOrElse(pageUrl)

      http(pageUrl, proxy = true).response(_.asString) { _ => body =>
        iqdbResult.booruService.parseHtml(body) match {
          case Some((url, tags)) => ImageData(url)(pageUrlFunction, tags)
          case None => throw new Exception(s"Not parsed: $pageUrl.")
        }
      }
    }

    def readBooruImage(imageData: ImageData): ReadImageData = {
      http(imageData.url, proxy = true).response(_.asBytes) { _ => body =>
        val name = {
          val url = imageData.url
          val start = url.lastIndexOf('/') + 1
          val end = url.indexOf('?', start)
          if (end >= start) url.substring(start, end) else url.substring(start)
        }

        ReadImageData(name, body)(imageData.pageUrlFunction, imageData.tags)
      }
    }

    def readBooruImages(query: Boolean)(iqdbResults: List[IqdbResult]): ReadImageData = {
      case class Result(successImageData: Option[ReadImageData] = None,
        additionalIqdbResults: List[IqdbResult] = Nil, exception: Option[Exception] = None)

      val result = iqdbResults.foldLeft(Result()) { (result, iqdbResult) =>
        if (result.successImageData.isEmpty && iqdbResult.matches && !query) {
          try {
            result.copy(successImageData = Some(readBooruImage(readBooruPage(iqdbResult))))
          } catch {
            case e: Exception => result.copy(exception = result.exception orElse Some(e))
          }
        } else {
          result.copy(additionalIqdbResults = iqdbResult :: result.additionalIqdbResults)
        }
      }

      result.successImageData.getOrElse {
        val notFoundMessage = result.exception.map { e =>
          handleException(e, messageWithImageAsCausal)
          "No images found (due to exception thrown)."
        }.getOrElse("No images found.")

        val (additionalResults, previewUrls) = result.additionalIqdbResults.reverse.map { iqdbResult =>
          val index = iqdbResult.index + 1
          val serviceName = iqdbResult.booruService.displayName
          val similarity = iqdbResult.similarity
          (s"$index: $similarity% — $serviceName", index, iqdbResult.previewUrl)
        }.foldRight("", List[(Int, Option[String])]()) { case ((text, index, previewUrl), (results, previewUrls)) =>
          (if (results.isEmpty) text else s"$text\n$results", (index, previewUrl) :: previewUrls)
        }

        val messageText = if (additionalResults.isEmpty) {
          notFoundMessage
        } else if (query) {
          s"Results:\n$additionalResults"
        } else {
          s"$notFoundMessage\n\nAdditional results:\n$additionalResults"
        }

        throw (if (additionalResults.isEmpty) result.exception else None)
          .getOrElse(new RecoverException(replyWithQueryList(messageText, previewUrls)))
      }
    }

    def collectTags(short: Boolean, tags: List[BooruService#Tag]): Option[String] = {
      def appendIterable(title: String, skip: Boolean, list: Iterable[BooruService#Tag],
        filter: BooruService#Tag => Boolean)(s: String): String = {
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
        .map(appendIterable("Characters", false, tags, _.character))
        .map(appendIterable("Copyrights", false, tags, _.copyright))
        .map(appendIterable("Artists", false, tags, _.artist))
        .map(appendIterable("Tags", short, tags, _.other))
        .filter(_.nonEmpty)
    }

    def replyWithImage(displayTags: Boolean)(imageData: ReadImageData): Future[(ReadImageData, Message)] = {
      val captionOption = (if (displayTags) collectTags(true, imageData.tags) else None)
        .map(imageData.pageUrl + "\n" + _) orElse Some(imageData.pageUrl)

      request(SendDocument(Left(message.sender), Left(InputFile(imageData.name, imageData.image)),
        replyToMessageId = Some(message.messageId), caption = captionOption))
        .map((imageData, _))
    }

    def replyWithTags(displayTags: Boolean)(imageData: ReadImageData, messageWithImage: Message):
      Future[(ReadImageData, Message)] = {
      (if (displayTags) collectTags(false, imageData.tags) else None)
        .map(replyQuote(_)(messageWithImage).map((imageData, _)))
        .getOrElse(Future.successful(imageData, messageWithImage))
    }

    def replyWithQueryList(messageText: String, previewUrls: List[(Int, Option[String])]): Future[Message] = {
      if (previewUrls.nonEmpty) {
        previewUrls.map { case (index, previewUrl) =>
          previewUrl.map { previewUrl =>
            Future[(Int, Option[Array[Byte]], String)] {
              http(previewUrl).response(_.asBytes)(_ => body => (index, Some(body), "image/jpeg"))
            }.recover { case e =>
              handleException(e, messageWithImageAsCausal)
              (index, None, "")
            }
          }.getOrElse(Future.successful(index, None, ""))
        }.foldRight[Future[List[(Int, Option[Array[Byte]], String)]]](Future.successful(Nil)) { case (future, result) =>
          result.flatMap(list => future.map(_ :: list))
        }.flatMap { list =>
          val preview = Utils.drawPreview(list)
          preview.map { preview =>
            request(SendPhoto(Left(message.sender), Left(InputFile("preview.png", preview)),
              replyToMessageId = Some(message.messageId), caption = Some(messageText)))
          }.getOrElse(replyQuote(messageText))
        }
      } else {
        replyQuote(messageText)
      }
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
      val priority = item.priority.map(_.reduce(_ + ", " + _).replaceAll("[`*_\\[\\]()]", "")).getOrElse("_default_")
      replyQuote("Current configuration:\n" +
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

    val similarityOption = arguments.int("s", "min-similarity")
      .map(s => if (s > 100) 100 else if (s < 0) 0 else s)

    val priorityOption = arguments.string("p", "priority")
      .map(_.split(",|\\s+").toList.filter(!_.isEmpty))

    if (arguments.string("h", "help").nonEmpty) {
      replyMan("Fetch image from \\*booru using iqdb.org.",
        (List("-i", "--index"), Some("integer"),
          "Fetch image by specified index.") ::
        (List("-q", "--query"), None,
          "Query list of images without result.") ::
        (List("-t", "--tags"), None,
          "Attach a complete list of tags to reply.") ::
        (List("-s", "--min-similarity"), Some("0-100"),
          "Set minimum allowed similarity for found images.") ::
        (List("-p", "--priority"), Some("string list"),
          "Set priority for \\*booru services.") ::
        (List("-c", "--configure"), None,
          "Set default arguments for user. Specified `--priority` and `--min-similarity` " +
          "arguments will be stored as default.") ::
        (List("--reset"), None,
          "Reset all default arguments. Can be used with `--configure` argument only.") ::
        (List("--example"), None,
          "Print examples of usage.") ::
        (List("--list"), None,
          "Print all supported \\*booru services.") ::
        (List("-h", "--help"), None,
          "Display this help.") ::
        Nil)
    } else if (arguments.string(null, "example").nonEmpty) {
      replyQuote("Examples of usage:" +
        "\n\nFetch image by index:" +
        "\n    `/iqdb --index 2`" +
        "\n    `/iqdb -i 2`" +
        "\n\nQuery list of images:" +
        "\n    `/iqdb --query`" +
        "\n    `/iqdb -q`" +
        "\n\nQuery image with tags:" +
        "\n    `/iqdb --tags`" +
        "\n    `/iqdb -t`" +
        "\n\nFetch first image with similarity >= 50%:" +
        "\n    `/iqdb --min-similarity 50`" +
        "\n    `/iqdb -s 50`" +
        "\n\nFetch first image from danbooru if possible:" +
        "\n    `/iqdb --priority danbooru`" +
        "\n    `/iqdb -p danbooru`" +
        "\n\nFetch first image from danbooru or gelbooru if possible:" +
        "\n    `/iqdb -p \"danbooru gelbooru\"`" +
        "\n\nFetch first image from danbooru with similarity >= 50%:" +
        "\n    `/iqdb -p danbooru -s 50`" +
        "\n\nView configuration:" +
        "\n    `/iqdb --configure`" +
        "\n    `/iqdb -c`" +
        "\n\nUpdate configuration:" +
        "\n    `/iqdb -c -s 50`" +
        "\n    `/iqdb -c -p \"danbooru gelbooru\"`" +
        "\n    `/iqdb -c -s 40 -p danbooru`" +
        "\n\nReset configuration:" +
        "\n    `/iqdb -c --reset`" +
        "\n    `/iqdb -c --reset -s 50`",
        Some(ParseMode.Markdown))
    } else if (arguments.string(null, "list").nonEmpty) {
      replyQuote("Supported \\*booru services:" + BooruService.list.foldLeft(1, "\n") { case ((i, a), v) =>
        val primaryDomain = v.aliases.find(_.primaryDomain).get.name
        val aliases = v.aliases.filter(!_.primaryDomain).map(_.name).reduce(_ + "_, _" + _)
        val aliasesText = if (aliases.isEmpty) "" else s" (aliases: _${aliases}_)"
        (i + 1, s"$a\n$i: $primaryDomain$aliasesText")
      }._2, Some(ParseMode.Markdown))
    } else if (arguments.string("c", "configure").nonEmpty) {
      val reset = arguments.string(null, "reset")

      message.from.map(_.id.toLong).map { userId =>
        IqdbConfigurationData.get(userId)
          .map(configureItem(userId, similarityOption, priorityOption, reset.nonEmpty))
          .flatMap((storeConfiguration _).tupled).map(printConfiguration)
          .recoverWith(handleError(message, "configuration handling"))
      }.getOrElse(Future.unit)
    } else {
      val indexOption = arguments.int("i", "index")
      val query = arguments.string("q", "query").nonEmpty
      val tags = arguments.string("t", "tags").nonEmpty
      val similarity = getConfiguration(similarityOption, _.minSimilarity, 70)
      val priority = getConfiguration(priorityOption, _.priority, Nil)

      Future(obtainMessageFileId(commands.head, messageWithImage)).flatMap(readTelegramFile)
        .flatMap(withConfiguration(sendIqdbRequest, similarity))
        .flatMap(withConfiguration(applyPriority, priority))
        .map(filterByIndex(indexOption)).map(readBooruImages(query))
        .flatMap(replyWithImage(!tags)).flatMap((replyWithTags(tags) _).tupled)
        .recoverWith(handleError(messageWithImageAsCausal, "image request"))
    }
  }
}
