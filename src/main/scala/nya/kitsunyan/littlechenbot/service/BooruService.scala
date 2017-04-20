package nya.kitsunyan.littlechenbot.service

import nya.kitsunyan.littlechenbot.Utils

import scala.util.matching.Regex

sealed trait BooruService {
  val iqdbId: String
  val aliases: List[Alias]
  def filterUrl(url: String): Boolean
  def parseHtml(html: String): Option[(String, List[Tag])]

  protected def collectSet(content: String, regex: Regex): Set[String] = {
    regex.findAllIn(content).matchData.map(_.subgroups.head).toSet
  }

  def displayName: String = {
    aliases.find(_.primaryName).get.name
  }

  def replaceDomain(url: String, alias: String): String = {
    aliases.filter(_.replaceDomain).map(_.name).find(_ == alias).flatMap { domain =>
      val index1 = url.indexOf("://")
      val index2 = if (index1 >= 0) url.indexOf('/', index1 + 3) else -1
      if (index2 >= index1) {
        Some(url.substring(0, index1 + 3) + domain + url.substring(index2))
      } else {
        None
      }
    }.getOrElse(url)
  }

  case class Tag(title: String)(val character: Boolean, val copyright: Boolean, val artist: Boolean) {
    def other: Boolean = !character && !copyright && !artist
  }

  case class Alias(name: String, primaryName: Boolean = false,
    primaryDomain: Boolean = false, replaceDomain: Boolean = false)
}

object BooruService {
  val list: List[BooruService] = List(DanbooruService, YandereService, GelbooruService)

  def findByUrl(url: String): Option[BooruService] = {
    list.find(_.filterUrl(url))
  }

  def findByName(name: String): Option[BooruService] = {
    list.find(_.aliases.map(_.name).contains(name.toLowerCase))
  }
}

object DanbooruService extends BooruService {
  override val iqdbId: String = "1"
  override val aliases: List[Alias] =
    Alias("danbooru", primaryName = true) ::
    Alias("danbooru.donmai.us", primaryDomain = true) ::
    Alias("hijiribe.donmai.us", replaceDomain = true) ::
    Nil

  override def filterUrl(url: String): Boolean = url.contains("//danbooru.donmai.us")

  override def parseHtml(html: String): Option[(String, List[Tag])] = {
    "data-file-url=\"(.*?)\"".r.findAllIn(html).matchData.map(_.subgroups.head).toList match {
      case _ :+ imageUrl =>
        val url = imageUrl match {
          case i if i.startsWith("//") => "https:" + i
          case i if i.startsWith("/") => "https://danbooru.donmai.us" + i
          case i => i
        }

        val tags = "<li class=\"category-(\\d+)\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(_.subgroups).map {
          case (category :: title :: Nil) =>
            Tag(Utils.unescapeHtml(title))(category == "4", category == "3", category == "1")
          case e => throw new MatchError(e)
        }.toList.distinct

        Some(url, tags)
      case _ => None
    }
  }
}

object YandereService extends BooruService {
  override val iqdbId: String = "3"
  override val aliases: List[Alias] =
    Alias("yandere", primaryName = true) ::
    Alias("yande.re", primaryDomain = true) ::
    Nil

  override def filterUrl(url: String): Boolean = url.contains("//yande.re")

  override def parseHtml(html: String): Option[(String, List[Tag])] = {
    def matchOption(r: Regex): Option[String] = r.findAllIn(html).matchData.map(_.subgroups.head).toList.headOption

    // Two regex because unchanged images have more priority
    (matchOption("<a class=\"original-file-unchanged\" .*?href=\"(.*?)\"".r) orElse
      matchOption("<a class=\"original-file-changed\" .*?href=\"(.*?)\"".r)).map { imageUrl =>
      val tags = "<li class=\".*?tag-type-(.*?)\" .*?data-name=\"(.*?)\"".r
        .findAllIn(html).matchData.map(_.subgroups).map {
        case (category :: title :: Nil) =>
          Tag(Utils.unescapeHtml(title).replace('_', ' '))(category == "character",
            category == "copyright", category == "artist")
        case e => throw new MatchError(e)
      }.toList.distinct

      (imageUrl, tags)
    }
  }
}

object GelbooruService extends BooruService {
  override val iqdbId: String = "4"
  override val aliases: List[Alias] =
    Alias("gelbooru", primaryName = true) ::
    Alias("gelbooru.com", primaryDomain = true) ::
    Nil

  override def filterUrl(url: String): Boolean = url.contains("//gelbooru.com")

  override def parseHtml(html: String): Option[(String, List[Tag])] = {
    "(?s)<!\\[CDATA\\[.*?(.*?)\\};.*?//\\]\\]>".r.findFirstMatchIn(html).flatMap { m =>
      val map = "'(.*?)': ?(?:'(.*?)'|(\\w+))".r.findAllIn(m.subgroups.head).matchData.map { data =>
        data.subgroups.head -> data.subgroups.tail
      }.map { case (key, values) =>
        if (values.head != null) (key, values.head) else (key, values.last)
      }.toMap
      try {
        val url = Some(map("domain") + "/" + map("base_dir") + "/" + map("dir") + "/" + map("img")).map { url =>
          if (url.startsWith("//")) "https:" + url else url
        }.get

        val tags = "<li class=\"tag-type-(.*?)\"><a .*?page=post.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(_.subgroups).map {
          case (category :: title :: Nil) =>
            Tag(Utils.unescapeHtml(title))(category == "character", category == "copyright", category == "artist")
          case e => throw new MatchError(e)
        }.toList.distinct

        Some(url, tags)
      } catch {
        case _: NoSuchElementException => None
      }
    }
  }

  def parseListHtml(html: String): List[String] = {
    "<a id=\"p\\d+\" href=\"(index.php?.*?id=\\d+)\".*?>".r
      .findAllIn(html).matchData.map(_.subgroups).map {
      case (url :: Nil) => s"http://gelbooru.com/${url.replace("&amp;", "&")}"
      case e => throw new MatchError(e)
    }.toList
  }
}
