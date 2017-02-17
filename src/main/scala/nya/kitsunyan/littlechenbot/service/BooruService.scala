package nya.kitsunyan.littlechenbot.service

import scala.util.matching.Regex

sealed trait BooruService {
  val iqdbId: String
  def filterUrl(url: String): Boolean
  def parseHtml(html: String): Option[(String, Set[String], Set[String])]

  protected def collectSet(content: String, regex: Regex): Set[String] = {
    regex.findAllIn(content).matchData.map(_.subgroups.head).toSet
  }
}

object BooruService {
  val list: List[BooruService] = List(DanbooruService, GelbooruService)
}

object DanbooruService extends BooruService {
  override val iqdbId: String = "1"

  override def filterUrl(url: String): Boolean = url.contains("//danbooru.donmai.us")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String])] = {
    "data-file-url=\"(.*?)\"".r.findAllIn(html).matchData.map(_.subgroups.head).toList match {
      case _ :+ imageUrl =>
        val url = imageUrl match {
          case i if i.startsWith("//") => "https:" + i
          case i if i.startsWith("/") => "https://danbooru.donmai.us" + i
          case i => i
        }
        val characters = collectSet(html, "<li class=\"category-4\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r)
        val artists = collectSet(html, "<li class=\"category-1\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r)
        Some(url, characters, artists)
      case _ => None
    }
  }
}

object GelbooruService extends BooruService {
  override val iqdbId: String = "4"

  override def filterUrl(url: String): Boolean = url.contains("//gelbooru.com")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String])] = {
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
        val characters = collectSet(html, "<li class=\"tag-type-character\"><a .*?page=post.*?>(.*?)</a>".r)
        val artists = collectSet(html, "<li class=\"tag-type-artist\"><a .*?page=post.*?>(.*?)</a>".r)
        Some(url, characters, artists)
      } catch {
        case _: NoSuchElementException => None
      }
    }
  }
}
