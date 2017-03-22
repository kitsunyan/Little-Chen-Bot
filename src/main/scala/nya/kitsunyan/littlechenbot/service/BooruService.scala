package nya.kitsunyan.littlechenbot.service

import scala.util.matching.Regex

sealed trait BooruService {
  val iqdbId: String
  val commonNames: List[String]
  def filterUrl(url: String): Boolean
  def parseHtml(html: String): Option[(String, Set[String], Set[String], Set[String])]

  protected def collectSet(content: String, regex: Regex): Set[String] = {
    regex.findAllIn(content).matchData.map(_.subgroups.head).toSet
  }
}

object BooruService {
  val list: List[BooruService] = List(DanbooruService, YandereService, GelbooruService)

  def findByUrl(url: String): Option[BooruService] = {
    list.find(_.filterUrl(url))
  }

  def findByName(name: String): Option[BooruService] = {
    list.find(_.commonNames.contains(name.toLowerCase))
  }
}

object DanbooruService extends BooruService {
  override val iqdbId: String = "1"
  override val commonNames: List[String] = List("danbooru", "danbooru.donmai.us")

  override def filterUrl(url: String): Boolean = url.contains("//danbooru.donmai.us")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String], Set[String])] = {
    "data-file-url=\"(.*?)\"".r.findAllIn(html).matchData.map(_.subgroups.head).toList match {
      case _ :+ imageUrl =>
        val url = imageUrl match {
          case i if i.startsWith("//") => "https:" + i
          case i if i.startsWith("/") => "https://danbooru.donmai.us" + i
          case i => i
        }
        val characters = collectSet(html, "<li class=\"category-4\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r)
        val copyrights = collectSet(html, "<li class=\"category-3\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r)
        val artists = collectSet(html, "<li class=\"category-1\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r)
        Some(url, characters, copyrights, artists)
      case _ => None
    }
  }
}

object YandereService extends BooruService {
  override val iqdbId: String = "3"
  override val commonNames: List[String] = List("yandere", "yande.re")

  override def filterUrl(url: String): Boolean = url.contains("//yande.re")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String], Set[String])] = {
    def matchOption(r: Regex): Option[String] = r.findAllIn(html).matchData.map(_.subgroups.head).toList.headOption
    def fixTag(s: String): String = s.replace('_', ' ')

    (matchOption("<a class=\"original-file-unchanged\" .*?href=\"(.*?)\"".r) orElse
      matchOption("<a class=\"original-file-changed\" .*?href=\"(.*?)\"".r)).map { imageUrl =>
      val characters = collectSet(html, "<li class=\".*?tag-type-character\" .*?data-name=\"(.*?)\"".r).map(fixTag)
      val copyrights = collectSet(html, "<li class=\".*?tag-type-copyright\" .*?data-name=\"(.*?)\"".r).map(fixTag)
      val artists = collectSet(html, "<li class=\".*?tag-type-artist\" .*?data-name=\"(.*?)\"".r).map(fixTag)
      (imageUrl, characters, copyrights, artists)
    }
  }
}

object GelbooruService extends BooruService {
  override val iqdbId: String = "4"
  override val commonNames: List[String] = List("gelbooru", "gelbooru.com")

  override def filterUrl(url: String): Boolean = url.contains("//gelbooru.com")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String], Set[String])] = {
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
        val copyrights = collectSet(html, "<li class=\"tag-type-copyright\"><a .*?page=post.*?>(.*?)</a>".r)
        val artists = collectSet(html, "<li class=\"tag-type-artist\"><a .*?page=post.*?>(.*?)</a>".r)
        Some(url, characters, copyrights, artists)
      } catch {
        case _: NoSuchElementException => None
      }
    }
  }
}
