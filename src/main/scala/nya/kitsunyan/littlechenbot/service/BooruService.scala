package nya.kitsunyan.littlechenbot.service

sealed trait BooruService {
  val iqdbId: String
  def filterUrl(url: String): Boolean
  def parseHtml(html: String): Option[(String, Set[String], Set[String])]
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
        val characters = "<li class=\"category-4\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(matchData => matchData.subgroups.head).toSet
        val artists = "<li class=\"category-1\">.*?<a .*?class=\"search-.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(matchData => matchData.subgroups.head).toSet
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
        val url = map("domain") + "/" + map("base_dir") + "/" + map("dir") + "/" + map("img")
        val characters = "<li class=\"tag-type-character\"><a .*?page=post.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(matchData => matchData.subgroups.head).toSet
        val artists = "<li class=\"tag-type-artist\"><a .*?page=post.*?>(.*?)</a>".r
          .findAllIn(html).matchData.map(matchData => matchData.subgroups.head).toSet
        Some(url, characters, artists)
      } catch {
        case _: NoSuchElementException => None
      }
    }
  }
}
