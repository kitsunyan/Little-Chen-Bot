package nya.kitsunyan.littlechenbot.booru

object GelbooruService extends BooruService {
  override val iqdbId: String = "4"

  override def filterUrl(url: String): Boolean = url.contains("//gelbooru.com")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String])] = {
    "(?s)<!\\[CDATA\\[.*?(.*?)\\};.*?//\\]\\]>".r.findFirstMatchIn(html) match {
      case Some(m) =>
        val map = (for {
          data <- "'(.*?)': ?(?:'(.*?)'|(\\w+))".r.findAllIn(m.subgroups.head).matchData
        } yield data.subgroups.head -> data.subgroups.tail).map { case (key, values) =>
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
      case None => None
    }
  }
}
