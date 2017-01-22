package nya.kitsunyan.littlechenbot.booru

object GelbooruService extends BooruService {
  override val iqdbId: String = "4"

  override def filterUrl(url: String): Boolean = url.contains("//gelbooru.com")

  override def parseHtml(html: String): Option[String] = {
    "(?s)<!\\[CDATA\\[.*?(.*?)\\};.*?//\\]\\]>".r.findFirstMatchIn(html) match {
      case Some(m) =>
        val map = (for {
          data <- "'(.*?)': ?(?:'(.*?)'|(\\w+))".r.findAllIn(m.subgroups.head).matchData
        } yield data.subgroups.head -> data.subgroups.tail).map { case (key, values) =>
          if (values.head != null) (key, values.head) else (key, values.last)
        }.toMap
        try {
          Some(map("domain") + "/" + map("base_dir") + "/"
            + map("dir") + "/" + map("img"))
        } catch {
          case _: NoSuchElementException => None
        }
      case None => None
    }
  }
}
