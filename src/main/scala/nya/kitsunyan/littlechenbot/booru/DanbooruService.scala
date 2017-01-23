package nya.kitsunyan.littlechenbot.booru

object DanbooruService extends BooruService {
  override val iqdbId: String = "1"

  override def filterUrl(url: String): Boolean = url.contains("//danbooru.donmai.us")

  override def parseHtml(html: String): Option[(String, Set[String], Set[String])] = {
    (for {
      data <- "data-file-url=\"(.*?)\"".r.findAllIn(html).matchData
      url = data.subgroups.head
    } yield url).toList match {
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
