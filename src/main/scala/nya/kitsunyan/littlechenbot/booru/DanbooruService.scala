package nya.kitsunyan.littlechenbot.booru

object DanbooruService extends BooruService {
  override val iqdbId: String = "1"

  override def filterUrl(url: String): Boolean = url.contains("//danbooru.donmai.us")

  override def parseHtml(html: String): Option[String] = {
    (for {
      data <- "data-file-url=\"(.*?)\"".r.findAllIn(html).matchData
      url = data.subgroups.head
    } yield url).toList match {
      case _ :+ imageUrl =>
        Some(imageUrl match {
          case i if i.startsWith("//") => "https:" + i
          case i if i.startsWith("/") => "https://danbooru.donmai.us" + i
          case i => i
        })
      case _ => None
    }
  }
}
