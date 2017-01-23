package nya.kitsunyan.littlechenbot.booru

trait BooruService {
  val iqdbId: String
  def filterUrl(url: String): Boolean
  def parseHtml(html: String): Option[(String, Set[String], Set[String])]
}
