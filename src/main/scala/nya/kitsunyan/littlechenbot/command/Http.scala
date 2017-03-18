package nya.kitsunyan.littlechenbot.command

import scalaj.http.HttpRequest

trait Http {
  def http(url: String, proxy: Boolean = false): HttpRequest
}
