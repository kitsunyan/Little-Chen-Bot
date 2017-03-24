package nya.kitsunyan.littlechenbot.command

import scala.language.implicitConversions
import scalaj.http.{HttpRequest, HttpResponse}

trait Http {
  def http(url: String, proxy: Boolean = false): HttpRequest

  class ExtendedHttpRequest(request: HttpRequest) {
    def response[T, R](read: HttpRequest => HttpResponse[T], validCodes: List[Int] = List(200))
      (callback: HttpResponse[T] => T => R): R = {
      val response = read(request)
      val code = response.code
      if (validCodes.contains(code)) {
        callback(response)(response.body)
      } else {
        val url = request.url
        throw new Exception(s"Invalid response: [$code] $url.")
      }
    }
  }

  implicit def extendHttpRequest(request: HttpRequest): ExtendedHttpRequest = {
    new ExtendedHttpRequest(request)
  }
}
