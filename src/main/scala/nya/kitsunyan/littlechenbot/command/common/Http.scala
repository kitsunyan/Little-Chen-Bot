package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util.UserMessageException

import scalaj.http._

import scala.language.implicitConversions

trait Http {
  def http(url: String, proxy: Boolean = false): HttpRequest

  type Headers = Map[String, IndexedSeq[String]]
  type HttpFilter = (String, Boolean, Int, Headers) => Unit

  class HttpException(override val userMessage: Option[String], message: String) extends Exception(message)
    with UserMessageException

  object HttpFilters {
    def any: HttpFilter = (_, _, _, _) => ()

    def ok: HttpFilter = code(200)

    def code(validCodes: Int*): HttpFilter = {
      (url, privateUrl, code, _) => {
        if (!validCodes.contains(code)) {
          throwWithPrivateUrl(url, privateUrl, s"Invalid response: [$code]")
        }
      }
    }

    def contentLength(maxContentLength: Long): HttpFilter = {
      (url, privateUrl, _, headers) => {
        headers.get("Content-Length")
          .flatMap(_.headOption)
          .map(_.toLong)
          .find(_ > maxContentLength)
          .foreach(l => throwWithPrivateUrl(url, privateUrl, s"Response is too large: [$l]"))
      }
    }

    private def throwWithPrivateUrl(url: String, privateUrl: Boolean, commonMessage: String): Nothing = {
      val publicMessage = s"$commonMessage."
      val privateMessage = s"$commonMessage $url."
      throw new HttpException(Some(if (privateUrl) publicMessage else privateMessage), privateMessage)
    }
  }

  class ExtendedHttpFilter(filter: HttpFilter) {
    def &&(anotherFilter: HttpFilter): HttpFilter = {
      (url, privateUrl, code, header) => {
        filter(url, privateUrl, code, header)
        anotherFilter(url, privateUrl, code, header)
      }
    }
  }

  implicit def extendedHttpFilter(filter: HttpFilter): ExtendedHttpFilter = {
    new ExtendedHttpFilter(filter)
  }

  class ExtendedHttpRequest(request: HttpRequest, privateUrl: Boolean) {
    private def run[T, R](filter: HttpFilter)(callback: HttpResponse[T] => R)
      (parser: (Headers, java.io.InputStream) => T): R = {
      callback(request.exec { (code, headers, stream) =>
        filter(request.url, privateUrl, code, headers)
        parser(headers, stream)
      })
    }

    def runString[R](filter: HttpFilter)(callback: HttpResponse[String] => R): R = {
      run(filter)(callback) { (headers, stream) =>
        val charset = headers.get("Content-Type")
          .flatMap(_.headOption)
          .flatMap(c => HttpConstants.CharsetRegex.findFirstMatchIn(c).map(_.group(1)))
          .getOrElse(request.charset)
        HttpConstants.readString(stream, charset)
      }
    }

    def runBytes[R](filter: HttpFilter)(callback: HttpResponse[Array[Byte]] => R): R = {
      run(filter)(callback)((_, stream) => HttpConstants.readBytes(stream))
    }

    def withPrivateUrl(privateUrl: Boolean): ExtendedHttpRequest = {
      new ExtendedHttpRequest(request, privateUrl)
    }
  }

  implicit def extendHttpRequest(request: HttpRequest): ExtendedHttpRequest = {
    new ExtendedHttpRequest(request, false)
  }
}
