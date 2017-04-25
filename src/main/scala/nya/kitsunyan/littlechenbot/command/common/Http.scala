package nya.kitsunyan.littlechenbot.command.common

import scalaj.http._

import scala.language.implicitConversions

trait Http {
  def http(url: String, proxy: Boolean = false): HttpRequest

  type Headers = Map[String, IndexedSeq[String]]
  type HttpFilter = (String, Int, Headers) => Unit

  object HttpFilters {
    def any: HttpFilter = (_, _, _) => ()

    def ok: HttpFilter = code(200)

    def code(validCodes: Int*): HttpFilter = {
      (url, code, _) => {
        if (!validCodes.contains(code)) {
          throw new Exception(s"Invalid response: [$code] $url.")
        }
      }
    }

    def contentLength(maxContentLength: Long): HttpFilter = {
      (url, _, headers) => {
        headers.get("Content-Length")
          .flatMap(_.headOption)
          .map(_.toLong)
          .find(_ > maxContentLength)
          .foreach { contentLength =>
          throw new Exception(s"Response is too large: [$contentLength] $url.")
        }
      }
    }
  }

  class ExtendedHttpFilter(filter: HttpFilter) {
    def &&(anotherFilter: HttpFilter): HttpFilter = {
      (url, code, header) => {
        filter(url, code, header)
        anotherFilter(url, code, header)
      }
    }
  }

  implicit def extendedHttpFilter(filter: HttpFilter): ExtendedHttpFilter = {
    new ExtendedHttpFilter(filter)
  }

  class ExtendedHttpRequest(request: HttpRequest) {
    def run[T, R](filter: HttpFilter)(callback: HttpResponse[T] => R)
      (parser: (Headers, java.io.InputStream) => T): R = {
      callback(request.exec { (code, headers, stream) =>
        filter(request.url, code, headers)
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
  }

  implicit def extendHttpRequest(request: HttpRequest): ExtendedHttpRequest = {
    new ExtendedHttpRequest(request)
  }
}
