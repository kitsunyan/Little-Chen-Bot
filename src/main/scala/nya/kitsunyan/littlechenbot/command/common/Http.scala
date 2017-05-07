package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util.UserMessageException

import okhttp3._

import java.util.concurrent.TimeUnit

import scala.language.implicitConversions

trait Http {
  val proxy: Option[java.net.Proxy]

  private lazy val client = new OkHttpClient.Builder()
    .connectTimeout(10000, TimeUnit.MILLISECONDS)
    .readTimeout(10000, TimeUnit.MILLISECONDS)
    .build

  private lazy val proxyClient = proxy.map(client.newBuilder().proxy(_).build).getOrElse(client)

  type HttpFilter = (String, Boolean, HttpExecution) => Unit

  class HttpException(override val userMessage: Option[String], message: String) extends Exception(message)
    with UserMessageException

  object HttpFilters {
    def any: HttpFilter = (_, _, _) => ()

    def ok: HttpFilter = code(200)

    def code(validCodes: Int*): HttpFilter = {
      (url, privateUrl, execution) => {
        if (!validCodes.contains(execution.code)) {
          throwWithPrivateUrl(url, privateUrl, s"Invalid response: [${execution.code}]")
        }
      }
    }

    def contentLength(maxContentLength: Long): HttpFilter = {
      (url, privateUrl, execution) => {
        execution.headers("Content-Length")
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
      (url, privateUrl, execution) => {
        filter(url, privateUrl, execution)
        anotherFilter(url, privateUrl, execution)
      }
    }
  }

  implicit def extendedHttpFilter(filter: HttpFilter): ExtendedHttpFilter = {
    new ExtendedHttpFilter(filter)
  }

  def http(url: String, proxy: Boolean = false): HttpRequest = {
    new HttpRequest(new Request.Builder().url(url), proxy = proxy)
      .header("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:53.0) Gecko/20100101 Firefox/53.0")
  }

  private[Http] trait BodyAppend {
    def form(body: FormBody.Builder): FormBody.Builder
    def multipart(body: MultipartBody.Builder): MultipartBody.Builder
  }

  private[Http] class StringBodyAppend(name: String, value: String) extends BodyAppend {
    override def form(body: FormBody.Builder): FormBody.Builder = {
      body.add(name, value)
    }

    override def multipart(body: MultipartBody.Builder): MultipartBody.Builder = {
      body.addFormDataPart(name, value)
    }
  }

  private[Http] class FileBodyAppend(multipartFile: MultipartFile) extends BodyAppend {
    override def form(body: FormBody.Builder): FormBody.Builder = {
      throw new UnsupportedOperationException
    }

    override def multipart(body: MultipartBody.Builder): MultipartBody.Builder = {
      body.addFormDataPart(multipartFile.name, multipartFile.filename,
        RequestBody.create(MediaType.parse(multipartFile.mimeType), multipartFile.data))
    }
  }

  case class MultipartFile(name: String, filename: String, mimeType: String, data: Array[Byte])

  class HttpRequest(builder: Request.Builder, proxy: Boolean = false, privateUrl: Boolean = false,
    multipart: Boolean = false, fields: List[BodyAppend] = Nil) {
    private def copy(builder: Request.Builder = builder, proxy: Boolean = proxy, privateUrl: Boolean = privateUrl,
      multipart: Boolean = multipart, fields: List[BodyAppend] = fields): HttpRequest = {
      new HttpRequest(builder, proxy, privateUrl, multipart, fields)
    }

    def header(name: String, value: String): HttpRequest = {
      copy(builder = builder.header(name, value))
    }

    def withPrivateUrl(privateUrl: Boolean): HttpRequest = {
      copy(privateUrl = privateUrl)
    }

    def file(multipartFile: MultipartFile): HttpRequest = {
      copy(multipart = true, fields = new FileBodyAppend(multipartFile) :: fields)
    }

    def field(field: (String, String)): HttpRequest = {
      fields(List(field))
    }

    def fields(fields: Iterable[(String, String)]): HttpRequest = {
      copy(fields = fields.foldLeft(this.fields) { case (fields, (name, value)) =>
        new StringBodyAppend(name, value) :: fields
      })
    }

    private def run[T, R](filter: HttpFilter, convert: ResponseBody => T, callback: HttpResponse[T] => R): R = {
      val request = {
        if (fields.nonEmpty) {
          val body = if (multipart) {
            fields.foldRight(new MultipartBody.Builder().setType(MultipartBody.FORM))(_.multipart(_)).build
          } else {
            fields.foldRight(new FormBody.Builder)(_.form(_)).build
          }
          builder.post(body).build
        } else {
          builder.build
        }
      }

      val response = (if (proxy) proxyClient else client).newCall(request).execute()
      val execution = new HttpExecution(response)
      try {
        filter(request.url().toString, privateUrl, execution)
        callback(new HttpResponse(convert(response.body), execution))
      } catch {
        case e: Exception =>
          response.close()
          throw e
      }
    }

    def runString[R](filter: HttpFilter)(callback: HttpResponse[String] => R): R = {
      run(filter, _.string, callback)
    }

    def runBytes[R](filter: HttpFilter)(callback: HttpResponse[Array[Byte]] => R): R = {
      run(filter, _.bytes, callback)
    }
  }

  class HttpExecution(response: Response) {
    def headers(name: String): List[String] = {
      import scala.collection.JavaConverters._
      response.headers(name).asScala.toList
    }

    def code: Int = response.code
  }

  class HttpResponse[T](val body: T, execution: HttpExecution) {
    def headers(name: String): List[String] = execution.headers(name)

    def code: Int = execution.code
  }
}
