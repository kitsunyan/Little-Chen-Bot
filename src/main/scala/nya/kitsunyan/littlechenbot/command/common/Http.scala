package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util.UserMessageException

import info.mukel.telegrambot4s.api.AkkaImplicits

import okhttp3._

import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.language.implicitConversions

trait Http {
  this: AkkaImplicits =>

  val proxy: Option[java.net.Proxy]

  private lazy val client = new OkHttpClient.Builder()
    .connectTimeout(10000, TimeUnit.MILLISECONDS)
    .readTimeout(10000, TimeUnit.MILLISECONDS)
    .build

  private lazy val proxyClient = proxy.map(client.newBuilder().proxy(_).build).getOrElse(client)

  case class HttpFilter(filterHeaders: (String, Boolean, HttpExecution) => Unit,
    maxSize: Option[(Int, (String, Boolean) => Nothing)] = None)

  class HttpException(override val userMessage: Option[String], message: String) extends Exception(message)
    with UserMessageException

  object HttpFilters {
    def any: HttpFilter = HttpFilter((_, _, _) => ())

    def ok: HttpFilter = code(200)

    def code(validCodes: Int*): HttpFilter = {
      HttpFilter((url, privateUrl, execution) => {
        if (!validCodes.contains(execution.code)) {
          throwWithPrivateUrl(url, privateUrl, s"Invalid response: [${execution.code}]")
        }
      })
    }

    def contentLength(maxContentLength: Int): HttpFilter = {
      HttpFilter((url, privateUrl, execution) => {
        execution.headers("Content-Length")
          .headOption
          .map(_.toLong)
          .find(_ > maxContentLength)
          .foreach(l => throwWithPrivateUrl(url, privateUrl, s"Response is too large: [$l]"))
      }, Some(maxContentLength, (url, privateUrl) => throwWithPrivateUrl(url, privateUrl, s"Response is too large")))
    }

    private def throwWithPrivateUrl(url: String, privateUrl: Boolean, commonMessage: String): Nothing = {
      val publicMessage = s"$commonMessage."
      val privateMessage = s"$commonMessage $url."
      throw new HttpException(Some(if (privateUrl) publicMessage else privateMessage), privateMessage)
    }
  }

  class ExtendedHttpFilter(filter: HttpFilter) {
    def &&(anotherFilter: HttpFilter): HttpFilter = {
      val maxSize = (for {
        (size, error) <- filter.maxSize
        (anotherSize, anotherError) <- anotherFilter.maxSize
      } yield if (size < anotherSize) {
        (size, error)
      } else {
        (anotherSize, anotherError)
      }) orElse filter.maxSize orElse anotherFilter.maxSize

      HttpFilter((url, privateUrl, execution) => {
        filter.filterHeaders(url, privateUrl, execution)
        anotherFilter.filterHeaders(url, privateUrl, execution)
      }, maxSize)
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

    private def run[T](filter: HttpFilter, convert: (String, Boolean, ResponseBody) => T): Future[HttpResponse[T]] = {
      Future {
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
          val url = request.url().toString
          filter.filterHeaders(url, privateUrl, execution)
          new HttpResponse(convert(url, privateUrl, response.body), execution)
        } catch {
          case e: Exception =>
            response.close()
            throw e
        }
      }
    }

    def runString[R](filter: HttpFilter): Future[HttpResponse[String]] = {
      run(filter, (_, _, body) => body.string)
    }

    def runBytes[R](filter: HttpFilter): Future[HttpResponse[Array[Byte]]] = {
      run(filter, readBytes(filter))
    }

    private def readBytes(filter: HttpFilter)(url: String, privateUrl: Boolean, body: ResponseBody): Array[Byte] = {
      import okhttp3.internal.Util

      val source = body.source

      try {
        filter.maxSize.map { case (size, error) =>
          val buffer = new okio.Buffer

          @tailrec def readBytes(upTo: Long): Boolean = {
            if (upTo <= 0) {
              false
            } else {
              val read = source.read(buffer, upTo)
              if (read >= 0) {
                readBytes(upTo - read)
              } else {
                true
              }
            }
          }

          if (readBytes(size + 1)) {
            buffer
          } else {
            error(url, privateUrl)
          }
        }.getOrElse(source).readByteArray()
      } finally {
        Util.closeQuietly(source)
      }
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
