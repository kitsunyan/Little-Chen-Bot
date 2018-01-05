package nya.kitsunyan.littlechenbot.command.common

import nya.kitsunyan.littlechenbot.util.UserMessageException

import info.mukel.telegrambot4s.api._

import okhttp3._

import java.net.SocketTimeoutException
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.language.implicitConversions

trait Http {
  this: AkkaImplicits with BotExecutionContext =>

  import Http._

  val proxy: Option[java.net.Proxy]

  val proxyWhitelist: Set[String]

  implicit val httpInstance: Http = this

  private lazy val client = new OkHttpClient.Builder()
    .connectTimeout(10000, TimeUnit.MILLISECONDS)
    .readTimeout(10000, TimeUnit.MILLISECONDS)
    .build

  private lazy val proxyClient = proxy.map(client.newBuilder().proxy(_).build).getOrElse(client)

  def http(url: String, proxy: Boolean = false): Request = {
    val httpUrl = HttpUrl.parse(url)
    val realProxy = proxy && !proxyWhitelist.contains(httpUrl.host)

    new Request(new okhttp3.Request.Builder().url(httpUrl), proxy = realProxy)
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

  class Request(builder: okhttp3.Request.Builder, proxy: Boolean = false, privateUrl: Boolean = false,
    multipart: Boolean = false, fields: List[BodyAppend] = Nil) {
    private def copy(builder: okhttp3.Request.Builder = builder, proxy: Boolean = proxy,
      privateUrl: Boolean = privateUrl, multipart: Boolean = multipart, fields: List[BodyAppend] = fields): Request = {
      new Request(builder, proxy, privateUrl, multipart, fields)
    }

    def header(nameValue: (String, String)): Request = {
      nameValue match {
        case (name, value) => copy(builder = builder.header(name, value))
      }
    }

    def withPrivateUrl(privateUrl: Boolean): Request = {
      copy(privateUrl = privateUrl)
    }

    def file(multipartFile: MultipartFile): Request = {
      copy(multipart = true, fields = new FileBodyAppend(multipartFile) :: fields)
    }

    def field(field: (String, String)): Request = {
      fields(List(field))
    }

    def fields(fields: Iterable[(String, String)]): Request = {
      copy(fields = fields.foldLeft(this.fields) { case (fields, (name, value)) =>
        new StringBodyAppend(name, value) :: fields
      })
    }

    private def run[T](filter: Filter, convert: (String, Boolean, ResponseBody) => T,
      attemptsLeft: Int): Future[HttpResponse[T]] = {
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
      }.recoverWith {
        case e: SocketTimeoutException =>
          if (attemptsLeft > 0) {
            Future(Thread.sleep(5000)).flatMap(_ => run(filter, convert, attemptsLeft - 1))
          } else {
            throw e
          }
      }
    }

    private def attemptsForRequest: Int = {
      if (proxy) {
        3
      } else {
        1
      }
    }

    def runString[R](filter: Filter): Future[HttpResponse[String]] = {
      run(filter, (_, _, body) => body.string, attemptsForRequest)
    }

    def runBytes[R](filter: Filter): Future[HttpResponse[Array[Byte]]] = {
      run(filter, readBytes(filter), attemptsForRequest)
    }

    private def readBytes(filter: Filter)(url: String, privateUrl: Boolean, body: ResponseBody): Array[Byte] = {
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
}

object Http {
  case class Filter(filterHeaders: (String, Boolean, HttpExecution) => Unit,
    maxSize: Option[(Int, (String, Boolean) => Nothing)] = None)

  class ExtendedFilter(filter: Filter) {
    def &&(anotherFilter: Filter): Filter = {
      val maxSize = (for {
        (size, error) <- filter.maxSize
        (anotherSize, anotherError) <- anotherFilter.maxSize
      } yield if (size < anotherSize) {
        (size, error)
      } else {
        (anotherSize, anotherError)
      }) orElse filter.maxSize orElse anotherFilter.maxSize

      Filter((url, privateUrl, execution) => {
        filter.filterHeaders(url, privateUrl, execution)
        anotherFilter.filterHeaders(url, privateUrl, execution)
      }, maxSize)
    }
  }

  implicit def extendedHttpFilter(filter: Filter): ExtendedFilter = {
    new ExtendedFilter(filter)
  }

  class HttpException(override val userMessage: Option[String], message: String) extends Exception(message)
    with UserMessageException

  object Filters {
    def any: Filter = Filter((_, _, _) => ())

    def ok: Filter = code(200)

    def code(validCodes: Int*): Filter = {
      Filter((url, privateUrl, execution) => {
        if (!validCodes.contains(execution.code)) {
          throwResponseCodeExceptionWithPrivateUrl(url, privateUrl, execution.code)
        }
      })
    }

    def contentLength(maxContentLength: Int): Filter = {
      Filter((url, privateUrl, execution) => {
        execution.headers("Content-Length")
          .headOption
          .map(_.toLong)
          .find(_ > maxContentLength)
          .foreach(l => throwWithPrivateUrl(url, privateUrl, s"Response is too large: [$l]"))
      }, Some(maxContentLength, (url, privateUrl) => throwWithPrivateUrl(url, privateUrl, s"Response is too large")))
    }
  }

  private def throwWithPrivateUrl(url: String, privateUrl: Boolean, commonMessage: String): Nothing = {
    val publicMessage = s"$commonMessage."
    val privateMessage = s"$commonMessage $url."
    throw new HttpException(Some(if (privateUrl) publicMessage else privateMessage), privateMessage)
  }

  def throwResponseCodeExceptionWithPrivateUrl(url: String, privateUrl: Boolean, responseCode: Int): Nothing = {
    throwWithPrivateUrl(url, privateUrl, s"Invalid response: [$responseCode]")
  }

  case class MultipartFile(name: String, filename: String, mimeType: String, data: Array[Byte])

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
