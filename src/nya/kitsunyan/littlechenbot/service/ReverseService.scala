package nya.kitsunyan.littlechenbot.service

import nya.kitsunyan.littlechenbot.command.common.Http
import nya.kitsunyan.littlechenbot.util.Utils

import scala.concurrent._

sealed trait ReverseService {
  import ReverseService._

  def getImages(multipartFile: String => Http.MultipartFile)
    (implicit http: Http, executionContext: ExecutionContext): Future[List[Image]]
}

object ReverseService {
  val list: List[ReverseService] = List(Google)

  case class Image(url: String)(val previewUrl: String, val width: Option[Int], val height: Option[Int])

  object Google extends ReverseService {
    override def getImages(multipartFile: String => Http.MultipartFile)
      (implicit http: Http, executionContext: ExecutionContext): Future[List[Image]] = {
      val response = http.http("https://images.google.com/searchbyimage/upload")
        .file(multipartFile("encoded_image"))
        .field("hl" -> "en")
        .runString(Http.Filters.ok)
        .map(_.body)

      response.map { response =>
        "<a href=\"(/imgres?.*?)\".*?<img src=\"(https://encrypted-.*?.gstatic.com/.*?)\"".r
          .findAllMatchIn(response).map(_.subgroups).flatMap {
          case urlPart :: previewUrl :: Nil =>
            val parameters = Utils.getUrlParameters(new java.net.URL("https://www.google.com"
              + Utils.unescapeHtml(urlPart)))
            for {
              url <- parameters.get("imgurl").flatten
              width = parameters.get("w").flatten.map(_.toInt)
              height = parameters.get("h").flatten.map(_.toInt)
            } yield Image(url)(previewUrl, width, height)
          case _ => None
        }.toList
      }
    }
  }
}
