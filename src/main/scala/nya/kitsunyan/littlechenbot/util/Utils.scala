package nya.kitsunyan.littlechenbot.util

object Utils {
  def exec(data: Option[Array[Byte]], commands: Seq[String]): Array[Byte] = {
    val process = Runtime.getRuntime.exec(commands.toArray)
    try {
      val outputStream = process.getOutputStream
      data.foreach(outputStream.write)
      outputStream.close()
      val result = io.Source.fromInputStream(process.getInputStream)(io.Codec.ISO8859).map(_.toByte).toArray
      val exitValue = process.waitFor()
      if (exitValue != 0) {
        throw new Exception(s"Exit value is $exitValue.")
      }
      result
    } finally {
      process.destroy()
    }
  }

  def webpToPng(data: Array[Byte]): Option[Array[Byte]] = {
    try {
      Some(exec(Some(data), List("dwebp", "-o", "-", "--", "-")))
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }

  sealed abstract class BlurMode(val color: String)

  object BlurMode {
    case object No extends BlurMode("#88bbee")
    case object Soft extends BlurMode("#ffee88")
    case object Hard extends BlurMode("#ff6688")
  }

  case class Preview(index: Int, image: Option[Array[Byte]], mimeType: String, blurMode: BlurMode) {
    def extension: Option[String] = {
      mimeType match {
        case "image/jpeg" => Some("jpg")
        case "image/png" => Some("png")
        case "image/gif" => Some("gif")
        case "image/webp" => Some("webp")
        case _ => None
      }
    }
  }

  def drawPreview(previews: List[Preview]): Option[Array[Byte]] = {
    case class Rect(left: Int, top: Int, right: Int, bottom: Int)
    case class Point(x: Int, y: Int)

    try {
      val padding = Rect(8, 8, 8, 8)
      val size = Point(160, 128)
      val indexSize = 24
      val maxColumns = 3
      val columns = math.min(previews.length, maxColumns)
      val rows = (previews.length + maxColumns - 1) / maxColumns
      val totalWidth = columns * size.x
      val totalHeight = rows * size.y

      val backgroundColor = "#303336"
      val imageBackgroundColor = "#40444a"
      val indexTextColor = "#222222"

      Some(previews.map { preview =>
        val width = size.x - padding.left - padding.right
        val height = size.y - padding.top - padding.bottom

        // Resize image to ${width}x${height} square and blur
        val newImage = preview.image.flatMap { image =>
          preview.extension.map(_ + ":-").map { extensionIn =>
            val denominator = preview.blurMode match {
              case BlurMode.No => Int.MaxValue
              case BlurMode.Soft => 25
              case BlurMode.Hard => 15
            }
            val blurRadius = math.max(width, height) / denominator

            val c1 = List("convert", extensionIn, "-resize", s"${width}x${height}")
            val c2 = List("-blur", s"${blurRadius}x${blurRadius / 2}")
            val c3 = List("-background", imageBackgroundColor, "-gravity", "center",
              "-extent", s"${width}x${height}", "png:-")

            exec(Some(image), c1 ::: (if (blurRadius >= 2) c2 else Nil) ::: c3)
          }
        }

        (preview.index, newImage, preview.blurMode.color)
      }.foldLeft(exec(None, List("convert", "-size", s"${totalWidth}x${totalHeight}",
        s"canvas:$backgroundColor", "png:-")), 0) { case ((result, i), (index, image, indexCircleColor)) =>
        // Draw each image on canvas
        (image.map { image =>
          val file = java.io.File.createTempFile("littlechenbot", null)
          val output = new java.io.FileOutputStream(file)
          try {
            output.write(image)
          } finally {
            output.close()
          }

          try {
            val x = (i % columns) * size.x
            val y = (i / columns) * size.y
            val imageX = x + padding.left
            val imageY = y + padding.top
            val indexX = x + size.x - padding.right - indexSize / 2
            val indexY = y + size.y - padding.bottom - indexSize / 2
            val indexXedge = indexX + indexSize / 2
            val indexYedge = indexY + indexSize / 2
            val indexXcenter = {
              val value = indexX - totalWidth / 2 + 1
              if (value > 0) s"+$value" else s"$value"
            }
            val indexYcenter = {
              val value = indexY - totalHeight / 2 + 1
              if (value > 0) s"+$value" else s"$value"
            }

            exec(Some(result), List("convert", "png:-",
              file.getPath, "-geometry", s"+$imageX+$imageY", "-composite",
              "-fill", backgroundColor, "-draw", s"rectangle $indexX,$indexY $indexXedge,$indexYedge",
              "-fill", indexCircleColor, "-draw", s"circle $indexX,$indexY ${indexXedge - 1},$indexY",
              "-fill", indexTextColor, "-font", "Roboto-Medium", "-pointsize", s"${indexSize / 2}",
              "-gravity", "center", "-annotate", s"$indexXcenter$indexYcenter", s"$index",
              "png:-"))
          } finally {
            file.delete()
          }
        }.getOrElse(result), i + 1)
      }._1)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }

  def unescapeHtml(s: String): String = {
    Option(s).filter(!_.isEmpty).flatMap { s =>
      try {
        import javax.swing.text.html.HTMLDocument
        import javax.swing.text.html.HTMLEditorKit
        import java.io.StringReader

        val document = new HTMLDocument
        new HTMLEditorKit().read(new StringReader(s), document, 0)
        Some(document.getText(1, document.getLength - 1))
      } catch {
        case _: Exception => None
      }
    }.getOrElse(s)
  }

  def getUrlParameters(url: java.net.URL): Map[String, Option[String]] = {
    def decode(s: String): String = java.net.URLDecoder.decode(s, "UTF-8")

    val query = Option(url.getQuery).getOrElse("")

    query.split("&").toList.map { part =>
      val s = part.split("=", 2)
      decode(s.head) -> s.lift(1).map(decode)
    }.toMap
  }

  def extractNameFromUrl(url: String): String = {
    val start = url.lastIndexOf('/') + 1
    val end = url.indexOf('?', start)
    val name = if (end >= start) url.substring(start, end) else url.substring(start)
    java.net.URLDecoder.decode(name, "UTF-8")
  }
}
