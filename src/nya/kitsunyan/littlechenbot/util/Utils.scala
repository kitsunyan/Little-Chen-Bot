package nya.kitsunyan.littlechenbot.util

object Utils {
  def execWithCode(data: Option[Array[Byte]], commands: Seq[String]): (Int, Array[Byte]) = {
    val process = Runtime.getRuntime.exec(commands.toArray)
    try {
      data.foreach { data =>
        new Thread(() => {
          try {
            val outputStream = process.getOutputStream
            outputStream.write(data)
            outputStream.close()
          } catch {
            case _: Exception =>
          }
        }).start()
      }

      val result = io.Source.fromInputStream(process.getInputStream)(io.Codec.ISO8859).map(_.toByte).toArray
      val exitValue = process.waitFor()
      (exitValue, result)
    } finally {
      process.destroy()
    }
  }

  def exec(data: Option[Array[Byte]], commands: Seq[String]): Array[Byte] = {
    val (exitValue, result) = execWithCode(data, commands)
    if (exitValue != 0) {
      throw new Exception(s"Exit value is $exitValue.")
    }
    result
  }

  def webpToPng(data: Array[Byte])(implicit binaries: Binaries): Array[Byte] = {
    exec(Some(data), List(binaries.dwebp, "-o", "-", "--", "-"))
  }

  def extractPreviewPng(data: Array[Byte], inputFormat: String)(implicit binaries: Binaries): Array[Byte] = {
    val (exitValue, result) = execWithCode(Some(data), Seq(binaries.ffmpeg, "-f", inputFormat, "-i", "-",
      "-vframes", "1", "-f", "image2pipe", "-vcodec", "png", "-"))

    val magic = Seq(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a).map(_.toByte)
    if (result.take(8).toSeq == magic) {
      result
    } else if (exitValue != 0) {
      throw new Exception(s"Exit value is $exitValue.")
    } else {
      throw new Exception("Invalid PNG result.")
    }
  }

  sealed abstract class BlurMode(val color: String)

  object BlurMode {
    case object No extends BlurMode("#88bbee")
    case object Soft extends BlurMode("#ffee88")
    case object Hard extends BlurMode("#ff6688")
  }

  val (mimeTypeMap, extensionMap) = {
    val list =
      "png" -> "image/png" ::
      "jpg" -> "image/jpeg" ::
      "jpeg" -> "image/jpeg" ::
      "gif" -> "image/gif" ::
      "webp" -> "image/webp" ::
      Nil

    (list.map {
      case (extension, mimeType) => mimeType -> extension
    }.toMap, list.toMap)
  }

  case class Preview(index: Int, image: Option[Array[Byte]], mimeType: String, blurMode: BlurMode) {
    def extension: Option[String] = mimeTypeMap.lift(mimeType)
  }

  def drawPreview(previews: List[Preview])(implicit binaries: Binaries): Option[Array[Byte]] = {
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

            val c1 = List(binaries.magick, extensionIn, "-resize", s"${width}x${height}")
            val c2 = List("-blur", s"${blurRadius}x${blurRadius / 2}")
            val c3 = List("-background", imageBackgroundColor, "-gravity", "center",
              "-extent", s"${width}x${height}", "png:-")

            exec(Some(image), c1 ::: (if (blurRadius >= 2) c2 else Nil) ::: c3)
          }
        }

        (preview.index, newImage, preview.blurMode.color)
      }.foldLeft(exec(None, List(binaries.magick, "-size", s"${totalWidth}x${totalHeight}",
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

            exec(Some(result), List(binaries.magick, "png:-",
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

  def extractNameFromUrl(url: String, mimeType: Option[String]): String = {
    val start = url.lastIndexOf('/') + 1
    val end = url.indexOf('?', start)
    val encodedName = if (end >= start) url.substring(start, end) else url.substring(start)
    val decodedName = java.net.URLDecoder.decode(encodedName, "UTF-8")

    val colon = decodedName.lastIndexOf(':')
    val dot = decodedName.lastIndexOf('.')
    val truncatedName = if (colon > dot && dot >= 0) decodedName.substring(0, colon) else decodedName
    val extension = if (dot >= 0) Some(truncatedName.substring(dot + 1)) else None

    val urlMimeType = extension.flatMap(extensionMap.lift)

    if (mimeType.nonEmpty && urlMimeType != mimeType) {
      val newName = if (dot >= 0) truncatedName.substring(0, dot) else truncatedName
      val extension = mimeType.flatMap(mimeTypeMap.lift).getOrElse("jpeg")
      s"$newName.$extension"
    } else {
      truncatedName
    }
  }

  private def schemeBoolean(https: Boolean): String = {
    if (https) {
      "https"
    } else {
      "http"
    }
  }

  def appendSchemeHost(https: Boolean, host: String): PartialFunction[String, String] = {
    case s if s.startsWith("//") => s"${schemeBoolean(https)}:$s"
    case s if s.startsWith("/") => s"${schemeBoolean(https)}://$host$s"
    case s if s.startsWith("http://") || s.startsWith("https://") => s
    case s => s"${schemeBoolean(https)}://$host/$s"
  }
}
