package nya.kitsunyan.littlechenbot

object Utils {
  private def exec(data: Option[Array[Byte]], commands: Array[String]): Array[Byte] = {
    val process = Runtime.getRuntime.exec(commands)
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
      Some(exec(Some(data), Array("dwebp", "-o", "-", "--", "-")))
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }

  def drawPreview(images: List[(Int, Option[Array[Byte]], String)]): Option[Array[Byte]] = {
    case class Rect(left: Int, top: Int, right: Int, bottom: Int)
    case class Point(x: Int, y: Int)

    try {
      val padding = Rect(8, 8, 8, 8)
      val size = Point(160, 128)
      val indexSize = 24
      val maxColumns = 3
      val columns = Math.min(images.length, maxColumns)
      val rows = (images.length + maxColumns - 1) / maxColumns
      val totalWidth = columns * size.x
      val totalHeight = rows * size.y

      val backgroundColor = "#303336"
      val imageBackgroundColor = "#40444a"
      val indexCircleColor = "#88bbee"
      val indexTextColor = "#222222"

      Some(images.map { case (index, image, mimeType) =>
        val width = size.x - padding.left - padding.right
        val height = size.y - padding.top - padding.bottom

        // Resize image to ${width}x${height} square
        val newImage = image.flatMap { image =>
          (if (mimeType == "image/jpeg") {
            Some("jpg:-")
          } else if (mimeType == "image/png") {
            Some("png:-")
          } else {
            None
          }).map { extensionIn =>
            exec(Some(image), Array("convert", extensionIn, "-resize", s"${width}x${height}",
              "-background", imageBackgroundColor, "-gravity", "center", "-extent", s"${width}x${height}", "png:-"))
          }
        }

        (index, newImage)
      }.foldLeft(exec(None, Array("convert", "-size", s"${totalWidth}x${totalHeight}",
        s"canvas:$backgroundColor", "png:-")), 0) { case ((result, i), (index, image)) =>
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

            exec(Some(result), Array("convert", "png:-",
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
}
