package nya.kitsunyan.littlechenbot

object Utils {
  def webpToPng(data: Array[Byte]): Array[Byte] = {
    try {
      val process = Runtime.getRuntime.exec(Array("dwebp", "-o", "-", "--", "-"))
      val outputStream = process.getOutputStream
      outputStream.write(data)
      outputStream.close()
      val result = io.Source.fromInputStream(process.getInputStream)(io.Codec.ISO8859).map(_.toByte).toArray
      val exitValue = process.waitFor()
      if (exitValue != 0) {
        throw new Exception(s"Exit value is $exitValue.")
      }
      result
    } catch {
      case e: Exception =>
        e.printStackTrace()
        data
    }
  }
}
