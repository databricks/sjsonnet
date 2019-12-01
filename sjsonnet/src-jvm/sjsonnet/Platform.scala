package sjsonnet
object Platform {
  def gzipBytes(b: Array[Byte]): String = {
    val outputStream = new java.io.ByteArrayOutputStream(b.length)
    val gzip = new java.util.zip.GZIPOutputStream(outputStream)
    gzip.write(b)
    gzip.close()
    val gzippedBase64: String = java.util.Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    gzippedBase64
  }
  def gzipString(s: String): String = {
    val outputStream = new java.io.ByteArrayOutputStream(s.length)
    val gzip = new java.util.zip.GZIPOutputStream(outputStream)
    gzip.write(s.getBytes())
    gzip.close()
    val gzippedBase64: String = java.util.Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    gzippedBase64
  }
  def md5(s: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map{ b => String.format("%02x", new java.lang.Integer(b & 0xff))}
      .mkString
  }
}