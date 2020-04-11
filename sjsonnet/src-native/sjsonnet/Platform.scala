package sjsonnet

object Platform {
  def gzipBytes(b: Array[Byte]): String = GZIP.gzipBytes(b)
  def gzipString(s: String): String = {
    gzipBytes(s.getBytes())
  }
  def xzBytes(b: Array[Byte]): String = throw new Exception("XZ not implemented in Scala Native")
  def xzString(s: String): String = throw new Exception("XZ not implemented in Scala Native")
  def md5(s: String): String = {
    Md5.md5(s)
      .map(b => String.format("%02x", new java.lang.Integer(b & 0xff)))
      .mkString
  }
}
