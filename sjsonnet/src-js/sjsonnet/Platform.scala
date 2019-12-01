package sjsonnet
object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }
}