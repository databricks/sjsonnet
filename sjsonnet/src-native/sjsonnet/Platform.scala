package sjsonnet
object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala Native")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala Native")
  }
  def xzBytes(s: Array[Byte]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def xzString(s: String): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala Native")
  }
}
