package sjsonnet
object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def xzBytes(s: Array[Byte]): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }
  def xzString(s: String): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }
  def yamlToJson(s: String): String = {
    throw new Exception("parseYaml() not implemented in Scala.js")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }

  def compactHashMap[K, V](map: java.util.LinkedHashMap[K, V]): java.util.Map[K, V] = {
    // No-op - we don't bother with compaction in Scala.js
    map
  }
}
