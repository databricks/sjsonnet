package sjsonnet

import java.io.File
import java.util
import java.util.regex.Pattern


object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def xzBytes(s: Array[Byte], compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }
  def xzString(s: String, compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }
  def yamlToJson(s: String): String = {
    throw new Exception("parseYaml() not implemented in Scala.js")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }
  def sha1(s: String): String = {
    throw new Exception("SHA1 not implemented in Scala.js")
  }
  def sha256(s: String): String = {
    throw new Exception("SHA256 not implemented in Scala.js")
  }
  def sha512(s: String): String = {
    throw new Exception("SHA512 not implemented in Scala.js")
  }
  def sha3(s: String): String = {
    throw new Exception("SHA3 not implemented in Scala.js")
  }
  def hashFile(file: File): String = {
    throw new Exception("hashFile not implemented in Scala.js")
  }

  private val regexCache = new util.concurrent.ConcurrentHashMap[String, Pattern]
  def getPatternFromCache(pat: String) : Pattern = regexCache.computeIfAbsent(pat, _ => Pattern.compile(pat))

  def regexQuote(s: String): String = Pattern.quote(s)
}
