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
  private val namedGroupPattern = Pattern.compile("\\(\\?<(.+?)>.*?\\)")
  private val namedGroupPatternReplace = Pattern.compile("(\\(\\?P<)(.+?>.*?\\))")

  // scala.js does not rely on re2. Per https://www.scala-js.org/doc/regular-expressions.html.
  // Expect to see some differences in behavior.
  def getPatternFromCache(pat: String): Pattern = {
    val fixedPattern = namedGroupPatternReplace.matcher(pat).replaceAll("(?<$2")
    regexCache.computeIfAbsent(pat, _ => Pattern.compile(fixedPattern))
  }

  def getNamedGroupsMap(pat: Pattern): Map[String, Int] = {
    val namedGroups = Map.newBuilder[String, Int]
    val matcher = namedGroupPattern.matcher(pat.pattern())
    while (matcher.find()) {
      for (i <- 1 to matcher.groupCount()) {
        namedGroups += matcher.group(i) -> i
      }
    }
    namedGroups.result()
  }

  def regexQuote(s: String): String = Pattern.quote(s)
}
