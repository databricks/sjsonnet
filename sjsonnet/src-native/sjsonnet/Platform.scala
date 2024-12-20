package sjsonnet

import java.io.{ByteArrayOutputStream, File}
import java.util
import java.util.Base64
import java.util.zip.GZIPOutputStream
import java.util.regex.Pattern

object Platform {
  def gzipBytes(b: Array[Byte]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    val gzip: GZIPOutputStream = new GZIPOutputStream(outputStream)
    try {
      gzip.write(b)
    } finally {
      gzip.close()
      outputStream.close()
    }
    Base64.getEncoder.encodeToString(outputStream.toByteArray)
  }

  def gzipString(s: String): String = {
    gzipBytes(s.getBytes())
  }

  def xzBytes(s: Array[Byte], compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def xzString(s: String, compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def yamlToJson(s: String): String = {
    throw new Exception("parseYaml() not implemented in Scala Native")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala Native")
  }
  def sha1(s: String): String = {
    throw new Exception("SHA1 not implemented in Scala Native")
  }
  def sha256(s: String): String = {
    throw new Exception("SHA256 not implemented in Scala Native")
  }
  def sha512(s: String): String = {
    throw new Exception("SHA512 not implemented in Scala Native")
  }
  def sha3(s: String): String = {
    throw new Exception("SHA3 not implemented in Scala Native")
  }

  def hashFile(file: File): String = {
    // File hashes in Scala Native are just the file content
    scala.io.Source.fromFile(file).mkString
  }

  private val regexCache = new util.concurrent.ConcurrentHashMap[String, Pattern]
  def getPatternFromCache(pat: String) : Pattern = regexCache.computeIfAbsent(pat, _ => Pattern.compile(pat))

  def regexQuote(s: String): String = Pattern.quote(s)
}
