package sjsonnet

import java.io.{ByteArrayOutputStream, File}
import java.util
import java.util.Base64
import java.util.zip.GZIPOutputStream
import scala.scalanative.regex.Pattern

import scala.collection.compat._

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

  private def computeHash(algorithm: String, s: String) = {
    java.security.MessageDigest
      .getInstance(algorithm)
      .digest(s.getBytes("UTF-8"))
      .map { b => String.format("%02x", (b & 0xff).asInstanceOf[Integer]) }
      .mkString
  }

  def md5(s: String): String = computeHash("MD5", s)

  def sha1(s: String): String = computeHash("SHA-1", s)

  def sha256(s: String): String = computeHash("SHA-256", s)

  def sha512(s: String): String = computeHash("SHA-512", s)

  // Same as go-jsonnet https://github.com/google/go-jsonnet/blob/2b4d7535f540f128e38830492e509a550eb86d57/builtins.go#L959
  def sha3(s: String): String = computeHash("SHA3-512", s)

  def hashFile(file: File): String = {
    // File hashes in Scala Native are just the file content
    scala.io.Source.fromFile(file).mkString
  }

  private val regexCache = new util.concurrent.ConcurrentHashMap[String, Pattern]
  private val dashPattern = getPatternFromCache("-")

  def getPatternFromCache(pat: String): Pattern =
    regexCache.computeIfAbsent(pat, _ => Pattern.compile(pat))

  def getNamedGroupsMap(pat: Pattern): Map[String, Int] = scala.jdk.javaapi.CollectionConverters
    .asScala(pat.re2.namedGroups)
    .view
    .mapValues(_.intValue())
    .toMap

  def regexQuote(s: String): String = {
    val quote = Pattern.quote(s)
    val matcher = dashPattern.matcher(quote)
    if (matcher.find()) {
      matcher.replaceAll("\\\\-")
    } else {
      quote
    }
  }
}
