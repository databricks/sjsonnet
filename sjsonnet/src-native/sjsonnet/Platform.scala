package sjsonnet

import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets.UTF_8
import java.util
import java.util.Base64
import java.util.zip.GZIPOutputStream
import scala.scalanative.regex.Pattern
import scala.annotation.nowarn
import scala.collection.mutable
import org.virtuslab.yaml.*

object Platform {
  private val hexChars = "0123456789abcdef".toCharArray

  private def repeatCapacity(s: String, count: Int): Int =
    if (count > 0 && s.length <= Int.MaxValue / count) s.length * count else 0

  def repeatString(s: String, count: Int): String = {
    if (count <= 0 || s.isEmpty) ""
    else {
      val builder = new StringBuilder(repeatCapacity(s, count))
      var i = 0
      while (i < count) {
        builder.append(s)
        i += 1
      }
      builder.toString()
    }
  }

  // Used only for strings already proven ASCII-safe; copies low bytes without allocation.
  @inline
  @nowarn("cat=deprecation")
  def copyAsciiStringToBytes(s: String, dst: Array[Byte], dstPos: Int): Unit =
    s.getBytes(0, s.length, dst, dstPos)

  def isAsciiJsonSafe(s: String): Boolean =
    CharSWAR.isAsciiJsonSafe(s)

  def isAsciiJsonSafe(s: String, from: Int, to: Int): Boolean =
    CharSWAR.isAsciiJsonSafe(s, from, to)

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
    gzipBytes(s.getBytes(UTF_8))
  }

  def xzBytes(s: Array[Byte], compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def xzString(s: String, compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }

  private val Yaml12OctalPattern = Pattern.compile("[-+]?0o[0-7][0-7_]*")
  private val YamlInvalidOctalDecimalPattern =
    Pattern.compile("[-+]?0[0-9]*[89][0-9]*")
  private val YamlNonFiniteFloatPattern =
    Pattern.compile("[-+]?[.](?:inf|Inf|INF|nan|NaN|NAN)")

  private def parseYamlDecimalLong(value: String): ujson.Num =
    ujson.Num(java.lang.Long.parseLong(value).toDouble)

  private def nodeToJson(node: Node, input: String): ujson.Value = node match {
    case sn: Node.ScalarNode =>
      if (sn.value.contains(":") && (sn.tag == Tag.int || sn.tag == Tag.float)) {
        ujson.Str(sn.value)
      } else if (sn.tag == Tag.float && YamlNonFiniteFloatPattern.matcher(sn.value).matches()) {
        ujson.Str(sn.value)
      } else {
        YamlDecoder.forAny.construct(sn) match {
          case Right(v) =>
            v match {
              case null | None => ujson.Null
              case v: String
                  if sn.tag == Tag.str && Yaml12OctalPattern.matcher(v).matches() &&
                  !isQuotedScalar(sn, input) =>
                val negative = v.charAt(0) == '-'
                val octalPart =
                  if (negative || v.charAt(0) == '+') v.substring(3) else v.substring(2)
                val result = java.lang.Long.parseLong(octalPart.replace("_", ""), 8)
                ujson.Num((if (negative) -result else result).toDouble)
              case v: String
                  if sn.tag == Tag.str && YamlInvalidOctalDecimalPattern.matcher(v).matches() &&
                  !isQuotedScalar(sn, input) =>
                parseYamlDecimalLong(v)
              case v: String =>
                ujson.read(s"\"${v.replace("\"", "\\\"").replace("\n", "\\n")}\"", false)
              case v: Boolean    => ujson.Bool(v)
              case v: Byte       => ujson.Num(v.toDouble)
              case v: Int        => ujson.Num(v.toDouble)
              case v: Long       => ujson.Num(v.toDouble)
              case v: Double     => ujson.Num(v)
              case v: Float      => ujson.Num(v.toDouble)
              case v: BigDecimal => ujson.Num(v.toDouble)
              case v: BigInt     => ujson.Num(v.toDouble)
              case v: Short      => ujson.Num(v.toDouble)
              case _             =>
                Error.fail(
                  "Unsupported YAML scalar type: " + v.getClass.getSimpleName + " with value: " + v
                )
            }
          case Left(e) =>
            Error.fail("Error converting YAML scalar to JSON: " + e.getMessage)
        }
      }
    case Node.SequenceNode(nodes, _) =>
      val buf = new mutable.ArrayBuffer[ujson.Value](nodes.size)
      for (n <- nodes) {
        buf += nodeToJson(n, input)
      }
      ujson.Arr(buf)
    case Node.MappingNode(mappings, _) =>
      val buf = upickle.core.LinkedHashMap[String, ujson.Value]()
      buf.sizeHint(mappings.size)
      for ((key, value) <- mappings) {
        key match {
          case Node.ScalarNode(k, _) => buf(k) = nodeToJson(value, input)
          case _ => Error.fail("Invalid YAML mapping key class: " + key.getClass.getSimpleName)
        }
      }
      ujson.Obj(buf)
    case _ =>
      Error.fail("Unsupported YAML node type: " + node.getClass.getSimpleName)
  }

  private def isQuotedScalar(sn: Node.ScalarNode, input: String): Boolean = {
    sn.pos match {
      case Some(range) =>
        val offset = range.start.offset
        offset >= 0 && offset < input.length && {
          val c = input.charAt(offset)
          c == '"' || c == '\''
        }
      case None => false
    }
  }

  private val YamlDocStartPattern = Pattern.compile("\\A\\s*---(?:[ \\t\\n\\r]|\\z)")

  def yamlToJson(s: String): ujson.Value = {
    if (s.trim.isEmpty) return ujson.Null

    // Preprocess to add explicit nulls for empty documents,
    // since scala-yaml's parseManyYamls can't handle empty documents
    // (DocumentStart immediately followed by DocumentEnd).
    val preprocessed = addExplicitNullsForEmptyDocs(s)
    val hasExplicitDocStart = YamlDocStartPattern.matcher(s).find()

    parseManyYamls(preprocessed) match {
      case Right(documents) =>
        documents.size match {
          case 0                         => ujson.Null
          case 1 if !hasExplicitDocStart => nodeToJson(documents.head, preprocessed)
          case _                         =>
            val buf = new mutable.ArrayBuffer[ujson.Value](documents.size)
            for (doc <- documents) {
              buf += nodeToJson(doc, preprocessed)
            }
            ujson.Arr(buf)
        }
      case Left(e) => Error.fail("Error converting YAML to JSON: " + e.getMessage)
    }
  }

  /**
   * Inserts explicit "null" content for empty YAML documents. An empty document is one where a
   * "---" marker has no content before the next "---" marker or end of input.
   */
  private def addExplicitNullsForEmptyDocs(s: String): String = {
    val lines = s.split("\n", -1).toList
    val result = new mutable.ArrayBuffer[String](lines.size + 4)
    var pendingEmptySep = false

    for (line <- lines) {
      val trimmed = line.trim
      val isSep =
        trimmed.startsWith("---") && (trimmed.length == 3 || trimmed.charAt(3).isWhitespace)

      if (isSep) {
        if (pendingEmptySep) {
          // Previous "---" had no content after it; insert explicit null
          result += "null"
        }
        result += line
        // Check if this separator has inline content (e.g. "--- 3", "--- >")
        val afterMarker = trimmed.substring(3).trim
        pendingEmptySep = afterMarker.isEmpty
      } else {
        if (pendingEmptySep && trimmed.nonEmpty) {
          pendingEmptySep = false
        }
        result += line
      }
    }

    // Handle trailing "---" with no content
    if (pendingEmptySep) {
      result += "null"
    }

    result.mkString("\n")
  }

  private def bytesToHex(bytes: Array[Byte]): String = {
    val out = new Array[Char](bytes.length * 2)
    var i = 0
    var j = 0
    while (i < bytes.length) {
      val b = bytes(i) & 0xff
      out(j) = hexChars(b >>> 4)
      out(j + 1) = hexChars(b & 0x0f)
      i += 1
      j += 2
    }
    new String(out)
  }

  private def computeHash(algorithm: String, s: String): String =
    bytesToHex(java.security.MessageDigest.getInstance(algorithm).digest(s.getBytes(UTF_8)))

  def md5(s: String): String = computeHash("MD5", s)

  def sha1(s: String): String = computeHash("SHA-1", s)

  def sha256(s: String): String = computeHash("SHA-256", s)

  def sha512(s: String): String = computeHash("SHA-512", s)

  // Same as go-jsonnet https://github.com/google/go-jsonnet/blob/2b4d7535f540f128e38830492e509a550eb86d57/builtins.go#L959
  def sha3(s: String): String = computeHash("SHA3-512", s)

  def hashBytes(bytes: Array[Byte]): String =
    scala.util.hashing.MurmurHash3.bytesHash(bytes).toHexString

  def hashFile(file: File): String = {
    scala.util.hashing.MurmurHash3
      .orderedHash(
        scala.io.Source.fromFile(file)
      )
      .toHexString
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

  def appendMemoryStats(sb: StringBuilder): Unit = {}
}
