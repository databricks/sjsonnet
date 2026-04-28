package sjsonnet

import java.io.{ByteArrayOutputStream, File}
import java.util
import java.util.Base64
import java.util.zip.GZIPOutputStream
import scala.scalanative.regex.Pattern
import scala.collection.mutable
import org.virtuslab.yaml.*

object Platform {
  // Scala Native local hyperfine is steadier on the Long-mask path than the single-char shortcut.
  final val useIntStripCharsBitset: Boolean = false

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

  private def nodeToJson(node: Node): ujson.Value = node match {
    case _: Node.ScalarNode =>
      YamlDecoder.forAny.construct(node) match {
        case Right(v) =>
          v match {
            case null | None => ujson.Null
            case v: String   =>
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
    case Node.SequenceNode(nodes, _) =>
      val buf = new mutable.ArrayBuffer[ujson.Value](nodes.size)
      for (n <- nodes) {
        buf += nodeToJson(n)
      }
      ujson.Arr(buf)
    case Node.MappingNode(mappings, _) =>
      val buf = upickle.core.LinkedHashMap[String, ujson.Value]()
      buf.sizeHint(mappings.size)
      for ((key, value) <- mappings) {
        key match {
          case Node.ScalarNode(k, _) => buf(k) = nodeToJson(value)
          case _ => Error.fail("Invalid YAML mapping key class: " + key.getClass.getSimpleName)
        }
      }
      ujson.Obj(buf)
    case _ =>
      Error.fail("Unsupported YAML node type: " + node.getClass.getSimpleName)
  }

  def yamlToJson(s: String): ujson.Value = {
    if (s.trim.isEmpty) return ujson.Null

    // Preprocess to add explicit nulls for empty documents,
    // since scala-yaml's parseManyYamls can't handle empty documents
    // (DocumentStart immediately followed by DocumentEnd).
    val preprocessed = addExplicitNullsForEmptyDocs(s)

    parseManyYamls(preprocessed) match {
      case Right(documents) =>
        documents.size match {
          case 0 => ujson.Null
          case 1 => nodeToJson(documents.head)
          case _ =>
            val buf = new mutable.ArrayBuffer[ujson.Value](documents.size)
            for (doc <- documents) {
              buf += nodeToJson(doc)
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
