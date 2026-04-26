package sjsonnet

import org.virtuslab.yaml.*

import java.util
import java.util.regex.Pattern
import scala.collection.mutable

object Platform {
  // Scala.js Long is expensive, so keep the ASCII stripChars fast path on Int masks.
  final val useIntStripCharsBitset: Boolean = true

  private def nodeToJson(node: Node): ujson.Value = node match {
    case _: Node.ScalarNode =>
      YamlDecoder.forAny.construct(node).getOrElse("") match {
        case null | None => ujson.Null
        case v: String  => ujson.read(s"\"${v.replace("\"", "\\\"").replace("\n", "\\n")}\"", false)
        case v: Boolean => ujson.Bool(v)
        case v: Int     => ujson.Num(v.toDouble)
        case v: Long    => ujson.Num(v.toDouble)
        case v: Double  => ujson.Num(v)
        case v: Float   => ujson.Num(v.toDouble)
        case v: BigDecimal => ujson.Num(v.toDouble)
        case v: BigInt     => ujson.Num(v.toDouble)
        case v: Short      => ujson.Num(v.toDouble)
        case _ => Error.fail("Unsupported YAML scalar type: " + node.getClass.getSimpleName)
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

  def appendMemoryStats(sb: StringBuilder): Unit = {}
}
