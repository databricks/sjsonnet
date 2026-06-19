package sjsonnet

import org.virtuslab.yaml.*

import java.util
import java.util.regex.Pattern
import scala.collection.mutable

object Platform {
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
  @inline def copyAsciiStringToBytes(s: String, dst: Array[Byte], dstPos: Int): Unit = {
    var i = 0
    var pos = dstPos
    val len = s.length
    while (i < len) {
      dst(pos) = s.charAt(i).toByte
      i += 1
      pos += 1
    }
  }

  def isAsciiJsonSafe(s: String): Boolean =
    CharSWAR.isAsciiJsonSafe(s)

  def isAsciiJsonSafe(s: String, from: Int, to: Int): Boolean =
    CharSWAR.isAsciiJsonSafe(s, from, to)

  private val Yaml12OctalPattern = Pattern.compile("[-+]?0o[0-7][0-7_]*")
  private val YamlInvalidOctalDecimalPattern =
    Pattern.compile("[-+]?0[0-9]*[89][0-9]*")

  private def parseYamlDecimalLong(value: String): ujson.Num =
    ujson.Num(java.lang.Long.parseLong(value).toDouble)

  private def nodeToJson(node: Node, input: String): ujson.Value = node match {
    case sn: Node.ScalarNode =>
      val constructed = YamlDecoder.forAny.construct(sn).getOrElse("")
      constructed match {
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
