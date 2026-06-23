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

  private val Yaml12OctalPattern = "[-+]?0o[0-7][0-7_]*".r
  private val YamlInvalidOctalDecimalPattern =
    "[-+]?0[0-9]*[89][0-9]*".r
  private val YamlNonFiniteInfPattern =
    "[-+]?[.](?:inf|Inf|INF)".r
  private val YamlNonFiniteNaNPattern =
    "[.](?:nan|NaN|NAN)".r
  private val YamlSignedNonFiniteNaNPattern =
    "[-+][.](?:nan|NaN|NAN)".r

  private def regexMatches(pattern: scala.util.matching.Regex, value: String): Boolean =
    pattern.pattern.matcher(value).matches()

  private def normalizeYamlNonFinite(value: String): String = value match {
    case ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => ".inf"
    case "-.inf" | "-.Inf" | "-.INF"                            => "-.inf"
    case ".nan" | ".NaN" | ".NAN"                               => ".nan"
    case other                                                  => other
  }

  private def isYamlNonFiniteCoreForm(value: String): Boolean =
    regexMatches(YamlNonFiniteInfPattern, value) ||
    regexMatches(YamlNonFiniteNaNPattern, value)

  private def isSignedYamlNaN(value: String): Boolean =
    regexMatches(YamlSignedNonFiniteNaNPattern, value)

  private def parseYamlDecimalLong(value: String): ujson.Num =
    ujson.Num(java.lang.Long.parseLong(value).toDouble)

  private def nodeToJson(node: Node, input: String): ujson.Value = node match {
    case sn: Node.ScalarNode =>
      val normalizedNonFinite = normalizeYamlNonFiniteScalar(sn, input)
      if (sn.value.contains(":") && (sn.tag == Tag.int || sn.tag == Tag.float)) {
        ujson.Str(sn.value)
      } else if (normalizedNonFinite.isDefined) {
        ujson.Str(normalizedNonFinite.get)
      } else if (
        sn.tag == Tag.float && isSignedYamlNaN(sn.value) && !hasExplicitFloatTag(
          sn,
          input
        )
      ) {
        ujson.Str(sn.value)
      } else {
        val constructed = YamlDecoder.forAny.construct(sn).getOrElse("")
        constructed match {
          case null | None => ujson.Null
          case v: String
              if sn.tag == Tag.str && regexMatches(Yaml12OctalPattern, v) &&
              !isQuotedScalar(sn, input) =>
            val negative = v.charAt(0) == '-'
            val octalPart =
              if (negative || v.charAt(0) == '+') v.substring(3) else v.substring(2)
            val result = java.lang.Long.parseLong(octalPart.replace("_", ""), 8)
            ujson.Num((if (negative) -result else result).toDouble)
          case v: String
              if sn.tag == Tag.str && regexMatches(YamlInvalidOctalDecimalPattern, v) &&
              !isQuotedScalar(sn, input) =>
            parseYamlDecimalLong(v)
          case v: String =>
            ujson.read(s"\"${v.replace("\"", "\\\"").replace("\n", "\\n")}\"", false)
          case v: Boolean    => ujson.Bool(v)
          case v: Int        => ujson.Num(v.toDouble)
          case v: Long       => ujson.Num(v.toDouble)
          case v: Double     => ujson.Num(v)
          case v: Float      => ujson.Num(v.toDouble)
          case v: BigDecimal => ujson.Num(v.toDouble)
          case v: BigInt     => ujson.Num(v.toDouble)
          case v: Short      => ujson.Num(v.toDouble)
          case _ => Error.fail("Unsupported YAML scalar type: " + node.getClass.getSimpleName)
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
          case sn: Node.ScalarNode => buf(yamlScalarKey(sn, input)) = nodeToJson(value, input)
          case _ => Error.fail("Invalid YAML mapping key class: " + key.getClass.getSimpleName)
        }
      }
      ujson.Obj(buf)
    case _ =>
      Error.fail("Unsupported YAML node type: " + node.getClass.getSimpleName)
  }

  private def yamlScalarKey(sn: Node.ScalarNode, input: String): String =
    normalizeYamlNonFiniteScalar(sn, input).getOrElse(sn.value)

  private def normalizeYamlNonFiniteScalar(sn: Node.ScalarNode, input: String): Option[String] = {
    val isCoreNonFinite = isYamlNonFiniteCoreForm(sn.value)
    val isExplicitSignedNaN =
      sn.tag == Tag.float && isSignedYamlNaN(sn.value) && hasExplicitFloatTag(sn, input)
    val canNormalizeResolvedFloat =
      sn.tag == Tag.float && (isCoreNonFinite || isExplicitSignedNaN)
    val canNormalizePlainCoreString =
      isCoreNonFinite && sn.tag == Tag.str && isPlainScalar(sn, input) &&
      !hasExplicitStringTag(sn, input)
    if (canNormalizeResolvedFloat || canNormalizePlainCoreString) {
      Some {
        if (isExplicitSignedNaN) ".nan"
        else normalizeYamlNonFinite(sn.value)
      }
    } else {
      None
    }
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

  private def isPlainScalar(sn: Node.ScalarNode, input: String): Boolean = {
    sn.pos match {
      case Some(range) =>
        val offset = range.start.offset
        offset >= 0 && offset < input.length && {
          input.charAt(offset) match {
            case '"' | '\'' | '|' | '>' => false
            case _                      => true
          }
        }
      case None => false
    }
  }

  private def hasExplicitStringTag(sn: Node.ScalarNode, input: String): Boolean = {
    sn.pos match {
      case Some(range) => hasExplicitStringTagAt(range.start.offset, sn.value, input)
      case None        => false
    }
  }

  private def hasExplicitFloatTag(sn: Node.ScalarNode, input: String): Boolean = {
    sn.pos match {
      case Some(range) => hasExplicitFloatTagAt(range.start.offset, sn.value, input)
      case None        => false
    }
  }

  private def hasExplicitStringTagAt(offset: Int, value: String, input: String): Boolean = {
    if (offset < 0 || offset > input.length) false
    else {
      val valueOffset =
        if (value.isEmpty) offset
        else {
          val found = input.indexOf(value, offset)
          if (found >= 0) found else offset
        }
      val scanEnd = math.max(0, math.min(valueOffset, input.length))
      val lineStart = input.lastIndexOf('\n', math.max(0, math.min(offset, input.length) - 1)) + 1
      val prefixStart = yamlScalarPrefixStart(input, lineStart, scanEnd)
      val currentLineHasTag = containsExplicitStringTag(input.substring(prefixStart, scanEnd))
      val previousPropertyLineHasTag =
        isWhitespaceOnly(input, lineStart, scanEnd) &&
        previousPropertyLineHasExplicitStringTag(input, lineStart)
      currentLineHasTag || previousPropertyLineHasTag
    }
  }

  private def hasExplicitFloatTagAt(offset: Int, value: String, input: String): Boolean = {
    if (offset < 0 || offset > input.length) false
    else {
      val valueOffset =
        if (value.isEmpty) offset
        else {
          val found = input.indexOf(value, offset)
          if (found >= 0) found else offset
        }
      val scanEnd = math.max(0, math.min(valueOffset, input.length))
      val lineStart = input.lastIndexOf('\n', math.max(0, math.min(offset, input.length) - 1)) + 1
      val prefixStart = yamlScalarPrefixStart(input, lineStart, scanEnd)
      val currentLineHasTag = containsExplicitFloatTag(input.substring(prefixStart, scanEnd))
      val previousPropertyLineHasTag =
        isWhitespaceOnly(input, lineStart, scanEnd) &&
        previousPropertyLineHasExplicitFloatTag(input, lineStart)
      currentLineHasTag || previousPropertyLineHasTag
    }
  }

  private def isWhitespaceOnly(input: String, from: Int, until: Int): Boolean = {
    var i = from
    while (i < until) {
      if (!input.charAt(i).isWhitespace) return false
      i += 1
    }
    true
  }

  private def previousLineStart(input: String, lineStart: Int): Int = {
    val previousLineEnd = math.max(0, lineStart - 1)
    input.lastIndexOf('\n', math.max(0, previousLineEnd - 1)) + 1
  }

  private def previousPropertyLineHasExplicitStringTag(input: String, lineStart: Int): Boolean = {
    previousPropertyLineHasExplicitTag(input, lineStart, containsExplicitStringTag)
  }

  private def previousPropertyLineHasExplicitFloatTag(input: String, lineStart: Int): Boolean = {
    previousPropertyLineHasExplicitTag(input, lineStart, containsExplicitFloatTag)
  }

  private def previousPropertyLineHasExplicitTag(
      input: String,
      lineStart: Int,
      containsTag: String => Boolean
  ): Boolean = {
    var lineEnd = math.max(0, lineStart - 1)
    while (lineEnd > 0) {
      val propertyLineStart = previousLineStart(input, lineEnd + 1)
      if (!isBlankOrCommentLine(input, propertyLineStart, lineEnd)) {
        val prefixStart = yamlScalarPrefixStart(input, propertyLineStart, lineEnd)
        return containsTag(input.substring(prefixStart, lineEnd))
      }
      lineEnd = math.max(0, propertyLineStart - 1)
    }
    false
  }

  private def isBlankOrCommentLine(input: String, from: Int, until: Int): Boolean = {
    var i = from
    while (i < until && input.charAt(i).isWhitespace) i += 1
    i >= until || input.charAt(i) == '#'
  }

  private def yamlScalarPrefixStart(input: String, from: Int, until: Int): Int = {
    var i = from
    var start = from
    var inVerbatimTag = false
    var quoted: Char = 0
    while (i < until) {
      val c = input.charAt(i)
      if (quoted != 0) {
        if (c == quoted) {
          if (quoted == '\'' && i + 1 < until && input.charAt(i + 1) == '\'') i += 1
          else if (quoted != '"' || !isEscapedDoubleQuote(input, i, from)) quoted = 0
        }
      } else if (inVerbatimTag) {
        if (c == '>') inVerbatimTag = false
      } else if ((c == '"' || c == '\'') && isYamlScalarQuoteStart(input, start, i)) {
        quoted = c
      } else if (c == '#' && (i == from || input.charAt(i - 1).isWhitespace)) {
        return start
      } else if (c == '<' && i > from && input.charAt(i - 1) == '!') {
        inVerbatimTag = true
      } else if (c == '[' || c == ']' || c == '{' || c == '}' || c == ',' || c == ':') {
        start = i + 1
      }
      i += 1
    }
    start
  }

  private def isEscapedDoubleQuote(input: String, quoteIndex: Int, from: Int): Boolean = {
    var i = quoteIndex - 1
    var backslashes = 0
    while (i >= from && input.charAt(i) == '\\') {
      backslashes += 1
      i -= 1
    }
    (backslashes & 1) == 1
  }

  private def isYamlScalarQuoteStart(input: String, scalarStart: Int, quoteIndex: Int): Boolean = {
    var i = scalarStart
    while (true) {
      while (i < quoteIndex && input.charAt(i).isWhitespace) i += 1
      if (i >= quoteIndex) return true

      val tokenStart = i
      while (i < quoteIndex && !input.charAt(i).isWhitespace) i += 1
      val token = input.substring(tokenStart, i)
      if (!isYamlNodePropertyToken(token)) return false
    }
    false
  }

  private def isYamlNodePropertyToken(token: String): Boolean = {
    token.startsWith("!") || token.startsWith("&")
  }

  private def containsExplicitStringTag(prefix: String): Boolean =
    containsExplicitTag(prefix, token => token.startsWith("!"))

  private def containsExplicitFloatTag(prefix: String): Boolean =
    containsExplicitTag(prefix, isExplicitFloatTagToken)

  private def containsExplicitTag(prefix: String, matches: String => Boolean): Boolean = {
    var i = 0
    while (i < prefix.length) {
      while (i < prefix.length && prefix.charAt(i).isWhitespace) i += 1
      if (i >= prefix.length) return false
      val start = i
      while (i < prefix.length && !prefix.charAt(i).isWhitespace) i += 1
      val token = prefix.substring(start, i)
      if (token.startsWith("#")) return false
      if (token.startsWith("!") && matches(token)) return true
    }
    false
  }

  private def isExplicitFloatTagToken(token: String): Boolean =
    token == "!!float" ||
    token == "!<tag:yaml.org,2002:float>" ||
    // Shorthand handles are only accepted after scala-yaml has resolved the node as Tag.float.
    token.endsWith("!float")

  private val YamlDocStartPattern = "\\A\\s*---(?:[ \\t\\n\\r]|\\z)".r

  def yamlToJson(s: String): ujson.Value = {
    if (s.trim.isEmpty) return ujson.Null

    // Preprocess to add explicit nulls for empty documents,
    // since scala-yaml's parseManyYamls can't handle empty documents
    // (DocumentStart immediately followed by DocumentEnd).
    val preprocessed = addExplicitNullsForEmptyDocs(s)
    val hasExplicitDocStart = YamlDocStartPattern.findFirstIn(s).isDefined

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
