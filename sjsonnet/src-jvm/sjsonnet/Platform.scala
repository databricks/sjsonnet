package sjsonnet

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util
import java.util.Base64
import java.util.HexFormat
import java.util.zip.GZIPOutputStream
import com.google.re2j.Pattern
import net.jpountz.xxhash.{StreamingXXHash64, XXHashFactory}
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import org.yaml.snakeyaml.{DumperOptions, LoaderOptions, Yaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode, SequenceNode, Tag}

import scala.annotation.nowarn
import scala.collection.compat.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Platform {
  private val hexFormat = HexFormat.of()

  def repeatString(s: String, count: Int): String =
    if (count <= 0) "" else s.repeat(count)

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

  /**
   * Valid compression levels are 0 (no compression) to 9 (maximum compression).
   */
  def xzBytes(b: Array[Byte], compressionLevel: Option[Int]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    // Set compression to specified level
    val level = compressionLevel.getOrElse(LZMA2Options.PRESET_DEFAULT)
    val xz: XZOutputStream = new XZOutputStream(outputStream, new LZMA2Options(level))
    try {
      xz.write(b)
    } finally {
      xz.close()
      outputStream.close()
    }
    Base64.getEncoder.encodeToString(outputStream.toByteArray)
  }

  def xzString(s: String, compressionLevel: Option[Int]): String = {
    xzBytes(s.getBytes(UTF_8), compressionLevel)
  }

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

  private def yamlNodeToJson(node: Node, input: String): ujson.Value = node match {
    case sn: ScalarNode =>
      val value = sn.getValue
      val tag = sn.getTag
      val isPlain = sn.getScalarStyle == DumperOptions.ScalarStyle.PLAIN
      val normalizedNonFinite = normalizeYamlNonFiniteScalar(sn, input)

      if (isPlain && regexMatches(Yaml12OctalPattern, value)) {
        val negative = value.charAt(0) == '-'
        val octalPart =
          if (negative || value.charAt(0) == '+') value.substring(3) else value.substring(2)
        val result = java.lang.Long.parseLong(octalPart.replace("_", ""), 8)
        val signed = if (negative) -result else result
        ujson.Num(signed.toDouble)
      } else if (isPlain && regexMatches(YamlInvalidOctalDecimalPattern, value)) {
        parseYamlDecimalLong(value)
      } else if (isPlain && value.contains(":") && (tag == Tag.INT || tag == Tag.FLOAT)) {
        ujson.Str(value)
      } else if (normalizedNonFinite.isDefined) {
        ujson.Str(normalizedNonFinite.get)
      } else if (tag == Tag.INT) {
        val cleaned = value.replace("_", "")
        val result: Long =
          if (cleaned.startsWith("0x") || cleaned.startsWith("-0x") || cleaned.startsWith("+0x")) {
            val negative = cleaned.startsWith("-")
            val hex =
              if (negative || cleaned.startsWith("+")) cleaned.substring(3)
              else cleaned.substring(2)
            val v = java.lang.Long.parseLong(hex, 16)
            if (negative) -v else v
          } else if (
            cleaned.startsWith("0b") || cleaned.startsWith("-0b") || cleaned.startsWith("+0b")
          ) {
            val negative = cleaned.startsWith("-")
            val bin =
              if (negative || cleaned.startsWith("+")) cleaned.substring(3)
              else cleaned.substring(2)
            val v = java.lang.Long.parseLong(bin, 2)
            if (negative) -v else v
          } else if (cleaned.length > 1 && cleaned.startsWith("0") && !cleaned.contains(".")) {
            val negative = cleaned.startsWith("-")
            val oct = if (negative || cleaned.startsWith("+")) cleaned.substring(1) else cleaned
            val v = java.lang.Long.parseLong(oct, 8)
            if (negative) -v else v
          } else {
            cleaned.toLong
          }
        ujson.Num(result.toDouble)
      } else if (tag == Tag.FLOAT && isSignedYamlNaN(value) && !hasExplicitFloatTag(sn, input)) {
        ujson.Str(value)
      } else if (tag == Tag.FLOAT) {
        val cleaned = value.replace("_", "")
        val result = cleaned match {
          case ".inf" | ".Inf" | ".INF"    => Double.PositiveInfinity
          case "+.inf" | "+.Inf" | "+.INF" => Double.PositiveInfinity
          case "-.inf" | "-.Inf" | "-.INF" => Double.NegativeInfinity
          case ".nan" | ".NaN" | ".NAN" | "+.nan" | "+.NaN" | "+.NAN" | "-.nan" | "-.NaN" |
              "-.NAN" =>
            Double.NaN
          case s => s.toDouble
        }
        if (java.lang.Double.isFinite(result)) ujson.Num(result)
        else ujson.Str(normalizeYamlNonFinite(value))
      } else if (tag == Tag.BOOL) {
        ujson.Bool(value.toLowerCase match {
          case "true" | "yes" | "on"  => true
          case "false" | "no" | "off" => false
          case _                      => Error.fail("Invalid YAML boolean: " + value)
        })
      } else if (tag == Tag.NULL) {
        ujson.Null
      } else {
        ujson.Str(value)
      }

    case mn: MappingNode =>
      val buf = upickle.core.LinkedHashMap[String, ujson.Value]()
      buf.sizeHint(mn.getValue.size)
      for (tuple <- mn.getValue.asScala) {
        val key = tuple.getKeyNode match {
          case sn: ScalarNode => yamlScalarKey(sn, input)
          case other          => Error.fail("Invalid YAML mapping key type: " + other.getTag)
        }
        buf(key) = yamlNodeToJson(tuple.getValueNode, input)
      }
      ujson.Obj(buf)

    case sn: SequenceNode =>
      val buf = new mutable.ArrayBuffer[ujson.Value](sn.getValue.size)
      for (n <- sn.getValue.asScala) {
        buf += yamlNodeToJson(n, input)
      }
      ujson.Arr(buf)

    case _ =>
      Error.fail("Unsupported YAML node type: " + node.getClass.getSimpleName)
  }

  private def yamlScalarKey(sn: ScalarNode, input: String): String =
    normalizeYamlNonFiniteScalar(sn, input).getOrElse(sn.getValue)

  private def normalizeYamlNonFiniteScalar(sn: ScalarNode, input: String): Option[String] = {
    val value = sn.getValue
    val tag = sn.getTag
    val isPlain = sn.getScalarStyle == DumperOptions.ScalarStyle.PLAIN
    val isCoreNonFinite = isYamlNonFiniteCoreForm(value)
    val isExplicitSignedNaN =
      tag == Tag.FLOAT && isSignedYamlNaN(value) && hasExplicitFloatTag(sn, input)
    val canNormalizeResolvedFloat =
      tag == Tag.FLOAT && (isCoreNonFinite || isExplicitSignedNaN)
    val canNormalizePlainCoreString =
      isCoreNonFinite && isPlain && tag == Tag.STR && !isQuotedScalar(sn, input) &&
      !hasExplicitStringTag(sn, input)
    if (canNormalizeResolvedFloat || canNormalizePlainCoreString) {
      Some {
        if (isExplicitSignedNaN) ".nan"
        else normalizeYamlNonFinite(value)
      }
    } else {
      None
    }
  }

  private def isQuotedScalar(sn: ScalarNode, input: String): Boolean = {
    val mark = sn.getStartMark
    if (mark == null) false
    else {
      val offset = mark.getIndex
      def isQuoteAt(i: Int): Boolean =
        i >= 0 && i < input.length && {
          val c = input.charAt(i)
          c == '"' || c == '\''
        }
      isQuoteAt(offset) || isQuoteAt(offset - 1)
    }
  }

  private def hasExplicitStringTag(sn: ScalarNode, input: String): Boolean = {
    val mark = sn.getStartMark
    if (mark == null) false
    else hasExplicitStringTagAt(mark.getIndex, sn.getValue, input)
  }

  private def hasExplicitFloatTag(sn: ScalarNode, input: String): Boolean = {
    val mark = sn.getStartMark
    if (mark == null) false
    else hasExplicitFloatTagAt(mark.getIndex, sn.getValue, input)
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
      containsExplicitStringTag(input.substring(prefixStart, scanEnd))
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
      containsExplicitFloatTag(input.substring(prefixStart, scanEnd))
    }
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
    // Shorthand handles are only accepted after SnakeYAML has resolved the node as Tag.FLOAT.
    token.endsWith("!float")

  private val YamlDocStartPattern =
    "\\A\\s*---(?:[ \\t\\n\\r]|\\z)".r

  def yamlToJson(yamlString: String): ujson.Value = {
    try {
      val yaml = new Yaml(new LoaderOptions())
      val docs = yaml.composeAll(new java.io.StringReader(yamlString)).asScala.toSeq
      val hasExplicitDocStart = YamlDocStartPattern.findFirstIn(yamlString).isDefined
      docs.size match {
        case 0                         => ujson.Null
        case 1 if !hasExplicitDocStart => yamlNodeToJson(docs.head, yamlString)
        case _                         =>
          val buf = new mutable.ArrayBuffer[ujson.Value](docs.size)
          for (doc <- docs) {
            buf += yamlNodeToJson(doc, yamlString)
          }
          ujson.Arr(buf)
      }
    } catch {
      case e: sjsonnet.Error => throw e
      case e: Exception      =>
        Error.fail("Error converting YAML to JSON: " + e)
    }
  }

  private def computeHash(algorithm: String, s: String): String =
    hexFormat.formatHex(
      java.security.MessageDigest.getInstance(algorithm).digest(s.getBytes(UTF_8))
    )

  def md5(s: String): String = computeHash("MD5", s)

  def sha1(s: String): String = computeHash("SHA-1", s)

  def sha256(s: String): String = computeHash("SHA-256", s)

  def sha512(s: String): String = computeHash("SHA-512", s)

  // Same as go-jsonnet https://github.com/google/go-jsonnet/blob/2b4d7535f540f128e38830492e509a550eb86d57/builtins.go#L959
  def sha3(s: String): String = computeHash("SHA3-512", s)

  private val xxHashFactory = XXHashFactory.fastestInstance()

  def hashBytes(bytes: Array[Byte]): String =
    xxHashFactory.hash64().hash(bytes, 0, bytes.length, 0).toString

  def hashFile(file: File): String = {
    val buffer = new Array[Byte](8192)
    val hash: StreamingXXHash64 = xxHashFactory.newStreamingHash64(0)

    val fis = new FileInputStream(file)
    val bis = new BufferedInputStream(fis)

    try {
      var bytesRead = bis.read(buffer)
      while (bytesRead != -1) {
        hash.update(buffer, 0, bytesRead)
        bytesRead = bis.read(buffer)
      }
    } finally {
      bis.close()
      fis.close()
    }

    hash.getValue.toString
  }

  private val regexCache = new util.concurrent.ConcurrentHashMap[String, Pattern]
  private val dashPattern = getPatternFromCache("-")

  def getPatternFromCache(pat: String): Pattern =
    regexCache.computeIfAbsent(pat, _ => Pattern.compile(pat))

  def getNamedGroupsMap(pat: Pattern): Map[String, Int] =
    pat.namedGroups().asScala.view.mapValues(_.intValue()).toMap

  def regexQuote(s: String): String = {
    val quote = Pattern.quote(s)
    val matcher = dashPattern.matcher(quote)
    if (matcher.find()) {
      matcher.replaceAll("\\\\-")
    } else {
      quote
    }
  }

  def appendMemoryStats(sb: StringBuilder): Unit = {
    val rt = Runtime.getRuntime
    val usedBytes = rt.totalMemory() - rt.freeMemory()
    val maxBytes = rt.maxMemory()
    sb.append(formatBytesLine("heap_used", usedBytes))
    sb.append(formatBytesLine("heap_max", maxBytes))
  }

  private def formatBytesLine(label: String, bytes: Long): String =
    f"$label%-25s ${formatBytes(bytes)}%s%n"

  private def formatBytes(bytes: Long): String = {
    if (bytes < 1024L) s"${bytes}B"
    else if (bytes < 1024L * 1024) f"${bytes / 1024.0}%.1fKB"
    else if (bytes < 1024L * 1024 * 1024) f"${bytes / (1024.0 * 1024)}%.1fMB"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.2fGB"
  }
}
