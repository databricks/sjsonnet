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

  private val Yaml12OctalPattern = java.util.regex.Pattern.compile("[-+]?0o[0-7]+")

  private def yamlNodeToJson(node: Node): ujson.Value = node match {
    case sn: ScalarNode =>
      val value = sn.getValue
      val tag = sn.getTag
      val isPlain = sn.getScalarStyle == DumperOptions.ScalarStyle.PLAIN

      if (isPlain && Yaml12OctalPattern.matcher(value).matches()) {
        val negative = value.charAt(0) == '-'
        val octalPart =
          if (negative || value.charAt(0) == '+') value.substring(3) else value.substring(2)
        val result = java.lang.Long.parseUnsignedLong(octalPart, 8)
        val signed = if (negative) -result else result
        ujson.Num(signed.toDouble)
      } else if (tag == Tag.INT) {
        val cleaned = value.replace("_", "")
        val result: Long =
          if (cleaned.startsWith("0x") || cleaned.startsWith("-0x") || cleaned.startsWith("+0x")) {
            val negative = cleaned.startsWith("-")
            val hex =
              if (negative || cleaned.startsWith("+")) cleaned.substring(3)
              else cleaned.substring(2)
            val v = java.lang.Long.parseUnsignedLong(hex, 16)
            if (negative) -v else v
          } else if (
            cleaned.startsWith("0b") || cleaned.startsWith("-0b") || cleaned.startsWith("+0b")
          ) {
            val negative = cleaned.startsWith("-")
            val bin =
              if (negative || cleaned.startsWith("+")) cleaned.substring(3)
              else cleaned.substring(2)
            val v = java.lang.Long.parseUnsignedLong(bin, 2)
            if (negative) -v else v
          } else if (cleaned.length > 1 && cleaned.startsWith("0") && !cleaned.contains(".")) {
            val negative = cleaned.startsWith("-")
            val oct = if (negative || cleaned.startsWith("+")) cleaned.substring(1) else cleaned
            val v = java.lang.Long.parseUnsignedLong(oct, 8)
            if (negative) -v else v
          } else if (cleaned.contains(":")) {
            val parts = cleaned.split(":")
            parts.foldLeft(0L)((acc, p) => acc * 60 + p.trim.toLong)
          } else {
            cleaned.toLong
          }
        ujson.Num(result.toDouble)
      } else if (tag == Tag.FLOAT) {
        val cleaned = value.replace("_", "")
        val result = cleaned match {
          case ".inf" | ".Inf" | ".INF"    => Double.PositiveInfinity
          case "-.inf" | "-.Inf" | "-.INF" => Double.NegativeInfinity
          case ".nan" | ".NaN" | ".NAN"    => Double.NaN
          case s if s.contains(":")        =>
            s.split(":").foldLeft(0.0)((acc, p) => acc * 60 + p.trim.toDouble)
          case s => s.toDouble
        }
        ujson.Num(result)
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
          case sn: ScalarNode => sn.getValue
          case other          => Error.fail("Invalid YAML mapping key type: " + other.getTag)
        }
        buf(key) = yamlNodeToJson(tuple.getValueNode)
      }
      ujson.Obj(buf)

    case sn: SequenceNode =>
      val buf = new mutable.ArrayBuffer[ujson.Value](sn.getValue.size)
      for (n <- sn.getValue.asScala) {
        buf += yamlNodeToJson(n)
      }
      ujson.Arr(buf)

    case _ =>
      Error.fail("Unsupported YAML node type: " + node.getClass.getSimpleName)
  }

  private val YamlDocStartPattern =
    java.util.regex.Pattern.compile("\\A\\s*---(?:[ \\t\\n\\r]|\\z)")

  def yamlToJson(yamlString: String): ujson.Value = {
    try {
      val yaml = new Yaml(new LoaderOptions())
      val docs = yaml.composeAll(new java.io.StringReader(yamlString)).asScala.toSeq
      val hasExplicitDocStart = YamlDocStartPattern.matcher(yamlString).find()
      docs.size match {
        case 0                         => ujson.Null
        case 1 if !hasExplicitDocStart => yamlNodeToJson(docs.head)
        case _                         =>
          val buf = new mutable.ArrayBuffer[ujson.Value](docs.size)
          for (doc <- docs) {
            buf += yamlNodeToJson(doc)
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
