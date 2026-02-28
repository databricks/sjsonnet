package sjsonnet

import java.io.{ByteArrayOutputStream, File}
import java.util
import java.util.Base64
import java.util.zip.GZIPOutputStream
import scala.scalanative.regex.Pattern
import scala.collection.mutable
import org.virtuslab.yaml.*

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
    // Split YAML multi-document stream manually, similar to SnakeYAML's loadAll
    // since parseManyYamls doesn't handle all cases correctly
    val documents = splitYamlDocuments(s)

    documents.size match {
      case 0 => ujson.Null
      case 1 => parseSingleDocument(documents.head)
      case _ =>
        val buf = new mutable.ArrayBuffer[ujson.Value](documents.size)
        for (doc <- documents) {
          buf += parseSingleDocument(doc)
        }
        ujson.Arr(buf)
    }
  }

  private def splitYamlDocuments(s: String): List[String] = {
    if (s.trim.isEmpty) return Nil

    // Split on document separator "---" at line start
    // But only if it's followed by whitespace or end of line
    val lines = s.split("\n", -1).toList
    val documents = mutable.ArrayBuffer[String]()
    val currentDoc = mutable.ArrayBuffer[String]()
    var isFirstDoc = true

    for (line <- lines) {
      val trimmed = line.trim
      // Check if this line starts with "---" and is followed by whitespace or end
      if (trimmed.startsWith("---") && (trimmed.length == 3 || trimmed.charAt(3).isWhitespace)) {
        // Save previous document if not empty
        if (currentDoc.nonEmpty || !isFirstDoc) {
          documents += currentDoc.mkString("\n")
        }
        currentDoc.clear()
        isFirstDoc = false
      } else {
        currentDoc += line
      }
    }

    // Add last document
    if (currentDoc.nonEmpty || documents.nonEmpty) {
      documents += currentDoc.mkString("\n")
    }

    documents.toList
  }

  private def parseSingleDocument(doc: String): ujson.Value = {
    val trimmed = doc.trim
    if (trimmed.isEmpty) {
      ujson.Null
    } else {
      // Use parseYaml for single document
      parseYaml(trimmed) match {
        case Right(node) => nodeToJson(node)
        case Left(e)     => Error.fail("Error converting YAML to JSON: " + e.getMessage)
      }
    }
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
}
