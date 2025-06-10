package sjsonnet

import org.virtuslab.yaml.*

import java.io.File
import java.util
import java.util.regex.Pattern
import scala.collection.mutable

object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala.js")
  }
  def xzBytes(s: Array[Byte], compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }
  def xzString(s: String, compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala.js")
  }

  private def nodeToJson(node: Node): ujson.Value = node match {
    case _: Node.ScalarNode =>
      YamlDecoder.forAny.construct(node).getOrElse("") match {
        case null          => ujson.Null
        case v: String     => ujson.Str(v.replaceAll("\\\\n", "\n"))
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
    val docs = s.split("---\\s*\n")
    docs.size match {
      case 0 => ujson.Obj()
      case 1 =>
        docs.head.asNode match {
          case Right(n) =>
            nodeToJson(n)
          case Left(e) if docs.head.trim.isEmpty =>
            ujson.Obj()
          case Left(e) =>
            Error.fail("Error converting YAML to JSON: " + e.getMessage)
        }
      case _ =>
        val buf = new mutable.ArrayBuffer[ujson.Value](docs.size)
        for (doc <- docs) {
          doc.asNode match {
            case Right(n)                          => buf += nodeToJson(n)
            case Left(e) if docs.head.trim.isEmpty =>
            case Left(e) =>
              Error.fail("Error converting YAML to JSON: " + e.getMessage)
          }
        }
        ujson.Arr(buf)
    }
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
  def hashFile(file: File): String = {
    throw new Exception("hashFile not implemented in Scala.js")
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
}
