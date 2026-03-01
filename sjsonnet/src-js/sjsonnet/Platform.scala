package sjsonnet

import org.virtuslab.yaml.*

import java.util
import java.util.regex.Pattern
import scala.collection.mutable

object Platform {
  private def nodeToJson(node: Node): ujson.Value = node match {
    case _: Node.ScalarNode =>
      YamlDecoder.forAny.construct(node).getOrElse("") match {
        case null       => ujson.Null
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
}
