package sjsonnet
import java.io.{StringWriter, Writer}
import java.util.regex.Pattern

import upickle.core.{ArrVisitor, ObjVisitor}
import ujson.BaseRenderer

import scala.annotation.switch

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class Renderer(out: Writer = new java.io.StringWriter(),
               indent: Int = -1) extends BaseRenderer(out, indent){
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }
  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (commaBuffered) {
      if (indent == -1) out.append(", ")
      else out.append(',')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      out.append('\n')

      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    newlineBuffered = false
    commaBuffered = false
  }
  override def visitArray(length: Int, index: Int) = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor = Renderer.this
    def visitValue(v: Writer, index: Int): Unit = {
      empty = false
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append(']')
      out
    }
  }

  override def visitObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor = Renderer.this
    def visitKey(index: Int) = Renderer.this
    def visitKeyValue(v: Any): Unit = {
      empty = false
      flushBuffer()
      out.append(colonSnippet)
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append('}')
      out
    }
  }
}

class YamlRenderer(out: StringWriter = new java.io.StringWriter(), indentArrayInObject: Boolean = false,
                   indent: Int = 2) extends BaseRenderer(out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterKey = false
  var topLevel = true
  var firstElementInArray = false
  val newlinePattern = Pattern.compile("\n")
  val outBuffer = out.getBuffer()
  def leftHandPrefixOffset = {
    var i = 0
    while({
      val index = outBuffer.length - 1 - i
      if (index < 0) false
      else outBuffer.charAt(index) match{
        case '\n' => false
        case _ => i += 1; true
      }
    })()
    i
  }
  override def visitString(s: CharSequence, index: Int): StringWriter = {
    flushBuffer()
    val len = s.length()
    if (len == 0) out.append("''")
    else if (s.charAt(len - 1) == '\n') {
      val splits = newlinePattern.split(s)
      out.append('|')
      depth += 1
      splits.foreach { split =>
        newlineBuffered = true
        val oldDepth = depth
        if (split == "") depth = 0
        flushBuffer()
        depth = oldDepth
        out.append(split) // TODO escaping?
      }
      depth -= 1
      out
    } else if (
      // http://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html
      !s.toString.contains(": ") &&
      !s.toString.startsWith("!") &&  // ! Tag like !!null
      !s.toString.startsWith("&") &&  // & Anchor like &mapping_for_later_use
      !s.toString.startsWith("*") &&  // * Alias like *mapping_for_later_use
      !s.toString.startsWith("- ") && // -<space> Block sequence entry
      !s.toString.startsWith(": ") && // :<space> Block mapping entry
      !s.toString.startsWith("? ") && // ?<space> Explicit mapping key
      !s.toString.startsWith("? ") && // ?<space> Explicit mapping key
      !s.toString.startsWith("}") && // {, }, [, ] Flow mapping or sequence
      !s.toString.startsWith("}") &&
      !s.toString.startsWith("[") &&
      !s.toString.startsWith("]") &&
      !s.toString.startsWith(",") && // , Flow Collection entry seperator
      !s.toString.startsWith(",") && // , Flow Collection entry seperator
      !s.toString.startsWith("#") && // # Comment
      !s.toString.startsWith("|") && // |, > Block Scalar
      !s.toString.startsWith(">") &&
      !s.toString.startsWith("@") && // @, '`' (backtick) Reserved characters
      !s.toString.startsWith("`") &&
      !s.toString.startsWith("\"") && // ", ' Double and single quote
      // https://makandracards.com/makandra/24809-yaml-keys-like-yes-or-no-evaluate-to-true-and-false
      // y|Y|yes|Yes|YES|n|N|no|No|NO
      // |true|True|TRUE|false|False|FALSE
      // |on|On|ON|off|Off|OFF
      s.toString != "y" && s.toString != "Y" && s.toString != "yes" && s.toString != "Yes" && s.toString != "YES" &&
      s.toString != "n" && s.toString != "N" && s.toString != "no" && s.toString != "No" && s.toString != "NO" &&
      s.toString != "true" && s.toString != "True" && s.toString != "TRUE" &&
      s.toString != "false" && s.toString != "False" && s.toString != "FALSE" &&
      s.toString != "on" && s.toString != "On" && s.toString != "ON" &&
      s.toString != "off" && s.toString != "Off" && s.toString != "OFF" &&
      (try{ s.toString.toDouble; false} catch{case e: Throwable => true}) &&
      s.toString != "null" && s.toString != "~"
    ) {
      wrapString(s.toString)
      out
    } else {
      val str = new StringWriter
      ujson.Renderer.escape(str, s, unicode = true)
      wrapString("'" + s.toString + "'")
      out
    }
  }
  def wrapString(s: String) = {
    val chunks = s.split(' ')
    var currentOffset = leftHandPrefixOffset
    var firstInLine = true
    for(chunk <- chunks) {
      if (!firstInLine && currentOffset + chunk.length + 1 > 90){
        out.write("\n")
        out.write(" " * (indent * (depth + 1)))
        out.write(chunk)
        currentOffset = indent * (depth + 1)
      }else{
        if (firstInLine) firstInLine = false
        else {
          out.write(" ")
          currentOffset += 1
        }
        out.write(chunk)
        currentOffset += chunk.length
      }

    }
  }
  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }
  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (newlineBuffered) {
      // drop space between colon and newline
      if (outBuffer.length() > 1 && outBuffer.charAt(outBuffer.length() - 1) == ' ') {
        outBuffer.setLength(outBuffer.length() - 1)
      }
      out.append('\n')

      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    if (dashBuffered) {
      out.append("- ")
    }
    dashBuffered = false
    newlineBuffered = false
    dashBuffered = false
  }
  override def visitArray(length: Int, index: Int) = new ArrVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    val outerFirstElementInArray = firstElementInArray
    firstElementInArray = true
    if (!topLevel) {
      depth += 1
      if (!firstElementInArray || !outerFirstElementInArray)  newlineBuffered = true
    }
    topLevel = false

    var dedentInObject = afterKey && !indentArrayInObject
    afterKey = false
    if (dedentInObject) depth -= 1
    dashBuffered = true

    def subVisitor = YamlRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {

      firstElementInArray = true
      empty = false
      flushBuffer()
      newlineBuffered = true

      dashBuffered = true
    }
    def visitEnd(index: Int) = {
      firstElementInArray = false
      if (!dedentInObject) depth -= 1
      if (empty) out.append("[]")
      newlineBuffered = false
      dashBuffered = false
      out
    }
  }
  override def visitObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
    firstElementInArray = false
    var empty = true
    flushBuffer()
    if (!topLevel) depth += 1
    topLevel = false

    if (afterKey) newlineBuffered = true

    def subVisitor = YamlRenderer.this
    def visitKey(index: Int) = YamlRenderer.this
    def visitKeyValue(s: Any): Unit = {
      empty = false
      flushBuffer()
      out.append(colonSnippet)
      afterKey = true
      newlineBuffered = false
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      newlineBuffered = true
      afterKey = false
    }
    def visitEnd(index: Int) = {
      if (empty) out.append("{}")
      newlineBuffered = false
      depth -= 1
      flushBuffer()
      out
    }
  }
}

class PythonRenderer(out: Writer = new java.io.StringWriter(),
                     indent: Int = -1) extends BaseRenderer(out, indent){

  override def visitNull(index: Int) = {
    flushBuffer()
    out.append("None")
    out
  }

  override def visitFalse(index: Int) = {
    flushBuffer()
    out.append("False")
    out
  }

  override def visitTrue(index: Int) = {
    flushBuffer()
    out.append("True")
    out
  }

  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(", ")
      renderIndent()
    }
  }
}


object RenderUtils {
  /**
    * Custom rendering of Doubles used in rendering
    */
  def renderDouble(d: Double): String = {
    if (d.toLong == d) d.toLong.toString
    else if (d % 1 == 0) {
      BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
    }
    else d.toString
  }
}
