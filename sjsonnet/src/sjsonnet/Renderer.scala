package sjsonnet
import java.io.StringWriter
import java.util.regex.Pattern

import upickle.core.{ArrVisitor, ObjVisitor}
import ujson.BaseRenderer

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class Renderer(out: StringWriter = new java.io.StringWriter(),
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
  override def visitArray(length: Int, index: Int) = new ArrVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    out.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor = Renderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
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

  override def visitObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
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
    def visitValue(v: StringWriter, index: Int): Unit = {
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

  val newlinePattern = Pattern.compile("\n")
  val outBuffer = out.getBuffer()

  override def visitString(s: CharSequence, index: Int): StringWriter = {
    flushBuffer()
    val len = s.length()
    if (len == 0) out.append("\"\"")
    else if (s.charAt(len - 1) == '\n') {
      val splits = newlinePattern.split(s)
      out.append('|')
      depth += 1
      splits.foreach { split =>
        newlineBuffered = true
        flushBuffer()
        out.append(split) // TODO escaping?
      }
      depth -= 1
      out
    } else {
      ujson.Renderer.escape(out, s, unicode = true)
      out
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

    if (!topLevel) {
      depth += 1
      newlineBuffered = true
    }
    topLevel = false

    var dedentInObject = afterKey && !indentArrayInObject
    afterKey = false
    if (dedentInObject) depth -= 1
    dashBuffered = true

    def subVisitor = YamlRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      empty = false
      flushBuffer()
      newlineBuffered = true
      dashBuffered = true
    }
    def visitEnd(index: Int) = {
      if (!dedentInObject) depth -= 1
      if (empty) out.append("[]")
      newlineBuffered = false
      dashBuffered = false
      out
    }
  }
  override def visitObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
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

class PythonRenderer(out: StringWriter = new java.io.StringWriter(),
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
