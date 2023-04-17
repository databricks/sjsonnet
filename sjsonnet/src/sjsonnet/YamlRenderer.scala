package sjsonnet

import java.io.{StringWriter, Writer}
import java.util.regex.Pattern

import upickle.core.{ArrVisitor, ObjVisitor}



class YamlRenderer(_out: StringWriter = new java.io.StringWriter(), indentArrayInObject: Boolean = false,
                   indent: Int = 2) extends BaseCharRenderer(_out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterKey = false
  private var topLevel = true

  private val outBuffer = _out.getBuffer()

  override def flushCharBuilder() = {
    elemBuilder.writeOutToIfLongerThan(_out, if (depth <= 0 || topLevel) 0 else 1000)
  }

  private[this] def appendString(s: String) = {
    val len = s.length
    var i = 0
    elemBuilder.ensureLength(len)
    while(i < len) {
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
  }

  override def visitString(s: CharSequence, index: Int): StringWriter = {
    flushBuffer()
    val len = s.length()
    if (len == 0) {
      elemBuilder.ensureLength(2)
      elemBuilder.append('"')
      elemBuilder.append('"')
    } else if (s.charAt(len - 1) == '\n') {
      val splits = YamlRenderer.newlinePattern.split(s)
      elemBuilder.append('|')
      depth += 1
      splits.foreach { split =>
        newlineBuffered = true
        flushBuffer()
        appendString(split) // TODO escaping?
      }
      depth -= 1
    } else {
      upickle.core.RenderUtils.escapeChar(unicodeCharBuilder, elemBuilder, s, true, true)
    }
    flushCharBuilder()
    _out
  }

  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    appendString(RenderUtils.renderDouble(d))
    flushCharBuilder()
    _out
  }

  override def flushBuffer() = {
    if (newlineBuffered) {
      // drop space between colon and newline
      elemBuilder.writeOutToIfLongerThan(_out, 0)
      if (outBuffer.length() > 1 && outBuffer.charAt(outBuffer.length() - 1) == ' ') {
        outBuffer.setLength(outBuffer.length() - 1)
      }
      YamlRenderer.writeIndentation(elemBuilder, indent * depth)
      flushCharBuilder()
    }
    if (dashBuffered) {
      elemBuilder.append('-')
      elemBuilder.append(' ')
      flushCharBuilder()
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

    val dedentInObject = afterKey && !indentArrayInObject
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
      if (empty) {
        elemBuilder.ensureLength(2)
        elemBuilder.append('[')
        elemBuilder.append(']')
      }
      newlineBuffered = false
      dashBuffered = false
      flushCharBuilder()
      _out
    }
  }
  override def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
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
      elemBuilder.ensureLength(2)
      elemBuilder.append(':')
      elemBuilder.append(' ')
      flushCharBuilder()
      afterKey = true
      newlineBuffered = false
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      newlineBuffered = true
      afterKey = false
    }
    def visitEnd(index: Int) = {
      if (empty) {
        elemBuilder.ensureLength(2)
        elemBuilder.append('{')
        elemBuilder.append('}')
      }
      newlineBuffered = false
      depth -= 1
      flushCharBuilder()
      flushBuffer()
      _out
    }
  }
}
object YamlRenderer{
  val newlinePattern = Pattern.compile("\n")

  def writeIndentation(out: upickle.core.CharBuilder, n: Int) = {
    out.ensureLength(n+1)
    out.append('\n')
    var i = n
    while(i > 0) {
      out.append(' ')
      i -= 1
    }
  }
}
