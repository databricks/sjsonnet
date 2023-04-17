package sjsonnet

// Vendored version of `ujson.BaseCharRenderer` from ujson 1.3.7
// with some private definitions made accessible to subclasses

import ujson._
import scala.annotation.switch
import upickle.core.{ArrVisitor, ObjVisitor}
class BaseCharRenderer[T <: upickle.core.CharOps.Output]
(out: T,
 indent: Int = -1,
 escapeUnicode: Boolean = false) extends JsVisitor[T, T]{
  protected[this] val elemBuilder = new upickle.core.CharBuilder
  protected[this] val unicodeCharBuilder = new upickle.core.CharBuilder()
  def flushCharBuilder() = {
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  protected[this] var depth: Int = 0

  protected[this] var visitingKey = false

  protected[this] var commaBuffered = false
  protected[this] var quoteBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.append(',')
      renderIndent()
    }
    if (quoteBuffered) {
      quoteBuffered = false
      elemBuilder.append('"')
    }
  }

  def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    renderIndent()
    def subVisitor = BaseCharRenderer.this
    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor = BaseCharRenderer.this
    def visitKey(index: Int) = {
      quoteBuffered = true
      visitingKey = true
      BaseCharRenderer.this
    }
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.append('"')
      visitingKey = false
      elemBuilder.append(':')
      if (indent != -1) elemBuilder.append(' ')
    }
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('l')
    flushCharBuilder()
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(5)
    elemBuilder.appendUnsafe('f')
    elemBuilder.appendUnsafe('a')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('s')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('t')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(s.length())
    var i = 0
    val sLength = s.length
    while(i < sLength){
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
    flushCharBuilder()
    out
  }

  override def visitFloat32(d: Float, index: Int) = {
    d match{
      case Float.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Float.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Float.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat32(d, index)
        flushBuffer()
    }
    flushCharBuilder()
    out
  }

  override def visitFloat64(d: Double, index: Int) = {
    d match{
      case Double.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Double.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat64(d, index)
        flushBuffer()
    }
    flushCharBuilder()
    out
  }

  def visitString(s: CharSequence, index: Int) = {

    if (s eq null) visitNull(index)
    else visitNonNullString(s, index)
  }

  def visitNonNullString(s: CharSequence, index: Int) = {
    flushBuffer()

    upickle.core.RenderUtils.escapeChar(
      unicodeCharBuilder, elemBuilder, s, escapeUnicode, !visitingKey
    )

    flushCharBuilder()
    out
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      elemBuilder.ensureLength(i + 1)
      elemBuilder.appendUnsafe('\n')
      while(i > 0) {
        elemBuilder.appendUnsafe(' ')
        i -= 1
      }
    }
  }
}

object BaseCharRenderer {
  final def escape(sb: java.io.Writer, s: CharSequence, unicode: Boolean): Unit = {
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.append("\\u").append(toHex((c >> 12) & 15)).append(toHex((c >> 8) & 15))
              .append(toHex((c >> 4) & 15)).append(toHex(c & 15))
          } else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar
}