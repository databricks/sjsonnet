package sjsonnet
import upickle.core.{ObjVisitor, ArrVisitor}
import scala.annotation.switch

/**
 * Vendored version of `ujson.BaseRenderer` from ujson 1.2.3.
 *
 * uJson has replaced this with a pair of byte/char specialized renderers for
 * performance. For now, we just want to upgrade uJson to the latest version
 * to avoid classpath conflicts, so just vendor this code for now. In the future
 * we may remove it to interface with uJson's specialized renderers directly, to
 * benefit from their improved performance.
 */
class BaseRenderer[T <: java.io.Writer]
                  (out: T,
                   indent: Int = -1,
                   escapeUnicode: Boolean = false) extends ujson.JsVisitor[T, T]{
  var depth: Int = 0
  val colonSnippet = if (indent == -1) ":" else ": "

  var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(',')
      renderIndent()
    }
  }
  def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    out.append('[')

    depth += 1
    renderIndent()
    def subVisitor = BaseRenderer.this
    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append(']')
      out
    }
  }

  def visitObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    out.append('{')
    depth += 1
    renderIndent()
    def subVisitor = BaseRenderer.this
    def visitKey(index: Int) = BaseRenderer.this
    def visitKeyValue(s: Any): Unit = out.append(colonSnippet)
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append('}')
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    out.append("null")
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    out.append("false")
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    out.append("true")
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    out.append(s)
    out
  }

  override def visitFloat64(d: Double, index: Int) = {
    d match{
      case Double.PositiveInfinity => visitString("Infinity", -1)
      case Double.NegativeInfinity => visitString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat64(d, index)
        flushBuffer()
    }

    out
  }

  def visitString(s: CharSequence, index: Int) = {
    flushBuffer()
    if (s == null) out.append("null")
    else BaseRenderer.escape(out, s, escapeUnicode)

    out
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      out.append('\n')
      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
  }
}
object BaseRenderer {
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