package sjsonnet
import upickle.core.{ObjVisitor, ArrVisitor}
import scala.annotation.switch

/**
 * Vendored version of `ujson.BaseRenderer` from ujson 1.2.3.
 *
 * uJson has replaced this with a pair of byte/char specialized renderers for performance. For now,
 * we just want to upgrade uJson to the latest version to avoid classpath conflicts, so just vendor
 * this code for now. In the future we may remove it to interface with uJson's specialized renderers
 * directly, to benefit from their improved performance.
 */
class BaseRenderer[T <: java.io.Writer](out: T, indent: Int = -1, escapeUnicode: Boolean = false)
    extends ujson.JsVisitor[T, T] {

  var depth: Int = 0
  val colonSnippet: String = if (indent == -1) ":" else ": "

  var commaBuffered = false

  def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(',')
      renderIndent()
    }
  }

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[T, T] =
    visitObject(length, index)

  def visitArray(length: Int, index: Int): upickle.core.ArrVisitor[T, T] {
    def subVisitor: sjsonnet.BaseRenderer[T]
  } = new ArrVisitor[T, T] {
    flushBuffer()
    out.append('[')

    depth += 1
    renderIndent()
    def subVisitor: sjsonnet.BaseRenderer[T] = BaseRenderer.this
    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): T = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append(']')
      out
    }
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, T] = new ObjVisitor[T, T] {
    flushBuffer()
    out.append('{')
    depth += 1
    renderIndent()
    def subVisitor: sjsonnet.BaseRenderer[T] = BaseRenderer.this
    def visitKey(index: Int): sjsonnet.BaseRenderer[T] = BaseRenderer.this
    def visitKeyValue(s: Any): Unit = out.append(colonSnippet)
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): T = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append('}')
      out
    }
  }

  def visitNull(index: Int): T = {
    flushBuffer()
    out.append("null")
    out
  }

  def visitFalse(index: Int): T = {
    flushBuffer()
    out.append("false")
    out
  }

  def visitTrue(index: Int): T = {
    flushBuffer()
    out.append("true")
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): T = {
    flushBuffer()
    out.append(s)
    out
  }

  override def visitFloat64(d: Double, index: Int): T = {
    d match {
      case Double.PositiveInfinity        => visitString("Infinity", -1)
      case Double.NegativeInfinity        => visitString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitString("NaN", -1)
      case d                              =>
        val i = d.toLong
        if (d == i) {
          if (i == 0L && java.lang.Double.doubleToRawLongBits(d) != 0L)
            visitFloat64StringParts("-0", -1, -1, index)
          else
            visitFloat64StringParts(i.toString, -1, -1, index)
        }
        else super.visitFloat64(d, index)
        flushBuffer()
    }

    out
  }

  def visitString(s: CharSequence, index: Int): T = {
    flushBuffer()
    if (s == null) out.append("null")
    else BaseRenderer.escape(out, s, escapeUnicode)

    out
  }

  final def renderIndent(): Unit = {
    if (indent == -1) ()
    else {
      out.append('\n')
      var i = indent * depth
      while (i > 0) {
        out.append(' ')
        i -= 1
      }
    }
  }
}
object BaseRenderer {

  /**
   * Escape a string for JSON-style output into the given Writer, surrounded by double quotes.
   *
   * Implementation strategy: track contiguous runs of "safe" characters (those that pass through
   * verbatim) and emit each run via a single bulk `Writer.write(String, off, len)` call. This
   * collapses the per-character `Writer.write(int)` loop — which on `StringWriter` synchronizes and
   * bounds-checks per call — into one `System.arraycopy` per safe run, with no upfront pass.
   *
   * "Safe" characters are everything outside `"`, `\`, control chars `< 0x20`, and — when
   * `unicode = true` — chars `> 0x7E` (which would otherwise be escaped to `\\uXXXX`). The mapping
   * for the unsafe set is identical to the per-char path it replaces.
   *
   * Tight, branch-light, charAt-based loop: friendly to JIT inlining (HotSpot, GraalVM) and to
   * Scala Native's LLVM backend. Common case (ASCII-clean strings used by config and manifest
   * renderers — TOML, YAML, escapeStringJson) reduces to a single bulk write.
   */
  final def escape(sb: java.io.Writer, s: CharSequence, unicode: Boolean): Unit = {
    sb.append('"')
    val len = s.length
    s match {
      case str: String => escapeStringChunked(sb, str, len, unicode)
      case _           => escapeChars(sb, s, len, unicode)
    }
    sb.append('"')
  }

  /**
   * Chunked escape for `String` input: emit maximal runs of safe characters via bulk write, with
   * single-character escape mappings interleaved for unsafe characters.
   */
  private def escapeStringChunked(
      sb: java.io.Writer,
      str: String,
      len: Int,
      unicode: Boolean): Unit = {
    var i = 0
    var start = 0
    while (i < len) {
      val c = str.charAt(i)
      // Inlined classification, mirroring escapeChars below; `<` on a signed char is fine since
      // chars are unsigned 16-bit; 0x20 / 0x7E comparisons are valid for all values.
      if (c == '"' || c == '\\' || c < 0x20 || (unicode && c > 0x7e)) {
        if (i > start) sb.write(str, start, i - start)
        (c: @switch) match {
          case '"'  => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case _    =>
            sb.append("\\u")
              .append(toHex((c >> 12) & 15))
              .append(toHex((c >> 8) & 15))
              .append(toHex((c >> 4) & 15))
              .append(toHex(c & 15))
        }
        start = i + 1
      }
      i += 1
    }
    if (start < len) sb.write(str, start, len - start)
  }

  private def escapeChars(sb: java.io.Writer, s: CharSequence, len: Int, unicode: Boolean): Unit = {
    var i = 0
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c    =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.append("\\u")
              .append(toHex((c >> 12) & 15))
              .append(toHex((c >> 8) & 15))
              .append(toHex((c >> 4) & 15))
              .append(toHex(c & 15))
          } else sb.append(c)
      }
      i += 1
    }
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar
}
