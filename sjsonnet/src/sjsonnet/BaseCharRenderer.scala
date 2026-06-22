package sjsonnet

// Vendored version of `ujson.BaseCharRenderer` from ujson 1.3.7
// with some private definitions made accessible to subclasses

import ujson._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object BaseCharRenderer {

  /**
   * Maximum nesting depth for pre-computed indent arrays. Depths beyond this fall back to
   * per-character rendering. 32 covers the vast majority of real-world Jsonnet output; even deeply
   * nested configurations rarely exceed 20 levels.
   */
  final val MaxCachedDepth = 32

  /**
   * Reusable scratch buffer for writeLongDirect (max 20 chars for Long.MinValue). Not thread-safe,
   * but renderers are single-threaded.
   */
  private[sjsonnet] val scratchBuf: Array[Char] = new Array[Char](20)

  /** Digit-pair lookup tables for two-digits-at-a-time integer rendering. */
  private[sjsonnet] val DIGIT_TENS: Array[Char] = {
    val a = new Array[Char](100)
    var i = 0
    while (i < 100) { a(i) = ('0' + i / 10).toChar; i += 1 }
    a
  }
  private[sjsonnet] val DIGIT_ONES: Array[Char] = {
    val a = new Array[Char](100)
    var i = 0
    while (i < 100) { a(i) = ('0' + i % 10).toChar; i += 1 }
    a
  }

  private[sjsonnet] val HEX_CHARS: Array[Char] =
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
}

class BaseCharRenderer[T <: upickle.core.CharOps.Output](
    out: T,
    indent: Int = -1,
    escapeUnicode: Boolean = false,
    newline: Array[Char] = Array('\n'),
    indentStr: Array[Char] = null)
    extends JsVisitor[T, T] {

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[T, T] =
    visitObject(length, index)

  private val indentChars: Array[Char] =
    if (indentStr != null) indentStr
    else if (indent > 0) { val a = new Array[Char](indent); java.util.Arrays.fill(a, ' '); a }
    else Array.empty[Char]

  protected val elemBuilder = new upickle.core.CharBuilder
  def flushCharBuilder(): Unit = {
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  protected var depth: Int = 0

  protected var commaBuffered = false

  /**
   * Pre-computed indent arrays: indentCache(d) = newline + indentChars*d. Used by [[renderIndent]]
   * to replace the per-character loop with a single bulk `System.arraycopy`, which is a significant
   * win on Scala Native (no JIT to unroll the loop) and measurable even on JVM for
   * materialization-heavy workloads.
   */
  protected val indentCache: Array[Array[Char]] =
    if (indent <= 0) null
    else {
      val maxDepth = BaseCharRenderer.MaxCachedDepth
      val arr = new Array[Array[Char]](maxDepth)
      val ic = indentChars
      val icLen = ic.length
      var d = 0
      while (d < maxDepth) {
        val indentLen = icLen * d
        val totalLen = newline.length + indentLen
        val buf = new Array[Char](totalLen)
        System.arraycopy(newline, 0, buf, 0, newline.length)
        var j = 0
        while (j < d) {
          System.arraycopy(ic, 0, buf, newline.length + j * icLen, icLen)
          j += 1
        }
        arr(d) = buf
        d += 1
      }
      arr
    }

  def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.append(',')
      renderIndent()
    }
  }

  def visitArray(length: Int, index: Int): ArrVisitor[T, T] = new ArrVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    renderIndent()

    def subVisitor: Visitor[T, T] = BaseCharRenderer.this

    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }

    def visitEnd(index: Int): T = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, T] = new ObjVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()

    def subVisitor: Visitor[T, T] = BaseCharRenderer.this

    def visitKey(index: Int): Visitor[T, T] = BaseCharRenderer.this

    def visitKeyValue(s: Any): Unit = {
      elemBuilder.append(':')
      if (indent != -1) elemBuilder.append(' ')
    }

    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }

    def visitEnd(index: Int): T = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }

  def visitNull(index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('l')
    flushCharBuilder()
    out
  }

  def visitFalse(index: Int): T = {
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

  def visitTrue(index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('t')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(s.length())
    var i = 0
    val sLength = s.length
    while (i < sLength) {
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
    flushCharBuilder()
    out
  }

  override def visitFloat64(d: Double, index: Int): T = {
    d match {
      case Double.PositiveInfinity        => visitNonNullString("Infinity", -1)
      case Double.NegativeInfinity        => visitNonNullString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitNonNullString("NaN", -1)
      case d                              =>
        val l = d.toLong
        if (RenderUtils.isExactLongDouble(d, l)) writeLongDirect(l)
        else {
          flushBuffer()
          appendString(RenderUtils.renderDouble(d))
          flushBuffer()
        }
    }
    flushCharBuilder()
    out
  }

  /**
   * Write a long integer directly into elemBuilder without intermediate String allocation. Uses
   * digit-pair lookup table for fast two-digits-at-a-time conversion.
   */
  protected def writeLongDirect(v: Long): Unit = {
    flushBuffer()
    if (v == 0L) {
      elemBuilder.ensureLength(1)
      elemBuilder.appendUnsafe('0')
      return
    }
    if (v == Long.MinValue) {
      visitFloat64StringParts("-9223372036854775808", -1, -1, -1)
      return
    }
    val negative = v < 0
    var abs = if (negative) -v else v
    // Write digits backward into a small local buffer, then bulk-copy.
    // Max Long digits = 19, plus sign = 20.
    val buf = BaseCharRenderer.scratchBuf
    var pos = 20
    while (abs >= 100) {
      val q = abs / 100
      val r = (abs - q * 100L).toInt
      abs = q
      pos -= 2
      buf(pos + 1) = BaseCharRenderer.DIGIT_ONES(r)
      buf(pos) = BaseCharRenderer.DIGIT_TENS(r)
    }
    if (abs >= 10) {
      val r = abs.toInt
      pos -= 2
      buf(pos + 1) = BaseCharRenderer.DIGIT_ONES(r)
      buf(pos) = BaseCharRenderer.DIGIT_TENS(r)
    } else {
      pos -= 1
      buf(pos) = ('0' + abs.toInt).toChar
    }
    if (negative) { pos -= 1; buf(pos) = '-' }
    val totalLen = 20 - pos
    elemBuilder.ensureLength(totalLen)
    val cbArr = elemBuilder.arr
    val startPos = elemBuilder.getLength
    System.arraycopy(buf, pos, cbArr, startPos, totalLen)
    elemBuilder.length = startPos + totalLen
  }

  def visitString(s: CharSequence, index: Int): T = {

    if (s eq null) visitNull(index)
    else visitNonNullString(s, index)
  }

  private def visitNonNullString(s: CharSequence, index: Int) = {
    flushBuffer()
    s match {
      case str: String if !escapeUnicode =>
        val len = str.length
        if (!CharSWAR.hasEscapeChar(str)) {
          elemBuilder.ensureLength(len + 2)
          elemBuilder.appendUnsafe('"')
          val cbArr = elemBuilder.arr
          val pos = elemBuilder.getLength
          str.getChars(0, len, cbArr, pos)
          elemBuilder.length = pos + len
          elemBuilder.appendUnsafe('"')
        } else {
          appendEscapedString(s, escapeUnicode)
        }
      case _ =>
        appendEscapedString(s, escapeUnicode)
    }
    flushCharBuilder()
    out
  }

  private def appendEscapedString(s: CharSequence, escapeUnicode: Boolean): Unit = {
    elemBuilder.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      s.charAt(i) match {
        case '"'  => appendEscapedAscii('"')
        case '\\' => appendEscapedAscii('\\')
        case '\b' => appendEscapedAscii('b')
        case '\f' => appendEscapedAscii('f')
        case '\n' => appendEscapedAscii('n')
        case '\r' => appendEscapedAscii('r')
        case '\t' => appendEscapedAscii('t')
        case c    =>
          if (c < ' ' || (c >= 0x7f && c <= 0x9f) || (escapeUnicode && c > '~')) {
            appendUnicodeEscape(c)
          } else elemBuilder.append(c)
      }
      i += 1
    }
    elemBuilder.append('"')
  }

  private def appendEscapedAscii(c: Char): Unit = {
    elemBuilder.append('\\')
    elemBuilder.append(c)
  }

  private def appendUnicodeEscape(c: Char): Unit = {
    val hex = BaseCharRenderer.HEX_CHARS
    elemBuilder.append('\\')
    elemBuilder.append('u')
    elemBuilder.append(hex((c >> 12) & 0xf))
    elemBuilder.append(hex((c >> 8) & 0xf))
    elemBuilder.append(hex((c >> 4) & 0xf))
    elemBuilder.append(hex(c & 0xf))
  }

  /**
   * Fast path for [[Val.AsciiSafeStr]]: the string is statically known to contain only chars in
   * 0x20-0x7E, excluding `"` and `\`. That means no JSON escaping is ever required — not even under
   * `escapeUnicode`, since every char is <= 0x7E. Emit `"` + raw chars + `"` with a single bulk
   * `getChars`, skipping the per-call `CharSWAR.hasEscapeChar` scan that [[visitNonNullString]]
   * would otherwise perform. Mirrors the no-escape ASCII fast path, minus the scan.
   */
  def visitAsciiSafeString(s: String, index: Int): T = {
    flushBuffer()
    val len = s.length
    elemBuilder.ensureLength(len + 2)
    elemBuilder.appendUnsafe('"')
    val cbArr = elemBuilder.arr
    val pos = elemBuilder.getLength
    s.getChars(0, len, cbArr, pos)
    elemBuilder.length = pos + len
    elemBuilder.appendUnsafe('"')
    flushCharBuilder()
    out
  }

  final def renderIndent(): Unit = {
    if (indent == -1) ()
    else if (indentCache != null && depth < BaseCharRenderer.MaxCachedDepth) {
      val cached = indentCache(depth)
      elemBuilder.appendAll(cached, cached.length)
    } else {
      val ic = indentChars
      val icLen = ic.length
      val indentLen = icLen * depth
      elemBuilder.ensureLength(indentLen + newline.length)
      elemBuilder.appendAll(newline, newline.length)
      var d = 0
      while (d < depth) {
        elemBuilder.appendAll(ic, icLen)
        d += 1
      }
    }
  }

  protected def appendString(s: String): Unit = {
    val len = s.length
    elemBuilder.ensureLength(len)
    val cbArr = elemBuilder.arr
    val pos = elemBuilder.getLength
    s.getChars(0, len, cbArr, pos)
    elemBuilder.length = pos + len
  }
}
