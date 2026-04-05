package sjsonnet

// Vendored version of `ujson.BaseCharRenderer` from ujson 1.3.7
// with some private definitions made accessible to subclasses

import ujson._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}
class BaseCharRenderer[T <: upickle.core.CharOps.Output](
    out: T,
    indent: Int = -1,
    escapeUnicode: Boolean = false,
    newline: Array[Char] = Array('\n'))
    extends JsVisitor[T, T] {

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[T, T] =
    visitObject(length, index)

  protected val elemBuilder = new upickle.core.CharBuilder
  def flushCharBuilder(): Unit = {
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  protected var depth: Int = 0

  protected var commaBuffered = false

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
        val i = d.toLong
        if (d == i) writeLongDirect(i)
        else super.visitFloat64(d, index)
        flushBuffer()
    }
    flushCharBuilder()
    out
  }

  /**
   * Write a long integer directly into elemBuilder without intermediate String allocation. Uses
   * digit-pair table for fast two-digits-at-a-time conversion.
   */
  private def writeLongDirect(v: Long): Unit = {
    flushBuffer()
    if (v == 0L) {
      elemBuilder.ensureLength(1)
      elemBuilder.appendUnsafe('0')
      return
    }
    if (v == Long.MinValue) {
      // -Long.MinValue overflows; handle specially
      visitFloat64StringParts("-9223372036854775808", -1, -1, -1)
      return
    }
    val negative = v < 0
    var abs = if (negative) -v else v
    // Count digits
    var numDigits = 0
    var tmp = abs
    while (tmp > 0) { numDigits += 1; tmp /= 10 }
    val totalLen = numDigits + (if (negative) 1 else 0)
    elemBuilder.ensureLength(totalLen)
    val cbArr = elemBuilder.arr
    val startPos = elemBuilder.getLength
    var writePos = startPos + totalLen - 1
    // Write digits from right to left, two at a time
    while (abs >= 100) {
      val q = abs / 100
      val r = (abs - q * 100L).toInt
      abs = q
      cbArr(writePos) = BaseCharRenderer.DIGIT_ONES(r)
      cbArr(writePos - 1) = BaseCharRenderer.DIGIT_TENS(r)
      writePos -= 2
    }
    if (abs >= 10) {
      val r = abs.toInt
      cbArr(writePos) = BaseCharRenderer.DIGIT_ONES(r)
      cbArr(writePos - 1) = BaseCharRenderer.DIGIT_TENS(r)
    } else {
      cbArr(writePos) = ('0' + abs.toInt).toChar
    }
    if (negative) cbArr(startPos) = '-'
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
        // Quick pre-scan: determine if any character needs escaping (control chars,
        // double-quote, backslash). This is NOT a replacement of escapeChar — it's a
        // gate that lets us SKIP escapeChar's per-character processing entirely when
        // the string is clean (the common case in Jsonnet output). When no escaping is
        // needed, String.getChars bulk-copies the entire string in one JVM intrinsic
        // call, which is significantly faster than character-by-character append.
        var needsEscape = false
        var i = 0
        while (i < len && !needsEscape) {
          val c = str.charAt(i)
          if (c < 32 || c == '"' || c == '\\') needsEscape = true
          i += 1
        }
        if (!needsEscape) {
          // Fast path: bulk copy entire string with surrounding quotes
          elemBuilder.ensureLength(len + 2)
          elemBuilder.appendUnsafe('"')
          val cbArr = elemBuilder.arr
          val pos = elemBuilder.getLength
          str.getChars(0, len, cbArr, pos)
          elemBuilder.length = pos + len
          elemBuilder.appendUnsafe('"')
        } else {
          // Slow path: delegate to upickle's per-character escapeChar
          upickle.core.RenderUtils.escapeChar(
            null,
            elemBuilder,
            s,
            escapeUnicode = escapeUnicode,
            wrapQuotes = true
          )
        }
      case _ =>
        upickle.core.RenderUtils.escapeChar(
          null,
          elemBuilder,
          s,
          escapeUnicode = escapeUnicode,
          wrapQuotes = true
        )
    }
    flushCharBuilder()
    out
  }

  final def renderIndent(): Unit = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      elemBuilder.ensureLength(i + 1)
      elemBuilder.appendAll(newline, newline.length)
      while (i > 0) {
        elemBuilder.appendUnsafe(' ')
        i -= 1
      }
    }
  }

  protected def appendString(s: String): Unit = {
    val len = s.length
    var i = 0
    elemBuilder.ensureLength(len)
    while (i < len) {
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
  }
}

object BaseCharRenderer {

  /**
   * Digit-pair lookup tables for fast two-digit-at-a-time integer rendering. DIGIT_TENS(i) gives
   * the tens digit for value i (0..99). DIGIT_ONES(i) gives the ones digit for value i (0..99).
   */
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
}
