package sjsonnet

import ujson._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/**
 * Byte-oriented JSON renderer that writes directly to an OutputStream via a ByteBuilder buffer.
 *
 * This bypasses the char[] → OutputStreamWriter → UTF-8 encode → byte[] pipeline used by
 * BaseCharRenderer, eliminating:
 *   - OutputStreamWriter overhead (synchronization, per-char UTF-8 encoding)
 *   - char[] to byte[] conversion at flush time
 *   - Extra memory for char[] buffer (byte[] is 2x more cache-friendly for ASCII)
 *
 * String rendering uses a two-tier strategy:
 *   - Short strings (< 128 chars): fused encode+check loop, zero allocation
 *   - Long strings (>= 128 chars): getBytes(UTF-8) + SWAR bulk scan + arraycopy
 */
class BaseByteRenderer[T <: java.io.OutputStream](
    out: T,
    indent: Int = -1,
    escapeUnicode: Boolean = false,
    newline: Array[Byte] = Array('\n'.toByte))
    extends JsVisitor[T, T] {

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[T, T] =
    visitObject(length, index)

  protected val elemBuilder = new upickle.core.ByteBuilder
  private[this] val unicodeCharBuilder = new upickle.core.CharBuilder

  def flushByteBuilder(): Unit = {
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 8192)
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

    def subVisitor: Visitor[T, T] = BaseByteRenderer.this

    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }

    def visitEnd(index: Int): T = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushByteBuilder()
      out
    }
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, T] = new ObjVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()

    def subVisitor: Visitor[T, T] = BaseByteRenderer.this

    def visitKey(index: Int): Visitor[T, T] = BaseByteRenderer.this

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
      flushByteBuilder()
      out
    }
  }

  def visitNull(index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafeC('n')
    elemBuilder.appendUnsafeC('u')
    elemBuilder.appendUnsafeC('l')
    elemBuilder.appendUnsafeC('l')
    flushByteBuilder()
    out
  }

  def visitFalse(index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(5)
    elemBuilder.appendUnsafeC('f')
    elemBuilder.appendUnsafeC('a')
    elemBuilder.appendUnsafeC('l')
    elemBuilder.appendUnsafeC('s')
    elemBuilder.appendUnsafeC('e')
    flushByteBuilder()
    out
  }

  def visitTrue(index: Int): T = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafeC('t')
    elemBuilder.appendUnsafeC('r')
    elemBuilder.appendUnsafeC('u')
    elemBuilder.appendUnsafeC('e')
    flushByteBuilder()
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
    flushByteBuilder()
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
    flushByteBuilder()
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
      elemBuilder.appendUnsafeC('0')
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
    val buf = BaseByteRenderer.scratchBuf
    var pos = 20
    while (abs >= 100) {
      val q = abs / 100
      val r = (abs - q * 100L).toInt
      abs = q
      pos -= 2
      buf(pos + 1) = BaseByteRenderer.DIGIT_ONES(r)
      buf(pos) = BaseByteRenderer.DIGIT_TENS(r)
    }
    if (abs >= 10) {
      val r = abs.toInt
      pos -= 2
      buf(pos + 1) = BaseByteRenderer.DIGIT_ONES(r)
      buf(pos) = BaseByteRenderer.DIGIT_TENS(r)
    } else {
      pos -= 1
      buf(pos) = ('0' + abs.toInt).toByte
    }
    if (negative) { pos -= 1; buf(pos) = '-'.toByte }
    val totalLen = 20 - pos
    elemBuilder.ensureLength(totalLen)
    val bArr = elemBuilder.arr
    val startPos = elemBuilder.length
    System.arraycopy(buf, pos, bArr, startPos, totalLen)
    elemBuilder.length = startPos + totalLen
  }

  def visitString(s: CharSequence, index: Int): T = {
    if (s eq null) visitNull(index)
    else visitNonNullString(s, index)
  }

  private[sjsonnet] def visitNonNullString(s: CharSequence, index: Int): T = {
    flushBuffer()
    s match {
      case str: String if !escapeUnicode =>
        val len = str.length
        if (len < 128) visitShortString(str, len)
        else visitLongString(str)
      case _ =>
        upickle.core.RenderUtils.escapeByte(
          unicodeCharBuilder,
          elemBuilder,
          s,
          escapeUnicode = escapeUnicode,
          wrapQuotes = true
        )
    }
    flushByteBuilder()
    out
  }

  /**
   * Render a quoted string into elemBuilder without calling flushBuffer/flushByteBuilder. Used by
   * the fused materializer path in ByteRenderer where the caller manages flush state.
   */
  private[sjsonnet] def renderQuotedString(str: String): Unit = {
    val len = str.length
    if (len < 128) visitShortString(str, len)
    else visitLongString(str)
  }

  /**
   * Fast path for strings known to be ASCII-safe (no escaping needed, all chars 0x20-0x7E). Skips
   * SWAR scanning and UTF-8 encoding — writes bytes directly from chars.
   */
  private[sjsonnet] def renderAsciiSafeString(str: String): Unit = {
    val len = str.length
    elemBuilder.ensureLength(len + 2)
    val arr = elemBuilder.arr
    var pos = elemBuilder.length
    arr(pos) = '"'.toByte
    pos += 1
    var i = 0
    while (i < len) {
      arr(pos) = str.charAt(i).toByte
      pos += 1
      i += 1
    }
    arr(pos) = '"'.toByte
    elemBuilder.length = pos + 1
  }

  /**
   * Zero-allocation fast path for short ASCII strings (the vast majority of JSON keys/values). Uses
   * getChars to bulk-copy into a reusable char buffer, then scans the buffer directly (avoiding
   * per-char String.charAt virtual dispatch). If any char needs escaping or is non-ASCII, falls
   * back to escapeByte.
   */
  private def visitShortString(str: String, len: Int): Unit = {
    // Reuse unicodeCharBuilder's array as temp char buffer (no allocation after warmup)
    unicodeCharBuilder.reset()
    unicodeCharBuilder.ensureLength(len)
    val chars = unicodeCharBuilder.arr
    str.getChars(0, len, chars, 0)

    elemBuilder.ensureLength(len + 2)
    val arr = elemBuilder.arr
    val startPos = elemBuilder.length
    arr(startPos) = '"'.toByte
    var pos = startPos + 1
    var i = 0
    while (i < len) {
      val c = chars(i)
      if (c < 0x20 || c == '"' || c == '\\' || c >= 0x80) {
        // DO NOT CHANGE
        // WHY: elemBuilder.length is intentionally NOT updated before this call.
        // escapeByte writes from the current elemBuilder.length position, overwriting
        // our partial work in the array. This avoids needing a separate "rollback".
        upickle.core.RenderUtils.escapeByte(
          unicodeCharBuilder,
          elemBuilder,
          str,
          escapeUnicode = false,
          wrapQuotes = true
        )
        return
      }
      arr(pos) = c.toByte
      pos += 1
      i += 1
    }
    arr(pos) = '"'.toByte
    elemBuilder.length = pos + 1
  }

  /**
   * SWAR-accelerated path for long strings. Converts to UTF-8 bytes once, scans with SWAR, and
   * bulk-copies if clean. The getBytes allocation is amortized by avoiding per-char processing.
   */
  private def visitLongString(str: String): Unit = {
    val bytes = str.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    if (!CharSWAR.hasEscapeChar(bytes, 0, bytes.length)) {
      val bLen = bytes.length
      elemBuilder.ensureLength(bLen + 2)
      val arr = elemBuilder.arr
      val pos = elemBuilder.length
      arr(pos) = '"'.toByte
      System.arraycopy(bytes, 0, arr, pos + 1, bLen)
      arr(pos + 1 + bLen) = '"'.toByte
      elemBuilder.length = pos + bLen + 2
    } else {
      upickle.core.RenderUtils.escapeByte(
        unicodeCharBuilder,
        elemBuilder,
        str,
        escapeUnicode = false,
        wrapQuotes = true
      )
    }
  }

  final def renderIndent(): Unit = {
    if (indent == -1) ()
    else {
      val nSpaces = indent * depth
      elemBuilder.ensureLength(nSpaces + newline.length)
      elemBuilder.appendAll(newline, newline.length)
      if (nSpaces > 0) {
        val spaces = BaseByteRenderer.SPACES
        val arr = elemBuilder.arr
        var pos = elemBuilder.length
        var remaining = nSpaces
        while (remaining > 0) {
          val chunk = math.min(remaining, spaces.length)
          System.arraycopy(spaces, 0, arr, pos, chunk)
          pos += chunk
          remaining -= chunk
        }
        elemBuilder.length = pos
      }
    }
  }

  protected def appendString(s: String): Unit = {
    val len = s.length
    elemBuilder.ensureLength(len)
    val arr = elemBuilder.arr
    val pos = elemBuilder.length
    var i = 0
    while (i < len) {
      arr(pos + i) = s.charAt(i).toByte
      i += 1
    }
    elemBuilder.length = pos + len
  }
}

object BaseByteRenderer {

  /** Pre-allocated spaces buffer for bulk indentation. */
  private[sjsonnet] val SPACES: Array[Byte] = {
    val a = new Array[Byte](64)
    java.util.Arrays.fill(a, ' '.toByte)
    a
  }

  /**
   * Reusable scratch buffer for writeLongDirect (max 20 bytes for Long.MinValue). Not thread-safe,
   * but renderers are single-threaded.
   */
  private[sjsonnet] val scratchBuf: Array[Byte] = new Array[Byte](20)

  /**
   * Digit-pair lookup tables for two-digits-at-a-time integer rendering. DIGIT_TENS(i) gives the
   * tens digit byte for value i (0..99). DIGIT_ONES(i) gives the ones digit byte for value i
   * (0..99).
   */
  private[sjsonnet] val DIGIT_TENS: Array[Byte] = {
    val a = new Array[Byte](100)
    var i = 0
    while (i < 100) { a(i) = ('0' + i / 10).toByte; i += 1 }
    a
  }
  private[sjsonnet] val DIGIT_ONES: Array[Byte] = {
    val a = new Array[Byte](100)
    var i = 0
    while (i < 100) { a(i) = ('0' + i % 10).toByte; i += 1 }
    a
  }
}
