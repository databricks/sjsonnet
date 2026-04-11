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
}

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

  /**
   * Pre-computed indent arrays: indentCache(d) = newline + indent*d spaces. Used by
   * [[renderIndent]] to replace the per-character space loop with a single bulk `System.arraycopy`,
   * which is a significant win on Scala Native (no JIT to unroll the loop) and measurable even on
   * JVM for materialization-heavy workloads.
   */
  protected val indentCache: Array[Array[Char]] =
    if (indent <= 0) null
    else {
      val maxDepth = BaseCharRenderer.MaxCachedDepth
      val arr = new Array[Array[Char]](maxDepth)
      var d = 0
      while (d < maxDepth) {
        val spaces = indent * d
        val totalLen = newline.length + spaces
        val buf = new Array[Char](totalLen)
        System.arraycopy(newline, 0, buf, 0, newline.length)
        java.util.Arrays.fill(buf, newline.length, totalLen, ' ')
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
          upickle.core.RenderUtils
            .escapeChar(null, elemBuilder, s, escapeUnicode = escapeUnicode, wrapQuotes = true)
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
    else if (indentCache != null && depth < BaseCharRenderer.MaxCachedDepth) {
      val cached = indentCache(depth)
      elemBuilder.appendAll(cached, cached.length)
    } else {
      var i = indent * depth
      elemBuilder.ensureLength(i + newline.length)
      elemBuilder.appendAll(newline, newline.length)
      while (i > 0) {
        elemBuilder.appendUnsafe(' ')
        i -= 1
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
