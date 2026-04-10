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
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat64(d, index)
        flushBuffer()
    }
    flushCharBuilder()
    out
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

  /**
   * Bulk-copy String chars into CharBuilder's backing array. Safe because ensureLength(len)
   * guarantees capacity for currentLength + len.
   */
  protected def appendString(s: String): Unit = {
    val len = s.length
    elemBuilder.ensureLength(len)
    val pos = elemBuilder.getLength
    s.getChars(0, len, elemBuilder.arr, pos)
    elemBuilder.length = pos + len
  }
}
