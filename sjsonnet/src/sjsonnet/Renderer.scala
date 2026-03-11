package sjsonnet

import java.io.{StringWriter, Writer}

import upickle.core.{ArrVisitor, ObjVisitor}

/**
 * Custom JSON renderer to try and match the behavior of google/jsonnet's render:
 *
 *   - Custom printing of Doubles
 *   - Custom printing of empty dictionaries and arrays
 */
class Renderer(out: Writer = new java.io.StringWriter(), indent: Int = -1)
    extends BaseCharRenderer(out, indent) {
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int): Writer = {
    flushBuffer()
    val i = d.toLong
    if (d == i) {
      // Fast path: render integers directly to char buffer, avoiding String allocation.
      // Most numbers in Jsonnet output are integers (array indices, counters, etc.).
      RenderUtils.appendLong(elemBuilder, i)
    } else if (d % 1 == 0) {
      appendString(
        BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
      )
    } else {
      appendString(d.toString)
    }
    flushCharBuilder()
    out
  }
  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      elemBuilder.append(',')
      if (indent == -1) elemBuilder.append(' ')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      if (indentCache != null && depth < BaseCharRenderer.MaxCachedDepth) {
        val cached = indentCache(depth)
        elemBuilder.appendAll(cached, cached.length)
      } else {
        var i = indent * depth
        elemBuilder.ensureLength(i + 1)
        elemBuilder.append('\n')
        while (i > 0) {
          elemBuilder.append(' ')
          i -= 1
        }
      }
    }
    newlineBuffered = false
    commaBuffered = false
  }
  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.Renderer
  } = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor: sjsonnet.Renderer = Renderer.this
    def visitValue(v: Writer, index: Int): Unit = {
      empty = false
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): Writer = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.Renderer; def visitKey(index: Int): sjsonnet.Renderer
  } = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor: sjsonnet.Renderer = Renderer.this
    def visitKey(index: Int): sjsonnet.Renderer = Renderer.this
    def visitKeyValue(v: Any): Unit = {
      empty = false
      // flushBuffer()
      elemBuilder.append(':')
      elemBuilder.append(' ')
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): Writer = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }
}

class PythonRenderer(out: Writer = new java.io.StringWriter(), indent: Int = -1)
    extends BaseCharRenderer(out, indent) {

  override def visitNull(index: Int): Writer = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('N')
    elemBuilder.appendUnsafe('o')
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitFalse(index: Int): Writer = {
    flushBuffer()
    elemBuilder.ensureLength(5)
    elemBuilder.appendUnsafe('F')
    elemBuilder.appendUnsafe('a')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('s')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitTrue(index: Int): Writer = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('T')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.PythonRenderer; def visitKey(index: Int): sjsonnet.PythonRenderer
  } = new ObjVisitor[Writer, Writer] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor: sjsonnet.PythonRenderer = PythonRenderer.this
    def visitKey(index: Int): sjsonnet.PythonRenderer = PythonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.ensureLength(2)
      elemBuilder.append(':')
      elemBuilder.append(' ')
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): Writer = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }

  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.ensureLength(2)
      elemBuilder.append(',')
      elemBuilder.append(' ')
      renderIndent()
    }
  }
}

/** Renderer used by std.manifestJson, std.manifestJsonMinified, and std.manifestJsonEx */
final case class MaterializeJsonRenderer(
    indent: Int = 4,
    escapeUnicode: Boolean = false,
    out: StringWriter = new StringWriter(),
    newline: String = "\n",
    keyValueSeparator: String = ": ")
    extends BaseCharRenderer(out, indent, escapeUnicode, newline.toCharArray) {
  private val newLineCharArray = newline.toCharArray
  private val keyValueSeparatorCharArray = keyValueSeparator.toCharArray

  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[java.io.StringWriter, java.io.StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
  } = new ArrVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    def subVisitor: sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): StringWriter = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.StringWriter, java.io.StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
    def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer
  } = new ObjVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    def subVisitor: sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
    def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.appendAll(keyValueSeparatorCharArray, keyValueSeparatorCharArray.length)
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): StringWriter = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }
}

object RenderUtils {

  // Pre-cached string representations of small integers (0-255)
  private val intStrCache: Array[String] = Array.tabulate(256)(_.toString)

  /**
   * Custom rendering of Doubles used in rendering
   */
  def renderDouble(d: Double): String = {
    val l = d.toLong
    if (l.toDouble == d) {
      if (l >= 0 && l < 256) intStrCache(l.toInt)
      else l.toString
    } else if (d % 1 == 0) {
      BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
    } else d.toString
  }

  /** Maximum number of digits in a Long value (Long.MinValue = -9223372036854775808, 20 chars). */
  private final val MaxLongChars = 20

  /**
   * Render a long value directly into a [[upickle.core.CharBuilder]], avoiding the intermediate
   * `String` allocation that `Long.toString` would create. For small absolute values (the common
   * case in Jsonnet output — array lengths, indices, counters), this saves one allocation per
   * number. The algorithm writes digits in reverse then reverses in-place.
   */
  def appendLong(cb: upickle.core.CharBuilder, value: Long): Unit = {
    if (value == 0) {
      cb.append('0')
      return
    }

    cb.ensureLength(MaxLongChars)
    val arr = cb.arr
    var pos = cb.getLength

    val negative = value < 0
    // Use negative accumulator to handle Long.MinValue correctly
    var n = if (negative) value else -value
    val startPos = pos

    while (n != 0) {
      val digit = -(n % 10).toInt
      arr(pos) = ('0' + digit).toChar
      pos += 1
      n /= 10
    }

    if (negative) {
      arr(pos) = '-'
      pos += 1
    }

    // Reverse the digits in-place
    var lo = startPos
    var hi = pos - 1
    while (lo < hi) {
      val tmp = arr(lo)
      arr(lo) = arr(hi)
      arr(hi) = tmp
      lo += 1
      hi -= 1
    }

    cb.length = pos
  }
}
