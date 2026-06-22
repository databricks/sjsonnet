package sjsonnet

import java.io.{StringWriter, Writer}

import upickle.core.{ArrVisitor, ObjVisitor}

final class StringBuilderWriter(initialCapacity: Int = 16) extends Writer {
  private val builder = new java.lang.StringBuilder(initialCapacity)

  /**
   * Exposes the underlying [[java.lang.StringBuilder]] for callers that need direct
   * length/charAt/setLength operations (e.g. [[YamlRenderer]] trims trailing spaces).
   * Single-threaded use only; the writer is not thread-safe.
   */
  def getBuilder: java.lang.StringBuilder = builder

  override def write(c: Int): Unit =
    builder.append(c.toChar)

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit =
    builder.append(cbuf, off, len)

  override def write(str: String): Unit =
    builder.append(str)

  override def write(str: String, off: Int, len: Int): Unit =
    builder.append(str, off, off + len)

  override def append(c: Char): Writer = {
    builder.append(c)
    this
  }

  override def append(csq: CharSequence): Writer = {
    builder.append(if (csq == null) "null" else csq)
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): Writer = {
    builder.append(if (csq == null) "null" else csq, start, end)
    this
  }

  override def flush(): Unit = ()
  override def close(): Unit = ()
  override def toString: String = builder.toString
}

/**
 * Custom JSON renderer to try and match the behavior of google/jsonnet's render:
 *
 *   - Custom printing of Doubles
 *   - Custom printing of empty dictionaries and arrays
 */
class Renderer(out: Writer = new StringBuilderWriter(), indent: Int = -1)
    extends BaseCharRenderer(out, indent) {
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int): Writer = {
    val l = d.toLong
    if (RenderUtils.isExactLongDouble(d, l)) writeLongDirect(l)
    else {
      flushBuffer()
      appendString(RenderUtils.renderDouble(d))
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

class PythonRenderer(out: Writer = new StringBuilderWriter(), indent: Int = -1)
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

  private val reusableArrVisitor: ArrVisitor[StringWriter, StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
  } = new ArrVisitor[StringWriter, StringWriter] {
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

  private val reusableObjVisitor: ObjVisitor[StringWriter, StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
    def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer
  } = new ObjVisitor[StringWriter, StringWriter] {
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

  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[java.io.StringWriter, java.io.StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
  } = {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    reusableArrVisitor
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.StringWriter, java.io.StringWriter] {
    def subVisitor: sjsonnet.MaterializeJsonRenderer
    def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer
  } = {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    reusableObjVisitor
  }
}

private[sjsonnet] final class FastMaterializeJsonRenderer(
    indent: Int = 4,
    escapeUnicode: Boolean = false,
    newline: String = "\n",
    keyValueSeparator: String = ": ",
    indentStr: Array[Char] = null,
    private val outWriter: StringBuilderWriter = new StringBuilderWriter())
    extends BaseCharRenderer(
      outWriter,
      indent,
      escapeUnicode,
      newline.toCharArray,
      indentStr
    ) {
  private val newLineCharArray = newline.toCharArray
  private val keyValueSeparatorCharArray = keyValueSeparator.toCharArray

  // Hot-path overrides with direct implementation (no super delegation).
  // @inline is safe here because this class is final and uses `outWriter` directly.
  @inline override def flushCharBuilder(): Unit =
    elemBuilder.writeOutToIfLongerThan(outWriter, if (depth == 0) 0 else 1000)

  @inline override def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.append(',')
      renderIndent()
    }
  }

  private val reusableArrVisitor: ArrVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer
  } = new ArrVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer = FastMaterializeJsonRenderer.this
    def visitValue(v: StringBuilderWriter, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): StringBuilderWriter = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      outWriter
    }
  }

  private val reusableObjVisitor: ObjVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer
    def visitKey(index: Int): sjsonnet.FastMaterializeJsonRenderer
  } = new ObjVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer = FastMaterializeJsonRenderer.this
    def visitKey(index: Int): sjsonnet.FastMaterializeJsonRenderer =
      FastMaterializeJsonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.appendAll(keyValueSeparatorCharArray, keyValueSeparatorCharArray.length)
    }
    def visitValue(v: StringBuilderWriter, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): StringBuilderWriter = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      outWriter
    }
  }

  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer
  } = {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    reusableArrVisitor
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[StringBuilderWriter, StringBuilderWriter] {
    def subVisitor: sjsonnet.FastMaterializeJsonRenderer
    def visitKey(index: Int): sjsonnet.FastMaterializeJsonRenderer
  } = {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    if (length == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
    reusableObjVisitor
  }
}

object RenderUtils {

  // Pre-cached string representations of small integers (0-255)
  private val intStrCache: Array[String] = Array.tabulate(256)(_.toString)

  /** Normalize platform double strings to a stable shortest-round-trip decimal spelling. */
  private[sjsonnet] def formatDoubleString(s: String): String = {
    val eIdx = s.indexOf('E')
    if (eIdx >= 0) {
      normalizeScientificString(s, eIdx)
    } else {
      val eIdxLower = s.indexOf('e')
      if (eIdxLower >= 0) {
        normalizeScientificString(s, eIdxLower)
      } else s
    }
  }

  private def normalizeScientificString(s: String, eIdx: Int): String = {
    val rawMantissa = s.substring(0, eIdx)
    val exp = s.substring(eIdx + 1)
    val expValue = Integer.parseInt(exp)
    val fixed = formatFixedDecimal(rawMantissa, expValue)
    if (fixed != null) return fixed

    val mantissa = trimTrailingFractionZeros(rawMantissa)
    val expHasSign = exp.nonEmpty && (exp.charAt(0) == '-' || exp.charAt(0) == '+')
    val expSign = if (exp.nonEmpty && exp.charAt(0) == '-') "-" else "+"
    val expDigits = if (expHasSign) exp.substring(1) else exp
    mantissa + "e" + expSign + padExponent(expDigits)
  }

  private def formatFixedDecimal(mantissa: String, exp: Int): String = {
    val negative = mantissa.startsWith("-")
    val unsigned = if (negative) mantissa.substring(1) else mantissa
    val dotIdx = unsigned.indexOf('.')
    val digitsBeforeDecimal = if (dotIdx >= 0) dotIdx else unsigned.length
    val decimalPoint = digitsBeforeDecimal + exp

    // ECMAScript Number.prototype.toString fixed-notation window: values in
    // [1e-6, 1e21) are spelled in fixed decimal form, and values outside that
    // window use scientific notation. JSON/Java/Scala.js follow the same
    // convention, so this keeps sjsonnet's canonical spelling aligned with
    // the platforms users compare against.
    if (decimalPoint <= -6 || decimalPoint > 21) return null

    val digits =
      if (dotIdx >= 0) unsigned.substring(0, dotIdx) + unsigned.substring(dotIdx + 1)
      else unsigned

    val fixed =
      if (decimalPoint <= 0) "0." + ("0" * -decimalPoint) + digits
      else if (decimalPoint >= digits.length) digits + ("0" * (decimalPoint - digits.length))
      else digits.substring(0, decimalPoint) + "." + digits.substring(decimalPoint)

    val trimmed = trimTrailingFractionZeros(fixed)
    if (negative) "-" + trimmed else trimmed
  }

  private def trimTrailingFractionZeros(s: String): String = {
    val dotIdx = s.indexOf('.')
    if (dotIdx < 0) return s

    var end = s.length
    while (end > dotIdx + 1 && s.charAt(end - 1) == '0') end -= 1
    if (end == dotIdx + 1) s.substring(0, dotIdx)
    else s.substring(0, end)
  }

  private def padExponent(expDigits: String): String =
    if (expDigits.length < 2) "0" + expDigits else expDigits

  /**
   * Custom rendering of Doubles used in rendering.
   *
   * Keeps non-integer doubles in fixed decimal form for the common JSON range, and otherwise
   * normalizes scientific notation to lowercase 'e', explicit sign, and minimum 2 exponent digits.
   */
  def renderDouble(d: Double): String = {
    if (java.lang.Double.compare(d, -0.0) == 0) return "-0"
    val l = d.toLong
    if (isExactLongDouble(d, l)) {
      if (l >= 0 && l < 256) intStrCache(l.toInt)
      else l.toString
    } else if (d % 1 == 0) {
      // valueOf uses the canonical decimal spelling of the double. The BigDecimal(double)
      // constructor exposes the exact binary64 payload, which is surprising for Jsonnet output
      // and regresses values such as 1e100 into long implementation-specific decimals.
      // HALF_EVEN is fine here: doubles that reach this branch have magnitude >= 2^53, where
      // the ULP is >= 1, so no representable value is an exact .5 tie — any rounding mode
      // yields the same integer.
      java.math.BigDecimal
        .valueOf(d)
        .setScale(0, java.math.RoundingMode.HALF_EVEN)
        .toBigInteger
        .toString
    } else formatDoubleString(d.toString)
  }

  private final val LongUpperExclusive = 9223372036854775808.0

  private[sjsonnet] def isExactLongDouble(d: Double, l: Long): Boolean =
    java.lang.Double.compare(d, -0.0) != 0 &&
    l.toDouble == d &&
    d >= Long.MinValue.toDouble &&
    d < LongUpperExclusive

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
