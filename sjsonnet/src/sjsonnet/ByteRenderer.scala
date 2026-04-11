package sjsonnet

import java.io.OutputStream

import upickle.core.{ArrVisitor, ObjVisitor}

/**
 * Byte-oriented sjsonnet JSON renderer.
 *
 * Mirrors [[Renderer]] but extends [[BaseByteRenderer]] to write byte[] directly
 * to an OutputStream, bypassing the OutputStreamWriter UTF-8 encoding layer.
 *
 * Custom sjsonnet formatting:
 *   - Doubles via [[RenderUtils.renderDouble]] (matches google/jsonnet output)
 *   - Empty arrays render as `[ ]`, empty objects as `{ }`
 *   - Comma-space separator when indent=-1 (minified mode)
 *
 * Uses reusable visitor instances to avoid per-object/array allocation.
 */
class ByteRenderer(out: OutputStream = new java.io.ByteArrayOutputStream(), indent: Int = -1)
    extends BaseByteRenderer(out, indent) {
  var newlineBuffered = false

  // Track empty state per nesting level. Bit i = 1 means level i has seen a value.
  // Supports up to 64 levels of nesting (realistic JSON rarely exceeds ~20).
  private var emptyBits: Long = 0L

  @inline private def markNonEmpty(): Unit = emptyBits |= (1L << depth)
  @inline private def isEmpty: Boolean = (emptyBits & (1L << depth)) == 0L
  @inline private def resetEmpty(): Unit = emptyBits &= ~(1L << depth)

  override def visitFloat64(d: Double, index: Int): OutputStream = {
    flushBuffer()
    val i = d.toLong
    if (d == i) {
      writeLongDirect(i)
    } else if (d % 1 == 0) {
      appendString(
        BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
      )
    } else {
      appendString(d.toString)
    }
    flushByteBuilder()
    out
  }

  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      elemBuilder.append(',')
      if (indent == -1) elemBuilder.append(' ')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      val nSpaces = indent * depth
      elemBuilder.ensureLength(nSpaces + 1)
      elemBuilder.append('\n')
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
    newlineBuffered = false
    commaBuffered = false
  }

  // Reusable ArrVisitor — avoids per-array allocation
  private val reusableArrVisitor: ArrVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer
  } = new ArrVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer = ByteRenderer.this
    def visitValue(v: OutputStream, index: Int): Unit = {
      markNonEmpty()
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): OutputStream = {
      commaBuffered = false
      newlineBuffered = false
      val wasEmpty = isEmpty
      resetEmpty()
      depth -= 1

      if (wasEmpty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append(']')
      flushByteBuilder()
      out
    }
  }

  // Reusable ObjVisitor — avoids per-object allocation
  private val reusableObjVisitor: ObjVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer; def visitKey(index: Int): sjsonnet.ByteRenderer
  } = new ObjVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer = ByteRenderer.this
    def visitKey(index: Int): sjsonnet.ByteRenderer = ByteRenderer.this
    def visitKeyValue(v: Any): Unit = {
      markNonEmpty()
      elemBuilder.append(':')
      elemBuilder.append(' ')
    }
    def visitValue(v: OutputStream, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): OutputStream = {
      commaBuffered = false
      newlineBuffered = false
      val wasEmpty = isEmpty
      resetEmpty()
      depth -= 1

      if (wasEmpty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append('}')
      flushByteBuilder()
      out
    }
  }

  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer
  } = {
    flushBuffer()
    elemBuilder.append('[')
    newlineBuffered = true
    depth += 1
    resetEmpty()
    reusableArrVisitor
  }

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[OutputStream, OutputStream] {
    def subVisitor: sjsonnet.ByteRenderer; def visitKey(index: Int): sjsonnet.ByteRenderer
  } = {
    flushBuffer()
    elemBuilder.append('{')
    newlineBuffered = true
    depth += 1
    resetEmpty()
    reusableObjVisitor
  }
}
