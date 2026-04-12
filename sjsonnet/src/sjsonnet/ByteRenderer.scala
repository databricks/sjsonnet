package sjsonnet

import java.io.OutputStream

import sjsonnet.Expr.Member.Visibility
import upickle.core.{ArrVisitor, ObjVisitor}

/**
 * Byte-oriented sjsonnet JSON renderer.
 *
 * Mirrors [[Renderer]] but extends [[BaseByteRenderer]] to write byte[] directly to an
 * OutputStream, bypassing the OutputStreamWriter UTF-8 encoding layer.
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

  /** Public accessor for the output stream, used by the fused materializer path. */
  def outputStream: OutputStream = out
  var newlineBuffered = false

  // Track empty state per nesting level. Bit i = 1 means level i has seen a value.
  // Supports up to 64 levels of nesting (realistic JSON rarely exceeds ~20).
  private var emptyBits: Long = 0L

  @inline private def markNonEmpty(): Unit = emptyBits |= (1L << depth)
  @inline private def isEmpty: Boolean = (emptyBits & (1L << depth)) == 0L
  @inline private def resetEmpty(): Unit = emptyBits &= ~(1L << depth)

  override def visitFloat64(d: Double, index: Int): OutputStream = {
    flushBuffer()
    renderDouble(d)
    flushByteBuilder()
    out
  }

  /** Render a double value directly into the byte buffer (no OutputStream return). */
  @inline private def renderDouble(d: Double): Unit = {
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

  // ── Fused materializer ──────────────────────────────────────────────────────
  // Bypasses the Visitor interface entirely: the materializer loop calls
  // ByteRenderer methods directly (no virtual dispatch, no ObjVisitor/ArrVisitor
  // wrapper calls). On Scala Native (no JIT), this eliminates ~5M virtual calls
  // for realistic2, shaving off the vtable-lookup + indirect-branch overhead.

  /**
   * Fused materialize-and-render: walks the Val tree and writes JSON bytes directly, without going
   * through the upickle Visitor interface.
   */
  def materializeDirect(v: Val)(implicit evaluator: EvalScope): Unit = {
    val ctx = Materializer.MaterializeContext(evaluator)
    try {
      materializeChild(v, 0, ctx)
      // Final flush — depth is 0, so this writes everything to out.
      elemBuilder.writeOutToIfLongerThan(out, 0)
    } catch {
      case _: StackOverflowError =>
        Error.fail("Stackoverflow while materializing, possibly due to recursive value", v.pos)
      case _: OutOfMemoryError =>
        Error.fail("Out of memory while materializing, possibly due to recursive value", v.pos)
    }
  }

  private def materializeChild(v: Val, matDepth: Int, ctx: Materializer.MaterializeContext)(implicit
      evaluator: EvalScope): Unit = {
    if (v == null) Error.fail("Unknown value type " + v)
    val vt: Int = v.valTag.toInt
    (vt: @scala.annotation.switch) match {
      case 0 => // TAG_STR
        renderQuotedString(v.asInstanceOf[Val.Str].str)
      case 1 => // TAG_NUM
        renderDouble(v.asDouble)
      case 2 => // TAG_TRUE
        elemBuilder.ensureLength(4)
        elemBuilder.appendUnsafeC('t')
        elemBuilder.appendUnsafeC('r')
        elemBuilder.appendUnsafeC('u')
        elemBuilder.appendUnsafeC('e')
      case 3 => // TAG_FALSE
        elemBuilder.ensureLength(5)
        elemBuilder.appendUnsafeC('f')
        elemBuilder.appendUnsafeC('a')
        elemBuilder.appendUnsafeC('l')
        elemBuilder.appendUnsafeC('s')
        elemBuilder.appendUnsafeC('e')
      case 4 => // TAG_NULL
        elemBuilder.ensureLength(4)
        elemBuilder.appendUnsafeC('n')
        elemBuilder.appendUnsafeC('u')
        elemBuilder.appendUnsafeC('l')
        elemBuilder.appendUnsafeC('l')
      case 5 => // TAG_ARR
        val xs = v.asInstanceOf[Val.Arr]
        if (matDepth < ctx.recursiveDepthLimit)
          materializeDirectArr(xs, matDepth + 1, ctx)
        else
          // Fall back to generic visitor path for extremely deep nesting
          Materializer.apply0(v, this)(evaluator)
      case 6 => // TAG_OBJ
        val obj = v.asInstanceOf[Val.Obj]
        if (matDepth < ctx.recursiveDepthLimit)
          materializeDirectObj(obj, matDepth + 1, ctx)
        else
          Materializer.apply0(v, this)(evaluator)
      case 7 => // TAG_FUNC
        val s = v.asInstanceOf[Val.Func]
        Error.fail(
          "Couldn't manifest function with params [" + s.params.names.mkString(",") + "]",
          v.pos
        )
      case _ =>
        v match {
          case mat: Materializer.Materializable =>
            mat.materialize(this)
          case tc: TailCall =>
            Error.fail(
              "Internal error: TailCall sentinel leaked into materialization.",
              tc.pos
            )
          case vv: Val =>
            Error.fail("Unknown value type " + vv.prettyName, vv.pos)
        }
    }
  }

  private def materializeDirectObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    if (!ctx.enterObject(obj))
      Error.fail("Stackoverflow while materializing, possibly due to recursive value", obj.pos)
    try {
      obj.triggerAllAsserts(ctx.brokenAssertionLogic)

      if (obj.canDirectIterate) {
        // Fast path: iterate inline field arrays directly, bypassing visibleKeyNames allocation,
        // value() HashMap lookup, and per-object key sorting. Mirrors Materializer's
        // materializeInlineObj / materializeSortedInlineObj but writes bytes directly.
        if (ctx.sort) materializeDirectInlineSortedObj(obj, matDepth, ctx)
        else materializeDirectInlineObj(obj, matDepth, ctx)
      } else {
        // Slow path: general objects with super chain or excluded keys
        materializeDirectGenericObj(obj, matDepth, ctx)
      }
    } finally {
      ctx.exitObject(obj)
    }
  }

  /**
   * Direct inline iteration for objects without super chain. Invokes members directly by array
   * index, avoiding visibleKeyNames allocation and value() HashMap lookup per key.
   */
  private def materializeDirectInlineObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers

      elemBuilder.append('{')
      newlineBuffered = true
      depth += 1
      resetEmpty()

      val rawN = rawKeys.length
      var i = 0
      while (i < rawN) {
        val m = rawMembers(i)
        if (m.visibility != Visibility.Hidden) {
          val childVal = m.invoke(obj, null, fs, evaluator)
          if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(i), childVal)

          markNonEmpty()
          flushBuffer()
          renderQuotedString(rawKeys(i))
          elemBuilder.append(':')
          elemBuilder.append(' ')
          materializeChild(childVal, matDepth, ctx)
          commaBuffered = true
        }
        i += 1
      }

      commaBuffered = false
      newlineBuffered = false
      val wasEmpty = isEmpty
      resetEmpty()
      depth -= 1
      if (wasEmpty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append('}')
      flushByteBuilder()
    } else {
      // Single-field object
      val sfm = obj.singleMem
      if (sfm.visibility != Visibility.Hidden) {
        val childVal = sfm.invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(obj.singleKey, childVal)

        elemBuilder.append('{')
        newlineBuffered = true
        depth += 1
        resetEmpty()
        markNonEmpty()
        flushBuffer()
        renderQuotedString(obj.singleKey)
        elemBuilder.append(':')
        elemBuilder.append(' ')
        materializeChild(childVal, matDepth, ctx)

        commaBuffered = false
        newlineBuffered = false
        resetEmpty()
        depth -= 1
        renderIndent()
        elemBuilder.append('}')
        flushByteBuilder()
      } else {
        elemBuilder.append('{')
        elemBuilder.append(' ')
        elemBuilder.append('}')
        flushByteBuilder()
      }
    }
  }

  /**
   * Sorted direct inline iteration using cached sort order from the MemberList expression. Avoids
   * per-object key sorting (125K sorts in realistic2 → 0 sorts with this optimization).
   */
  private def materializeDirectInlineSortedObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers
      // Use cached sorted order if available, otherwise compute once
      val order = {
        val cached = obj._sortedInlineOrder
        if (cached != null) cached
        else Materializer.computeSortedInlineOrder(rawKeys, rawMembers)
      }
      val visCount = order.length

      elemBuilder.append('{')
      newlineBuffered = true
      depth += 1
      resetEmpty()

      var i = 0
      while (i < visCount) {
        val idx = order(i)
        val childVal = rawMembers(idx).invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(idx), childVal)

        markNonEmpty()
        flushBuffer()
        renderQuotedString(rawKeys(idx))
        elemBuilder.append(':')
        elemBuilder.append(' ')
        materializeChild(childVal, matDepth, ctx)
        commaBuffered = true
        i += 1
      }

      commaBuffered = false
      newlineBuffered = false
      val wasEmpty = isEmpty
      resetEmpty()
      depth -= 1
      if (wasEmpty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append('}')
      flushByteBuilder()
    } else {
      // Single-field: sorting is trivial (same as unsorted)
      materializeDirectInlineObj(obj, matDepth, ctx)
    }
  }

  /** General object materialization for objects with super chain or excluded keys. */
  private def materializeDirectGenericObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val keys =
      if (ctx.sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
      else obj.visibleKeyNames

    elemBuilder.append('{')
    newlineBuffered = true
    depth += 1
    resetEmpty()

    var i = 0
    while (i < keys.length) {
      val key = keys(i)
      val childVal = obj.value(key, ctx.emptyPos)

      markNonEmpty()
      flushBuffer()
      renderQuotedString(key)
      elemBuilder.append(':')
      elemBuilder.append(' ')
      materializeChild(childVal, matDepth, ctx)
      commaBuffered = true
      i += 1
    }

    commaBuffered = false
    newlineBuffered = false
    val wasEmpty = isEmpty
    resetEmpty()
    depth -= 1
    if (wasEmpty) elemBuilder.append(' ')
    else renderIndent()
    elemBuilder.append('}')
    flushByteBuilder()
  }

  private def materializeDirectArr(
      xs: Val.Arr,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val len = xs.length

    // Inline of visitArray — open bracket
    elemBuilder.append('[')
    newlineBuffered = true
    depth += 1
    resetEmpty()

    var i = 0
    while (i < len) {
      val childVal = xs.value(i)

      markNonEmpty()
      flushBuffer()

      // Render element directly — no flush overhead
      materializeChild(childVal, matDepth, ctx)

      commaBuffered = true
      i += 1
    }

    // Inline of visitEnd — close bracket
    commaBuffered = false
    newlineBuffered = false
    val wasEmpty = isEmpty
    resetEmpty()
    depth -= 1
    if (wasEmpty) elemBuilder.append(' ')
    else renderIndent()
    elemBuilder.append(']')
    flushByteBuilder()
  }
}
