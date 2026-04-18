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
      writeLongDirect(i)
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

/**
 * Renderer used by std.manifestJson, std.manifestJsonMinified, and std.manifestJsonEx.
 *
 * Supports both the Visitor-based path (via Materializer.apply0) and a fused direct path
 * (materializeDirect) that bypasses the Visitor interface for better Scala Native performance.
 */
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

  // ── Fused materializer ──────────────────────────────────────────────────────
  // Bypasses the Visitor interface entirely: walks the Val tree and writes chars
  // directly into elemBuilder. On Scala Native (no JIT), this eliminates virtual
  // dispatch overhead on every visitString/visitObject/visitArray call.

  /**
   * Fused materialize-and-render: walks the Val tree and writes JSON chars directly, without going
   * through the upickle Visitor interface.
   */
  def materializeDirect(v: Val)(implicit evaluator: EvalScope): Unit = {
    val ctx = Materializer.MaterializeContext(evaluator)
    try {
      materializeChild(v, 0, ctx)
      // Final flush — write everything to out.
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
        elemBuilder.appendUnsafe('t')
        elemBuilder.appendUnsafe('r')
        elemBuilder.appendUnsafe('u')
        elemBuilder.appendUnsafe('e')
      case 3 => // TAG_FALSE
        elemBuilder.ensureLength(5)
        elemBuilder.appendUnsafe('f')
        elemBuilder.appendUnsafe('a')
        elemBuilder.appendUnsafe('l')
        elemBuilder.appendUnsafe('s')
        elemBuilder.appendUnsafe('e')
      case 4 => // TAG_NULL
        elemBuilder.ensureLength(4)
        elemBuilder.appendUnsafe('n')
        elemBuilder.appendUnsafe('u')
        elemBuilder.appendUnsafe('l')
        elemBuilder.appendUnsafe('l')
      case 5 => // TAG_ARR
        val xs = v.asInstanceOf[Val.Arr]
        if (matDepth < ctx.recursiveDepthLimit)
          materializeDirectArr(xs, matDepth + 1, ctx)
        else
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
            Error.fail("Internal error: TailCall sentinel leaked into materialization.", tc.pos)
          case vv: Val =>
            Error.fail("Unknown value type " + vv.prettyName, vv.pos)
        }
    }
  }

  /** Render a quoted string into elemBuilder (char-based) with chunked SWAR scanning. */
  private def renderQuotedString(str: String): Unit = renderQuotedStringSWAR(str)

  /** Render a double value directly into the char buffer. */
  @inline private def renderDouble(d: Double): Unit = {
    val i = d.toLong
    if (d == i) {
      writeLongDirect(i)
    } else if (d % 1 == 0) {
      appendString(BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString())
    } else {
      appendString(d.toString)
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
        if (ctx.sort) materializeDirectSortedInlineObj(obj, matDepth, ctx)
        else materializeDirectInlineObj(obj, matDepth, ctx)
      } else {
        materializeDirectGenericObj(obj, matDepth, ctx)
      }
    } finally {
      ctx.exitObject(obj)
    }
  }

  /** Open an object brace with indent. */
  @inline private def openObjBrace(isEmpty: Boolean): Unit = {
    elemBuilder.append('{')
    depth += 1
    if (isEmpty && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()
  }

  /** Close an object brace. */
  @inline private def closeObjBrace(wasEmpty: Boolean): Unit = {
    commaBuffered = false
    depth -= 1
    renderIndent()
    elemBuilder.append('}')
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  /** Render a single key-value pair. */
  @inline private def renderKeyValue(
      key: String,
      childVal: Val,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    flushBuffer()
    renderQuotedString(key)
    elemBuilder.appendAll(keyValueSeparatorCharArray, keyValueSeparatorCharArray.length)
    materializeChild(childVal, matDepth, ctx)
  }

  /** Fused inline object rendering — bypasses visibleKeyNames and value() lookup. */
  private def materializeDirectInlineObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers
      val rawN = rawKeys.length

      // Count visible fields for empty detection
      var visCount = 0
      var i = 0
      while (i < rawN) {
        if (rawMembers(i).visibility != Expr.Member.Visibility.Hidden) visCount += 1
        i += 1
      }

      openObjBrace(visCount == 0)

      i = 0
      while (i < rawN) {
        val m = rawMembers(i)
        if (m.visibility != Expr.Member.Visibility.Hidden) {
          val childVal = m.invoke(obj, null, fs, evaluator)
          if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(i), childVal)
          renderKeyValue(rawKeys(i), childVal, matDepth, ctx)
          commaBuffered = true
        }
        i += 1
      }

      closeObjBrace(visCount == 0)
    } else {
      // Single-field object
      val sfm = obj.singleMem
      if (sfm.visibility != Expr.Member.Visibility.Hidden) {
        openObjBrace(false)
        val childVal = sfm.invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(obj.singleKey, childVal)
        renderKeyValue(obj.singleKey, childVal, matDepth, ctx)
        closeObjBrace(false)
      } else {
        // Empty object (single hidden field)
        openObjBrace(true)
        closeObjBrace(true)
      }
    }
  }

  /** Fused sorted inline object rendering — uses cached sorted field order. */
  private def materializeDirectSortedInlineObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers
      val order = {
        val cached = obj._sortedInlineOrder
        if (cached != null) cached
        else Materializer.computeSortedInlineOrder(rawKeys, rawMembers)
      }
      val visCount = order.length

      openObjBrace(visCount == 0)

      var i = 0
      while (i < visCount) {
        val idx = order(i)
        val childVal = rawMembers(idx).invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(idx), childVal)
        renderKeyValue(rawKeys(idx), childVal, matDepth, ctx)
        commaBuffered = true
        i += 1
      }

      closeObjBrace(visCount == 0)
    } else {
      // Single-field: sorted = unsorted
      materializeDirectInlineObj(obj, matDepth, ctx)
    }
  }

  /** Generic object rendering — uses visibleKeyNames + value() lookup. */
  private def materializeDirectGenericObj(
      obj: Val.Obj,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val keys =
      if (ctx.sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
      else obj.visibleKeyNames

    openObjBrace(keys.isEmpty)

    var i = 0
    while (i < keys.length) {
      val key = keys(i)
      val childVal = obj.value(key, ctx.emptyPos)
      renderKeyValue(key, childVal, matDepth, ctx)
      commaBuffered = true
      i += 1
    }

    closeObjBrace(keys.isEmpty)
  }

  private def materializeDirectArr(
      xs: Val.Arr,
      matDepth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    val len = xs.length

    elemBuilder.append('[')
    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (len == 0 && indent != -1)
      elemBuilder.appendAll(newLineCharArray, newLineCharArray.length)
    else renderIndent()

    // Fast path for byte-backed arrays: emit numbers directly
    xs match {
      case ba: Val.ByteArr =>
        val bytes = ba.rawBytes
        var i = 0
        while (i < len) {
          flushBuffer()
          renderDouble((bytes(i) & 0xff).toDouble)
          commaBuffered = true
          i += 1
        }
      case _ =>
        var i = 0
        while (i < len) {
          val childVal = xs.value(i)
          flushBuffer()
          materializeChild(childVal, matDepth, ctx)
          commaBuffered = true
          i += 1
        }
    }

    // Close bracket
    commaBuffered = false
    depth -= 1
    renderIndent()
    elemBuilder.append(']')
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }
}

object RenderUtils {

  /**
   * Custom rendering of Doubles used in rendering
   */
  def renderDouble(d: Double): String = {
    if (d.toLong == d) d.toLong.toString
    else if (d % 1 == 0) {
      BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
    } else d.toString
  }
}
