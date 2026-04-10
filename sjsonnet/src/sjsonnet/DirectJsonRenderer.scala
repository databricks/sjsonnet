package sjsonnet

/**
 * Specialized JSON renderer that bypasses the upickle Visitor pattern entirely, writing JSON
 * directly to a [[java.lang.StringBuilder]]. This eliminates:
 *
 *   - Per-object/array anonymous class allocations ([[upickle.core.ArrVisitor]] /
 *     [[upickle.core.ObjVisitor]])
 *   - Virtual dispatch overhead on every visitor method call (~3.3M calls on realistic2)
 *   - CharBuilder → Writer → StringBuffer intermediate pipeline
 *   - StringBuffer synchronization overhead (POSIX mutex on Scala Native)
 *
 * Used as an optimization for the common JSON rendering path (non-YAML, non-string mode). Falls
 * back to Visitor-based rendering for [[Materializer.Materializable]] custom values.
 *
 * The output is identical to [[Renderer]] for all indent values:
 *   - indent < 0 (minified): `{"k": v, "k2": v2}`, `[v1, v2]`, `{ }`, `[ ]`
 *   - indent >= 0: newlines + indentation matching Renderer/BaseCharRenderer exactly
 *
 * @param indent
 *   indentation width per nesting level; -1 for minified output
 * @param mat
 *   the Materializer instance providing storePos callbacks (yamlDebug support)
 * @param ctx
 *   immutable snapshot of materialization settings (sort order, depth limits, etc.)
 */
private[sjsonnet] final class DirectJsonRenderer(
    indent: Int,
    mat: Materializer,
    ctx: Materializer.MaterializeContext)(implicit private val ev: EvalScope) {

  // Initial capacity tuned for realistic workloads; realistic2 produces ~28MB output
  private val sb = new java.lang.StringBuilder(65536)

  /**
   * Pre-computed indent strings: indentCache(d) = "\n" + indent*d spaces. Same optimization as
   * [[BaseCharRenderer.indentCache]] but using String for efficient StringBuilder.append(String).
   */
  private val indentCache: Array[String] =
    if (indent <= 0) null
    else {
      val maxD = BaseCharRenderer.MaxCachedDepth
      val arr = new Array[String](maxD)
      val buf = new java.lang.StringBuilder(maxD * indent + 1)
      var d = 0
      while (d < maxD) {
        buf.setLength(0)
        buf.append('\n')
        var s = indent * d
        while (s > 0) { buf.append(' '); s -= 1 }
        arr(d) = buf.toString
        d += 1
      }
      arr
    }

  def render(v: Val): String = {
    try {
      renderVal(v, 0)
      sb.toString
    } catch {
      case _: StackOverflowError =>
        Error.fail(
          "Stackoverflow while materializing, possibly due to recursive value",
          v.pos
        )
      case _: OutOfMemoryError =>
        Error.fail(
          "Out of memory while materializing, possibly due to recursive value",
          v.pos
        )
    }
  }

  private def renderVal(v: Val, depth: Int): Unit = {
    if (v == null) Error.fail("Unknown value type " + v)
    val vt: Int = v.valTag.toInt
    (vt: @scala.annotation.switch) match {
      case 0 => // TAG_STR
        val s = v.asInstanceOf[Val.Str]
        mat.storePos(s.pos)
        renderString(s.str)
      case 1 => // TAG_NUM
        mat.storePos(v.pos)
        renderNumber(v.asDouble)
      case 2 => // TAG_TRUE
        mat.storePos(v.pos)
        sb.append("true")
      case 3 => // TAG_FALSE
        mat.storePos(v.pos)
        sb.append("false")
      case 4 => // TAG_NULL
        mat.storePos(v.pos)
        sb.append("null")
      case 5 => // TAG_ARR
        if (depth >= ctx.recursiveDepthLimit)
          fallbackMaterialize(v, depth)
        else
          renderArray(v.asInstanceOf[Val.Arr], depth)
      case 6 => // TAG_OBJ
        if (depth >= ctx.recursiveDepthLimit)
          fallbackMaterialize(v, depth)
        else
          renderObject(v.asInstanceOf[Val.Obj], depth)
      case 7 => // TAG_FUNC
        val f = v.asInstanceOf[Val.Func]
        Error.fail(
          "Couldn't manifest function with params [" + f.params.names.mkString(",") + "]",
          v.pos
        )
      case _ =>
        v match {
          case m: Materializer.Materializable =>
            mat.storePos(v.pos)
            // Fall back to Visitor-based rendering for custom materializable values.
            // We set the Renderer's initial depth to match the current nesting level so
            // that indentation in the Materializable's output aligns correctly.
            val currentDepth = depth
            val sw = new java.io.StringWriter()
            val renderer = createFallbackRenderer(sw, currentDepth)
            m.materialize(renderer)
            sb.append(sw.toString)
          case tc: TailCall =>
            Error.fail(
              "Internal error: TailCall sentinel leaked into materialization. " +
              "This indicates a bug in the TCO protocol — a TailCall was not resolved before " +
              "reaching the Materializer.",
              tc.pos
            )
          case vv: Val =>
            Error.fail("Unknown value type " + vv.prettyName, vv.pos)
        }
    }
  }

  /**
   * Create a [[Renderer]] for fallback paths (Materializable values and deep nesting). Overrides
   * `flushCharBuilder()` to always flush with threshold 0, because the standard `BaseCharRenderer`
   * only flushes unconditionally at depth 0 (threshold=0) and uses threshold=1000 at depth >= 1.
   * Since this fallback Renderer starts at `initialDepth >= 1` and never returns to depth 0, small
   * outputs (≤1000 chars) would silently stay in the internal `elemBuilder` and never reach the
   * `StringWriter`.
   */
  private def createFallbackRenderer(sw: java.io.StringWriter, initialDepth: Int): Renderer = {
    new Renderer(sw, indent) {
      this.depth = initialDepth
      // Override to always flush: the standard BaseCharRenderer uses threshold 1000 at depth >= 1,
      // so small outputs (≤1000 chars) would stay in elemBuilder and never reach the StringWriter.
      // Since this fallback Renderer starts at initialDepth >= 1, we must force immediate flush.
      override def flushCharBuilder(): Unit = elemBuilder.writeOutToIfLongerThan(sw, 0)
    }
  }

  /**
   * Fall back to the Materializer's hybrid recursive/iterative path for deeply nested subtrees.
   * This ensures that nesting beyond [[Settings.materializeRecursiveDepthLimit]] (default 128) uses
   * the ArrayDeque-based iterative materializer instead of unbounded JVM stack recursion, matching
   * the behavior of the standard [[Materializer.apply0]] path.
   */
  private def fallbackMaterialize(v: Val, currentDepth: Int): Unit = {
    val sw = new java.io.StringWriter()
    val renderer = createFallbackRenderer(sw, currentDepth)
    mat.apply0(v, renderer)(ev)
    sb.append(sw.toString)
  }

  private def renderObject(obj: Val.Obj, depth: Int): Unit = {
    mat.storePos(obj.pos)
    obj.triggerAllAsserts(ctx.brokenAssertionLogic)
    val keys =
      if (ctx.sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
      else obj.visibleKeyNames

    if (keys.length == 0) {
      sb.append("{ }")
      return
    }

    checkDepth(obj.pos, depth)
    sb.append('{')
    val childDepth = depth + 1
    var prevKey: String = null
    var i = 0
    while (i < keys.length) {
      val key = keys(i)
      if (i > 0) {
        sb.append(',')
        if (indent < 0) sb.append(' ')
      }
      if (indent >= 0) renderIndent(childDepth)

      val childVal = obj.value(key, ctx.emptyPos)
      mat.storePos(childVal)

      if (ctx.sort) {
        if (prevKey != null && Util.compareStringsByCodepoint(key, prevKey) <= 0)
          Error.fail(
            s"""Internal error: Unexpected key "$key" after "$prevKey" in sorted object materialization""",
            childVal.pos
          )
        prevKey = key
      }

      renderString(key)
      sb.append(':')
      sb.append(' ')
      renderVal(childVal, childDepth)
      i += 1
    }
    if (indent >= 0) renderIndent(depth)
    sb.append('}')
  }

  private def renderArray(xs: Val.Arr, depth: Int): Unit = {
    mat.storePos(xs.pos)
    val len = xs.length
    if (len == 0) {
      sb.append("[ ]")
      return
    }

    checkDepth(xs.pos, depth)
    sb.append('[')
    val childDepth = depth + 1
    var i = 0
    while (i < len) {
      if (i > 0) {
        sb.append(',')
        if (indent < 0) sb.append(' ')
      }
      if (indent >= 0) renderIndent(childDepth)
      renderVal(xs.value(i), childDepth)
      i += 1
    }
    if (indent >= 0) renderIndent(depth)
    sb.append(']')
  }

  /**
   * Render a JSON string with escape handling. Single-pass: scan and copy simultaneously. For the
   * common case (no escape needed), this performs a single StringBuilder.append(String) after
   * scanning, which is more efficient than the two-pass approach (pre-scan then bulk copy).
   */
  private def renderString(s: String): Unit = {
    sb.append('"')
    val len = s.length
    var start = 0
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\') {
        if (i > start) sb.append(s, start, i)
        escapeChar(c)
        start = i + 1
      }
      i += 1
    }
    if (start == 0) {
      // Fast path: no escaping needed, append entire string at once
      sb.append(s)
    } else if (start < len) {
      sb.append(s, start, len)
    }
    sb.append('"')
  }

  private def escapeChar(c: Char): Unit = {
    sb.append('\\')
    (c: @scala.annotation.switch) match {
      case '"'  => sb.append('"')
      case '\\' => sb.append('\\')
      case '\b' => sb.append('b')
      case '\f' => sb.append('f')
      case '\n' => sb.append('n')
      case '\r' => sb.append('r')
      case '\t' => sb.append('t')
      case _    =>
        sb.append('u')
        sb.append(Character.forDigit((c >> 12) & 0xf, 16))
        sb.append(Character.forDigit((c >> 8) & 0xf, 16))
        sb.append(Character.forDigit((c >> 4) & 0xf, 16))
        sb.append(Character.forDigit(c & 0xf, 16))
    }
  }

  /**
   * Render a double using the same logic as [[Renderer.visitFloat64]]: integers as longs, large
   * whole doubles via BigDecimal, and fractional doubles via Double.toString.
   */
  private def renderNumber(d: Double): Unit = {
    val i = d.toLong
    if (d == i) sb.append(i)
    else if (d % 1 == 0)
      sb.append(
        BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
      )
    else sb.append(d)
  }

  /**
   * Append newline + indentation for the given depth. Mirrors [[BaseCharRenderer.renderIndent]]
   * behavior exactly: indent < 0 does nothing, indent == 0 appends only newline, indent > 0 appends
   * newline + (indent * depth) spaces, using pre-computed cache for common depths.
   */
  @inline private def renderIndent(depth: Int): Unit = {
    if (indent < 0) ()
    else if (indentCache != null && depth < BaseCharRenderer.MaxCachedDepth)
      sb.append(indentCache(depth))
    else {
      sb.append('\n')
      var i = indent * depth
      while (i > 0) { sb.append(' '); i -= 1 }
    }
  }

  @inline private def checkDepth(pos: Position, depth: Int): Unit =
    if (depth >= ctx.maxDepth)
      Error.fail(
        "Stackoverflow while materializing, possibly due to recursive value",
        pos
      )
}
