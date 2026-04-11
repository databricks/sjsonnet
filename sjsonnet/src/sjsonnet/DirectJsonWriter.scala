package sjsonnet

import java.io.Writer
import scala.annotation.switch

/**
 * High-performance JSON writer that fuses materialization and rendering into a single pass,
 * bypassing the Visitor pattern entirely. This eliminates per-value overhead from:
 *
 *   - Anonymous ArrVisitor/ObjVisitor allocation (one per array/object)
 *   - Virtual method dispatch through the Visitor interface (5-10 calls per JSON value)
 *   - SubVisitor lookups and type casts
 *   - Buffered comma/newline state machine in BaseCharRenderer
 *
 * On Scala Native (no JIT inlining/devirtualization), this overhead is substantial for
 * materialization-heavy workloads like realistic_2 (28.5MB output, ~200K JSON values).
 *
 * Output is byte-identical to the standard Renderer path for all JSON output modes. YAML output and
 * string-expect mode still use the Visitor-based path.
 */
final class DirectJsonWriter(
    out: Writer,
    indent: Int = 3,
    sort: Boolean = true,
    brokenAssertionLogic: Boolean = false)(implicit ev: EvalScope) {

  private val buf = new upickle.core.CharBuilder
  private var depth: Int = 0
  private val emptyPos: Position = ev.emptyMaterializeFileScopePos

  // Whether we're in compact mode (indent == -1): uses ", " between elements, no newlines
  private val compact: Boolean = indent == -1

  /**
   * Pre-computed indent arrays: indentCache(d) = '\n' + indent*d spaces. Matches BaseCharRenderer's
   * indentCache for identical output. Only allocated when indent > 0 (pretty-printing mode).
   */
  private val indentCache: Array[Array[Char]] =
    if (indent <= 0) null
    else {
      val maxDepth = BaseCharRenderer.MaxCachedDepth
      val arr = new Array[Array[Char]](maxDepth)
      var d = 0
      while (d < maxDepth) {
        val spaces = indent * d
        val totalLen = 1 + spaces // '\n' + spaces
        val b = new Array[Char](totalLen)
        b(0) = '\n'
        java.util.Arrays.fill(b, 1, totalLen, ' ')
        arr(d) = b
        d += 1
      }
      arr
    }

  /** Write the top-level Val and flush all buffered output. */
  def write(v: Val): Unit = {
    writeVal(v)
    buf.writeOutToIfLongerThan(out, 0)
  }

  private def writeVal(v: Val): Unit = {
    val vt: Int = v.valTag.toInt
    (vt: @switch) match {
      case 0 => // TAG_STR
        writeString(v.asInstanceOf[Val.Str].str)
      case 1 => // TAG_NUM
        writeNum(v.asDouble)
      case 2 => // TAG_TRUE
        buf.ensureLength(4)
        buf.appendUnsafe('t')
        buf.appendUnsafe('r')
        buf.appendUnsafe('u')
        buf.appendUnsafe('e')
      case 3 => // TAG_FALSE
        buf.ensureLength(5)
        buf.appendUnsafe('f')
        buf.appendUnsafe('a')
        buf.appendUnsafe('l')
        buf.appendUnsafe('s')
        buf.appendUnsafe('e')
      case 4 => // TAG_NULL
        buf.ensureLength(4)
        buf.appendUnsafe('n')
        buf.appendUnsafe('u')
        buf.appendUnsafe('l')
        buf.appendUnsafe('l')
      case 5 => // TAG_ARR
        writeArr(v.asInstanceOf[Val.Arr])
      case 6 => // TAG_OBJ
        writeObj(v.asInstanceOf[Val.Obj])
      case 7 => // TAG_FUNC
        val s = v.asInstanceOf[Val.Func]
        Error.fail(
          "Couldn't manifest function with params [" + s.params.names.mkString(",") + "]",
          v.pos
        )
      case _ =>
        v match {
          case mat: Materializer.Materializable =>
            // Fall back to Visitor-based materialization for custom types
            mat.materialize(new Renderer(out, indent))
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

  private def writeArr(arr: Val.Arr): Unit = {
    val len = arr.length
    buf.append('[')
    if (len == 0) {
      // Jsonnet always renders empty arrays as "[ ]" (with space), matching google/jsonnet
      buf.append(' ')
    } else if (compact) {
      // Compact mode: [v1, v2, v3] with space after comma, no newlines
      writeVal(arr.value(0))
      var i = 1
      while (i < len) {
        buf.ensureLength(2)
        buf.appendUnsafe(',')
        buf.appendUnsafe(' ')
        writeVal(arr.value(i))
        i += 1
      }
    } else {
      // Pretty mode: newline + indent between elements
      depth += 1
      renderIndent()
      writeVal(arr.value(0))
      var i = 1
      while (i < len) {
        buf.append(',')
        renderIndent()
        writeVal(arr.value(i))
        i += 1
      }
      depth -= 1
      renderIndent()
    }
    buf.append(']')
    maybeFlush()
  }

  private def writeObj(obj: Val.Obj): Unit = {
    obj.triggerAllAsserts(brokenAssertionLogic)
    val keys =
      if (sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
      else obj.visibleKeyNames
    val len = keys.length
    buf.append('{')
    if (len == 0) {
      // Jsonnet always renders empty objects as "{ }" (with space), matching google/jsonnet
      buf.append(' ')
    } else if (compact) {
      // Compact mode: {"k1": v1, "k2": v2} with space after comma, no newlines
      writeKeyValue(keys(0), obj)
      var i = 1
      while (i < len) {
        buf.ensureLength(2)
        buf.appendUnsafe(',')
        buf.appendUnsafe(' ')
        writeKeyValue(keys(i), obj)
        i += 1
      }
    } else {
      // Pretty mode: newline + indent between key-value pairs
      depth += 1
      renderIndent()
      writeKeyValue(keys(0), obj)
      var i = 1
      while (i < len) {
        buf.append(',')
        renderIndent()
        writeKeyValue(keys(i), obj)
        i += 1
      }
      depth -= 1
      renderIndent()
    }
    buf.append('}')
    maybeFlush()
  }

  /** Render "key": value pair. Colon always followed by space (Jsonnet convention). */
  @inline private def writeKeyValue(key: String, obj: Val.Obj): Unit = {
    writeString(key)
    buf.ensureLength(2)
    buf.appendUnsafe(':')
    buf.appendUnsafe(' ')
    writeVal(obj.value(key, emptyPos))
  }

  /**
   * Write a JSON string with proper escaping. Uses the same fast-path pre-scan as BaseCharRenderer:
   * bulk-copy clean strings via String.getChars, fall back to per-character escaping only when
   * needed.
   */
  private def writeString(s: String): Unit = {
    val len = s.length
    // Pre-scan for characters that need escaping
    var needsEscape = false
    var i = 0
    while (i < len && !needsEscape) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\') needsEscape = true
      i += 1
    }
    if (!needsEscape) {
      // Fast path: bulk copy entire string with surrounding quotes
      buf.ensureLength(len + 2)
      buf.appendUnsafe('"')
      val cbArr = buf.arr
      val pos = buf.getLength
      s.getChars(0, len, cbArr, pos)
      buf.length = pos + len
      buf.appendUnsafe('"')
    } else {
      // Slow path: delegate to upickle's per-character escapeChar
      upickle.core.RenderUtils.escapeChar(
        null,
        buf,
        s,
        escapeUnicode = false,
        wrapQuotes = true
      )
    }
  }

  /**
   * Write a Jsonnet number. Matches Renderer behavior:
   *   - Integer doubles rendered without decimal point (via appendLong)
   *   - Large whole doubles rendered via BigDecimal (e.g. 1e20)
   *   - Fractional doubles rendered via Double.toString
   */
  private def writeNum(d: Double): Unit = {
    val i = d.toLong
    if (d == i) {
      RenderUtils.appendLong(buf, i)
    } else if (d % 1 == 0) {
      appendStringDirect(
        BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
      )
    } else {
      appendStringDirect(d.toString)
    }
  }

  /** Bulk-copy a String into the char buffer using String.getChars (no escaping). */
  private def appendStringDirect(s: String): Unit = {
    val len = s.length
    buf.ensureLength(len)
    val cbArr = buf.arr
    val pos = buf.getLength
    s.getChars(0, len, cbArr, pos)
    buf.length = pos + len
  }

  /** Render newline + indentation. Only used in pretty mode (indent > 0). */
  @inline private def renderIndent(): Unit = {
    if (indentCache != null && depth < BaseCharRenderer.MaxCachedDepth) {
      val cached = indentCache(depth)
      buf.appendAll(cached, cached.length)
    } else {
      var i = indent * depth
      buf.ensureLength(i + 1)
      buf.append('\n')
      while (i > 0) {
        buf.append(' ')
        i -= 1
      }
    }
  }

  /**
   * Flush CharBuilder to Writer when it gets large, reducing peak memory. Threshold of 1000 chars
   * keeps memory bounded while avoiding excessive flush overhead.
   */
  @inline private def maybeFlush(): Unit = {
    buf.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }
}
