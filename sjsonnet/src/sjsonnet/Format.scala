package sjsonnet

/**
 * Minimal re-implementation of Python's `%` formatting logic, since Jsonnet's `%` formatter is
 * basically "do whatever python does", with a link to:
 *
 *   - https://docs.python.org/2/library/stdtypes.html#string-formatting
 *
 * Parses the formatted strings into a sequence of literal strings separated by `%` interpolations
 * modelled as structured [[Format.FormatSpec]]s, and use those to decide how to inteprolate the
 * provided Jsonnet [[Val]]s into the final string.
 */
object Format {

  /**
   * Opaque marker trait for compiled format entries stored in [[FormatCache]]. The internal
   * representation ([[RuntimeFormat]]) is package-private so that cache implementations don't
   * depend on format internals.
   */
  sealed trait CompiledFormat

  /**
   * Pre-processed format string: arrays for indexed access, metadata for fast-path checks. Not a
   * case class because Array fields have reference-based equals/hashCode.
   */
  private[sjsonnet] final class RuntimeFormat(
      val leading: String,
      val specs: Array[FormatSpec],
      val literals: Array[String],
      val hasAnyStar: Boolean,
      val staticChars: Int)
      extends CompiledFormat

  final case class FormatSpec(
      label: Option[String],
      alternate: Boolean,
      zeroPadded: Boolean,
      leftAdjusted: Boolean,
      blankBeforePositive: Boolean,
      signCharacter: Boolean,
      width: Option[Int],
      widthStar: Boolean,
      precision: Option[Int],
      precisionStar: Boolean,
      conversion: Char) {
    def updateWithStarValues(newWidth: Option[Int], newPrecision: Option[Int]): FormatSpec = {
      this.copy(
        width = newWidth.orElse(this.width),
        widthStar = newWidth.isDefined || this.widthStar,
        precision = newPrecision.orElse(this.precision),
        precisionStar = newPrecision.isDefined || this.precisionStar
      )
    }
  }
  import fastparse._, NoWhitespace._
  def integer[$: P]: P[Unit] = P(CharIn("1-9") ~ CharsWhileIn("0-9", 0) | "0")
  def label[$: P]: P[Option[String]] = P(("(" ~ CharsWhile(_ != ')', 0).! ~ ")").?)
  def flags[$: P]: P[String] = P(CharsWhileIn("#0\\- +", 0).!)
  def width[$: P]: P[Option[String]] = P((integer | "*").!.?)
  def precision[$: P]: P[Option[String]] = P(("." ~/ (integer | "*").!).?)
  def conversion[$: P]: P[String] = P(CharIn("diouxXeEfFgGcrsa%").!)
  def formatSpec[$: P]: P[FormatSpec] = P(
    label ~ flags ~ width ~ precision ~ CharIn("hlL").? ~ conversion
  ).map { case (label, flags, width, precision, conversion) =>
    FormatSpec(
      label,
      flags.contains('#'),
      flags.contains('0'),
      flags.contains('-'),
      flags.contains(' '),
      flags.contains('+'),
      width.filterNot(_ == "*").map(_.toInt),
      width.contains("*"),
      precision.filterNot(_ == "*").map(_.toInt),
      precision.contains("*"),
      conversion.charAt(0)
    )
  }

  def plain[$: P]: P[String] = P(CharsWhile(_ != '%', 0).!)
  def format[$: P]: P[(String, Seq[(FormatSpec, String)])] = P(
    plain ~ (("%" ~/ formatSpec) ~ plain).rep ~ End
  )

  def widenRaw(formatted: FormatSpec, txt: String): String =
    if (formatted.width.isEmpty) txt // fast path: no width/padding needed
    else widen(formatted, "", "", txt, numeric = false, signedConversion = false)
  def widen(
      formatted: FormatSpec,
      lhs: String,
      mhs: String,
      rhs: String,
      numeric: Boolean,
      signedConversion: Boolean): String = {

    val lhs2 =
      if (signedConversion && formatted.blankBeforePositive) " " + lhs
      else if (signedConversion && formatted.signCharacter) "+" + lhs
      else lhs

    val missingWidth = formatted.width.getOrElse(-1) - lhs2.length - mhs.length - rhs.length

    if (missingWidth <= 0) {
      // Avoid unnecessary string concatenation when parts are empty
      if (lhs2.isEmpty && mhs.isEmpty) rhs
      else if (lhs2.isEmpty) mhs + rhs
      else lhs2 + mhs + rhs
    } else if (formatted.zeroPadded) {
      if (numeric) lhs2 + mhs + "0" * missingWidth + rhs
      else {
        if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
        else " " * missingWidth + lhs2 + mhs + rhs
      }
    } else if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
    else " " * missingWidth + lhs2 + mhs + rhs
  }

  def format(s: String, values0: Val, pos: Position)(implicit evaluator: EvalScope): String = {
    val parsed = parseFormatCached(s, evaluator.formatCache)
    format(parsed, values0, pos)
  }

  /** Look up or parse and cache a format string using the provided [[FormatCache]]. */
  private def parseFormatCached(s: String, cache: FormatCache): RuntimeFormat = {
    // CompiledFormat is sealed with RuntimeFormat as the only subtype, so this match is exhaustive.
    // Using pattern match instead of asInstanceOf for explicit safety.
    cache.getOrElseUpdate(s, scanFormat(s)) match {
      case rf: RuntimeFormat => rf
    }
  }

  /**
   * Hand-written format string scanner. Replaces the fastparse-based parser with direct
   * `String.indexOf('%')` scanning, which is a JVM intrinsic / native SIMD-optimized operation. For
   * large format strings (e.g. 605KB large_string_template with 256 interpolations), this avoids
   * the overhead of running parser combinators over hundreds of KB of literal text.
   */
  private def scanFormat(s: String): RuntimeFormat = {
    val len = s.length
    val specsBuilder = new java.util.ArrayList[FormatSpec]()
    val literalsBuilder = new java.util.ArrayList[String]()
    var staticChars = 0
    var hasAnyStar = false

    // Find the first '%' to extract the leading literal
    var pos = s.indexOf('%')
    val leading =
      if (pos < 0) s // No format specs at all
      else s.substring(0, pos)
    staticChars += leading.length

    while (pos >= 0 && pos < len) {
      pos += 1 // skip the '%'
      if (pos >= len) throw new Exception("Truncated format code at end of string")

      // Parse format spec: %(label)flags width.precision [hlL] conversion
      // 1. Optional label: (key)
      val label: Option[String] =
        if (s.charAt(pos) == '(') {
          val closeIdx = s.indexOf(')', pos + 1)
          if (closeIdx < 0) throw new Exception("Unterminated ( in format spec")
          val key = s.substring(pos + 1, closeIdx)
          pos = closeIdx + 1
          Some(key)
        } else None

      // 2. Flags: #0- +
      var alternate = false
      var zeroPadded = false
      var leftAdjusted = false
      var blankBeforePositive = false
      var signCharacter = false
      var parsingFlags = pos < len
      while (parsingFlags) {
        s.charAt(pos) match {
          case '#' => alternate = true; pos += 1
          case '0' => zeroPadded = true; pos += 1
          case '-' => leftAdjusted = true; pos += 1
          case ' ' => blankBeforePositive = true; pos += 1
          case '+' => signCharacter = true; pos += 1
          case _   => parsingFlags = false
        }
        if (pos >= len) parsingFlags = false
      }

      // 3. Width: integer or *
      var width: Option[Int] = None
      var widthStar = false
      if (pos < len) {
        val c = s.charAt(pos)
        if (c == '*') {
          widthStar = true
          pos += 1
        } else if (c >= '1' && c <= '9') {
          var w = c - '0'
          pos += 1
          while (pos < len && s.charAt(pos) >= '0' && s.charAt(pos) <= '9') {
            w = w * 10 + (s.charAt(pos) - '0')
            pos += 1
          }
          width = Some(w)
        } else if (c == '0' && zeroPadded) {
          // '0' was already consumed as a flag, but '0' followed by digits could be width
          // The flag loop already handled leading '0', skip
        }
      }

      // 4. Precision: .integer or .*
      var precision: Option[Int] = None
      var precisionStar = false
      if (pos < len && s.charAt(pos) == '.') {
        pos += 1
        if (pos < len) {
          val c = s.charAt(pos)
          if (c == '*') {
            precisionStar = true
            pos += 1
          } else if (c >= '0' && c <= '9') {
            var p = c - '0'
            pos += 1
            while (pos < len && s.charAt(pos) >= '0' && s.charAt(pos) <= '9') {
              p = p * 10 + (s.charAt(pos) - '0')
              pos += 1
            }
            precision = Some(p)
          } else {
            // "." with no digits = precision 0
            precision = Some(0)
          }
        }
      }

      // 5. Optional length modifier: h, l, L (ignored per Python spec)
      if (pos < len) {
        val c = s.charAt(pos)
        if (c == 'h' || c == 'l' || c == 'L') pos += 1
      }

      // 6. Conversion character
      if (pos >= len) throw new Exception("Truncated format code at end of string")
      val conversion = s.charAt(pos)
      pos += 1
      if ("diouxXeEfFgGcrsa%".indexOf(conversion) < 0)
        throw new Exception(s"Unrecognized conversion type: $conversion")

      specsBuilder.add(
        FormatSpec(
          label,
          alternate,
          zeroPadded,
          leftAdjusted,
          blankBeforePositive,
          signCharacter,
          width,
          widthStar,
          precision,
          precisionStar,
          conversion
        )
      )
      hasAnyStar ||= widthStar || precisionStar

      // Find next '%' to extract the literal between this spec and the next
      val nextPct = s.indexOf('%', pos)
      val literal =
        if (nextPct < 0) s.substring(pos) // Rest of string is literal
        else s.substring(pos, nextPct)
      literalsBuilder.add(literal)
      staticChars += literal.length

      pos = nextPct
    }

    val size = specsBuilder.size()
    val specs = new Array[FormatSpec](size)
    val literals = new Array[String](size)
    var idx = 0
    while (idx < size) {
      specs(idx) = specsBuilder.get(idx)
      literals(idx) = literalsBuilder.get(idx)
      idx += 1
    }
    new RuntimeFormat(leading, specs, literals, hasAnyStar, staticChars)
  }

  /** Convert a parsed format (leading + Seq of tuples) into a RuntimeFormat with arrays. */
  private def lowerParsedFormat(
      parsed: (String, scala.Seq[(FormatSpec, String)])): RuntimeFormat = {
    val (leading, chunks) = parsed
    val size = chunks.size
    val specs = new Array[FormatSpec](size)
    val literals = new Array[String](size)
    var staticChars = leading.length
    var hasAnyStar = false
    var idx = 0
    while (idx < size) {
      val (formatted, literal) = chunks(idx)
      specs(idx) = formatted
      literals(idx) = literal
      staticChars += literal.length
      hasAnyStar ||= formatted.widthStar || formatted.precisionStar
      idx += 1
    }
    new RuntimeFormat(leading, specs, literals, hasAnyStar, staticChars)
  }

  def format(leading: String, chunks: scala.Seq[(FormatSpec, String)], values0: Val, pos: Position)(
      implicit evaluator: EvalScope): String = {
    format(lowerParsedFormat((leading, chunks)), values0, pos)
  }

  private def format(parsed: RuntimeFormat, values0: Val, pos: Position)(implicit
      evaluator: EvalScope): String = {
    val values = values0 match {
      case x: Val.Arr => x
      case x: Val.Obj => x
      case x          => Val.Arr(pos, Array[Eval](x))
    }
    val numSpecs = parsed.specs.length
    if (numSpecs == 0) {
      if (values.isInstanceOf[Val.Arr] && values.cast[Val.Arr].length > 0) {
        Error.fail(
          "Too many values to format: %d, expected %d".format(values.cast[Val.Arr].length, 0)
        )
      }
      return parsed.leading
    }

    // Pass 1: compute all formatted values into an array
    val formattedValues = new Array[String](numSpecs)
    var i = 0
    var idx = 0
    while (idx < numSpecs) {
      val rawFormatted = parsed.specs(idx)
      var formatted = rawFormatted
      val cooked0 = formatted.conversion match {
        case '%' => widenRaw(formatted, "%")
        case _   =>
          if (values.isInstanceOf[Val.Arr] && i >= values.cast[Val.Arr].length) {
            Error.fail(
              "Too few values to format: %d, expected at least %d".format(
                values.cast[Val.Arr].length,
                i + 1
              )
            )
          }
          val raw = formatted.label match {
            case None =>
              // Fast path: skip star checks when format has no * specifiers
              if (!parsed.hasAnyStar) values.cast[Val.Arr].value(i)
              else
                (formatted.widthStar, formatted.precisionStar) match {
                  case (false, false) => values.cast[Val.Arr].value(i)
                  case (true, false)  =>
                    val width = values.cast[Val.Arr].value(i)
                    if (!width.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a width"
                          .format(
                            idx
                          )
                      )
                    }
                    i += 1
                    formatted = formatted.updateWithStarValues(Some(width.asInt), None)
                    values.cast[Val.Arr].value(i)
                  case (false, true) =>
                    val precision = values.cast[Val.Arr].value(i)
                    if (!precision.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a precision"
                          .format(idx)
                      )
                    }
                    i += 1
                    formatted = formatted.updateWithStarValues(None, Some(precision.asInt))
                    values.cast[Val.Arr].value(i)
                  case (true, true) =>
                    val width = values.cast[Val.Arr].value(i)
                    if (!width.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a width"
                          .format(
                            idx
                          )
                      )
                    }
                    i += 1
                    val precision = values.cast[Val.Arr].value(i)
                    if (!precision.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a precision"
                          .format(idx)
                      )
                    }
                    i += 1
                    formatted =
                      formatted.updateWithStarValues(Some(width.asInt), Some(precision.asInt))
                    values.cast[Val.Arr].value(i)
                }
            case Some(key) =>
              values match {
                case v: Val.Arr => v.value(i)
                case v: Val.Obj => v.value(key, pos)
                case _          => Error.fail("Invalid format values")
              }
          }
          // Direct Val dispatch: skip Materializer for common types (Str, Num, Bool, Null).
          // This avoids the overhead of materializing to ujson.Value and then matching on it,
          // which is a significant cost for format-heavy workloads like large_string_template.
          val rawVal = raw.value
          val formattedValue = rawVal match {
            case f: Val.Func => Error.fail("Cannot format function value", f)
            case vs: Val.Str =>
              if (formatted.conversion != 's' && formatted.conversion != 'c')
                Error.fail("Format required a number at %d, got string".format(i))
              widenRaw(formatted, vs.str)
            case vn: Val.Num =>
              val s = vn.asDouble
              formatted.conversion match {
                case 'd' | 'i' | 'u' => formatInteger(formatted, s)
                case 'o'             => formatOctal(formatted, s)
                case 'x'             => formatHexadecimal(formatted, s)
                case 'X'             => formatHexadecimal(formatted, s).toUpperCase
                case 'e'             => formatExponent(formatted, s).toLowerCase
                case 'E'             => formatExponent(formatted, s)
                case 'f' | 'F'       => formatFloat(formatted, s)
                case 'g'             => formatGeneric(formatted, s).toLowerCase
                case 'G'             => formatGeneric(formatted, s)
                case 'c'             => widenRaw(formatted, Character.toString(s.toInt))
                case 's'             =>
                  if (s.toLong == s) widenRaw(formatted, s.toLong.toString)
                  else widenRaw(formatted, s.toString)
                case _ =>
                  Error.fail("Format required a %s at %d, got string".format(rawVal.prettyName, i))
              }
            case _: Val.True =>
              val b = 1
              formatted.conversion match {
                case 'd' | 'i' | 'u' => formatInteger(formatted, b)
                case 'o'             => formatOctal(formatted, b)
                case 'x'             => formatHexadecimal(formatted, b)
                case 'X'             => formatHexadecimal(formatted, b).toUpperCase
                case 'e'             => formatExponent(formatted, b).toLowerCase
                case 'E'             => formatExponent(formatted, b)
                case 'f' | 'F'       => formatFloat(formatted, b)
                case 'g'             => formatGeneric(formatted, b).toLowerCase
                case 'G'             => formatGeneric(formatted, b)
                case 'c'             => widenRaw(formatted, Character.forDigit(b, 10).toString)
                case 's'             => widenRaw(formatted, "true")
                case _               =>
                  Error.fail("Format required a %s at %d, got string".format(rawVal.prettyName, i))
              }
            case _: Val.False =>
              val b = 0
              formatted.conversion match {
                case 'd' | 'i' | 'u' => formatInteger(formatted, b)
                case 'o'             => formatOctal(formatted, b)
                case 'x'             => formatHexadecimal(formatted, b)
                case 'X'             => formatHexadecimal(formatted, b).toUpperCase
                case 'e'             => formatExponent(formatted, b).toLowerCase
                case 'E'             => formatExponent(formatted, b)
                case 'f' | 'F'       => formatFloat(formatted, b)
                case 'g'             => formatGeneric(formatted, b).toLowerCase
                case 'G'             => formatGeneric(formatted, b)
                case 'c'             => widenRaw(formatted, Character.forDigit(b, 10).toString)
                case 's'             => widenRaw(formatted, "false")
                case _               =>
                  Error.fail("Format required a %s at %d, got string".format(rawVal.prettyName, i))
              }
            case _: Val.Null =>
              widenRaw(formatted, "null")
            case _ =>
              // Complex types (Arr, Obj): materialize via Renderer
              val value = rawVal match {
                case r: Val.Arr => Materializer.apply0(r, new Renderer(indent = -1))
                case r: Val.Obj => Materializer.apply0(r, new Renderer(indent = -1))
                case _          => Materializer(rawVal)
              }
              widenRaw(formatted, value.toString)
          }
          i += 1
          formattedValue
      }
      formattedValues(idx) = cooked0
      idx += 1
    }

    if (values.isInstanceOf[Val.Arr] && i < values.cast[Val.Arr].length) {
      Error.fail(
        "Too many values to format: %d, expected %d".format(values.cast[Val.Arr].length, i)
      )
    }

    // Pass 2: compute exact output length
    var totalLen = parsed.leading.length
    idx = 0
    while (idx < numSpecs) {
      totalLen += formattedValues(idx).length + parsed.literals(idx).length
      idx += 1
    }

    // Pass 3: assemble into pre-sized char[] — eliminates StringBuilder overhead
    // (capacity checks, resizing, final toString copy)
    val chars = new Array[Char](totalLen)
    var cPos = 0
    val leading = parsed.leading
    val leadLen = leading.length
    if (leadLen > 0) {
      leading.getChars(0, leadLen, chars, cPos)
      cPos += leadLen
    }
    idx = 0
    while (idx < numSpecs) {
      val fv = formattedValues(idx)
      val fvLen = fv.length
      if (fvLen > 0) {
        fv.getChars(0, fvLen, chars, cPos)
        cPos += fvLen
      }
      val lit = parsed.literals(idx)
      val litLen = lit.length
      if (litLen > 0) {
        lit.getChars(0, litLen, chars, cPos)
        cPos += litLen
      }
      idx += 1
    }
    new String(chars)
  }

  private def formatInteger(formatted: FormatSpec, s: Double): String = {
    // Fast path: if the value fits in a Long (and isn't Long.MinValue where
    // negation overflows), avoid BigInt allocation entirely
    val sl = s.toLong
    if (sl.toDouble == s && sl != Long.MinValue) {
      val negative = sl < 0
      val lhs = if (negative) "-" else ""
      val rhs = java.lang.Long.toString(if (negative) -sl else sl)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(formatted, lhs, "", rhs2, numeric = true, signedConversion = !negative)
    } else {
      val i = BigDecimal(s).toBigInt
      val negative = i.signum < 0
      val lhs = if (negative) "-" else ""
      val rhs = i.abs.toString(10)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(formatted, lhs, "", rhs2, numeric = true, signedConversion = !negative)
    }
  }

  private def formatFloat(formatted: FormatSpec, s: Double): String = {
    widen(
      formatted,
      if (s < 0) "-" else "",
      "",
      sjsonnet.DecimalFormat
        .format(
          formatted.precision.getOrElse(6),
          0,
          formatted.alternate,
          None,
          math.abs(s)
        )
        .replace("E", "E+"),
      numeric = true,
      signedConversion = s > 0
    )

  }

  private def formatOctal(formatted: FormatSpec, s: Double): String = {
    // Fast path: if the value fits in a Long, avoid BigInt allocation
    val sl = s.toLong
    if (sl.toDouble == s && sl != Long.MinValue) {
      val negative = sl < 0
      val lhs = if (negative) "-" else ""
      val rhs = java.lang.Long.toString(if (negative) -sl else sl, 8)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(
        formatted,
        lhs,
        if (!formatted.alternate || rhs2.charAt(0) == '0') "" else "0",
        rhs2,
        numeric = true,
        signedConversion = !negative
      )
    } else {
      val i = BigDecimal(s).toBigInt
      val negative = i.signum < 0
      val lhs = if (negative) "-" else ""
      val rhs = i.abs.toString(8)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(
        formatted,
        lhs,
        if (!formatted.alternate || rhs2.charAt(0) == '0') "" else "0",
        rhs2,
        numeric = true,
        signedConversion = !negative
      )
    }
  }

  private def formatHexadecimal(formatted: FormatSpec, s: Double): String = {
    // Fast path: if the value fits in a Long, avoid BigInt allocation
    val sl = s.toLong
    if (sl.toDouble == s && sl != Long.MinValue) {
      val negative = sl < 0
      val lhs = if (negative) "-" else ""
      val rhs = java.lang.Long.toString(if (negative) -sl else sl, 16)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(
        formatted,
        lhs,
        if (!formatted.alternate) "" else "0x",
        rhs2,
        numeric = true,
        signedConversion = !negative
      )
    } else {
      val i = BigDecimal(s).toBigInt
      val negative = i.signum < 0
      val lhs = if (negative) "-" else ""
      val rhs = i.abs.toString(16)
      val rhs2 = precisionPad(lhs, rhs, formatted.precision)
      widen(
        formatted,
        lhs,
        if (!formatted.alternate) "" else "0x",
        rhs2,
        numeric = true,
        signedConversion = !negative
      )
    }
  }

  private def precisionPad(lhs: String, rhs: String, precision: Option[Int]): String = {
    precision match {
      case None    => rhs
      case Some(p) =>
        val shortage = p - rhs.length
        if (shortage > 0) "0" * shortage + rhs else rhs
    }
  }

  private def formatGeneric(formatted: FormatSpec, s: Double): String = {
    val precision = formatted.precision.getOrElse(6)
    val exponent = if (s != 0) math.floor(math.log10(math.abs(s))).toInt else 0
    if (exponent < -4 || exponent >= precision) {
      widen(
        formatted,
        if (s < 0) "-" else "",
        "",
        sjsonnet.DecimalFormat
          .format(
            if (formatted.alternate) precision - 1 else 0,
            if (formatted.alternate) 0 else precision - 1,
            formatted.alternate,
            Some(2),
            math.abs(s)
          )
          .replace("E", "E+"),
        numeric = true,
        signedConversion = s > 0
      )
    } else {
      val digitsBeforePoint = math.max(1, exponent + 1)
      widen(
        formatted,
        if (s < 0) "-" else "",
        "",
        sjsonnet.DecimalFormat
          .format(
            if (formatted.alternate) precision - digitsBeforePoint else 0,
            if (formatted.alternate) 0 else precision - digitsBeforePoint,
            formatted.alternate,
            None,
            math.abs(s)
          )
          .replace("E", "E+"),
        numeric = true,
        signedConversion = s > 0
      )
    }

  }

  private def formatExponent(formatted: FormatSpec, s: Double): String = {
    widen(
      formatted,
      if (s < 0) "-" else "",
      "",
      sjsonnet.DecimalFormat
        .format(
          formatted.precision.getOrElse(6),
          0,
          formatted.alternate,
          Some(2),
          math.abs(s)
        )
        .replace("E", "E+"),
      numeric = true,
      signedConversion = s > 0
    )
  }

  /**
   * Pre-compiled format string for constant `"..." % expr` patterns detected by
   * [[StaticOptimizer]]. The format is parsed once at construction time and stored directly in this
   * instance, bypassing [[FormatCache]] since each literal format string is unique and already
   * cached within the AST.
   */
  class PartialApplyFmt(fmt: String) extends Val.Builtin1("format", "values") {
    // Pre-parse the format string at construction time (during static optimization).
    // Uses the hand-written scanner instead of fastparse for faster parsing of large format strings.
    // Each PartialApplyFmt instance caches its own parsed format, so no external cache needed.
    private val parsed = scanFormat(fmt)
    def evalRhs(values0: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, format(parsed, values0.value, pos)(ev))
  }
}
