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
   * Pre-processed format string: primitive arrays for indexed access, metadata for fast-path
   * checks. Not a case class because Array fields have reference-based equals/hashCode.
   */
  private[sjsonnet] final class RuntimeFormat(
      val leading: String,
      val specBits: Array[Long],
      /** Null when the format contains no labels, otherwise one entry per spec. */
      val labels: Array[String],
      val literals: Array[String],
      val hasAnyStar: Boolean,
      val staticChars: Int,
      /** Original format string for offset-based appends; null for legacy lowered formats. */
      val source: String,
      /** Char offsets into `source` for the leading literal: [start, end). */
      val leadingStart: Int,
      val leadingEnd: Int,
      /** Char offsets into `source` for each literal following a format spec. */
      val literalStarts: Array[Int],
      val literalEnds: Array[Int],
      /** Non-null when all simple named specs use the same label. */
      val singleNamedLabel: String,
      /**
       * True when ALL specs are simple `%(key)s` with a named label and no formatting flags. In
       * this case we can use a fast path that caches the object key lookup and avoids widenRaw
       * entirely.
       */
      val allSimpleNamedString: Boolean)
      extends CompiledFormat

  final class FormatSpec private (val bits: Long) extends AnyVal {
    import FormatSpec._

    def conversion: Char = (bits & ConversionMask).toChar
    private def flags: Int = ((bits >>> FlagsShift) & FlagsMask).toInt

    def hasLabel: Boolean = (flags & HasLabelFlag) != 0
    def alternate: Boolean = (flags & AlternateFlag) != 0
    def zeroPadded: Boolean = (flags & ZeroPaddedFlag) != 0
    def leftAdjusted: Boolean = (flags & LeftAdjustedFlag) != 0
    def blankBeforePositive: Boolean = (flags & BlankBeforePositiveFlag) != 0
    def signCharacter: Boolean = (flags & SignCharacterFlag) != 0
    def widthStar: Boolean = (flags & WidthStarFlag) != 0
    def precisionStar: Boolean = (flags & PrecisionStarFlag) != 0

    def widthValue: Int = decodeNumber((bits >>> WidthShift) & NumberMask)
    def precisionValue: Int = decodeNumber((bits >>> PrecisionShift) & NumberMask)
    def hasWidth: Boolean = widthValue != NoNumber
    def hasPrecision: Boolean = precisionValue != NoNumber
    def widthOr(default: Int): Int = {
      val w = widthValue
      if (w == NoNumber) default else w
    }
    def precisionOr(default: Int): Int = {
      val p = precisionValue
      if (p == NoNumber) default else p
    }

    /** Compatibility helpers for the legacy fastparse parser path. Avoid these in hot loops. */
    def width: Option[Int] = if (hasWidth) Some(widthValue) else None
    def precision: Option[Int] = if (hasPrecision) Some(precisionValue) else None

    /** True when this spec is a simple `%s` or `%(key)s` with no formatting flags. */
    def isSimpleString: Boolean =
      conversion == 's' && !hasWidth && !hasPrecision &&
      (flags & SimpleStringDisqualifierFlags) == 0

    def withStarWidth(newWidth: Int): FormatSpec =
      new FormatSpec((bits & ~(NumberMask << WidthShift)) | (encodeNumber(newWidth) << WidthShift))

    def withStarPrecision(newPrecision: Int): FormatSpec =
      new FormatSpec(
        (bits & ~(NumberMask << PrecisionShift)) | (encodeNumber(newPrecision) << PrecisionShift)
      )

    def withStarValues(newWidth: Int, newPrecision: Int): FormatSpec =
      withStarWidth(newWidth).withStarPrecision(newPrecision)
  }

  object FormatSpec {
    private val ConversionMask = 0xffL
    private val FlagsShift = 8
    private val FlagsMask = 0xffL
    private val WidthShift = 16
    private val PrecisionShift = 40
    private val NumberMask = 0xffffffL
    private val NumberBias = 1 << 23
    private val EncodedNoNumber = NumberMask
    private val MinNumber = -NumberBias
    private val MaxNumber = NumberBias - 2

    private val AlternateFlag = 1
    private val ZeroPaddedFlag = 1 << 1
    private val LeftAdjustedFlag = 1 << 2
    private val BlankBeforePositiveFlag = 1 << 3
    private val SignCharacterFlag = 1 << 4
    private val WidthStarFlag = 1 << 5
    private val PrecisionStarFlag = 1 << 6
    private val HasLabelFlag = 1 << 7
    private val SimpleStringDisqualifierFlags =
      AlternateFlag | ZeroPaddedFlag | LeftAdjustedFlag | BlankBeforePositiveFlag |
      SignCharacterFlag | WidthStarFlag | PrecisionStarFlag

    final val NoNumber = Int.MinValue

    private def encodeNumber(value: Int): Long = {
      if (value == NoNumber) EncodedNoNumber
      else if (value < MinNumber || value > MaxNumber) {
        throw new Exception("Format width/precision is too large: " + value)
      } else {
        (value + NumberBias).toLong
      }
    }

    private def decodeNumber(encoded: Long): Int =
      if (encoded == EncodedNoNumber) NoNumber else encoded.toInt - NumberBias

    private[sjsonnet] def appendNumberDigit(current: Int, digit: Int): Int = {
      if (current > (MaxNumber - digit) / 10) {
        throw new Exception("Format width/precision is too large")
      }
      current * 10 + digit
    }

    def fromBits(bits: Long): FormatSpec = new FormatSpec(bits)

    def apply(
        hasLabel: Boolean,
        alternate: Boolean,
        zeroPadded: Boolean,
        leftAdjusted: Boolean,
        blankBeforePositive: Boolean,
        signCharacter: Boolean,
        width: Int,
        widthStar: Boolean,
        precision: Int,
        precisionStar: Boolean,
        conversion: Char): FormatSpec = {
      var flags = 0
      if (hasLabel) flags |= HasLabelFlag
      if (alternate) flags |= AlternateFlag
      if (zeroPadded) flags |= ZeroPaddedFlag
      if (leftAdjusted) flags |= LeftAdjustedFlag
      if (blankBeforePositive) flags |= BlankBeforePositiveFlag
      if (signCharacter) flags |= SignCharacterFlag
      if (widthStar) flags |= WidthStarFlag
      if (precisionStar) flags |= PrecisionStarFlag

      val bits =
        (conversion.toLong & ConversionMask) |
        (flags.toLong << FlagsShift) |
        (encodeNumber(width) << WidthShift) |
        (encodeNumber(precision) << PrecisionShift)
      new FormatSpec(bits)
    }

    def apply(
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
        conversion: Char): FormatSpec =
      apply(
        label.isDefined,
        alternate,
        zeroPadded,
        leftAdjusted,
        blankBeforePositive,
        signCharacter,
        width.getOrElse(NoNumber),
        widthStar,
        precision.getOrElse(NoNumber),
        precisionStar,
        conversion
      )
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
    if (!formatted.hasWidth) txt // fast path: no width/padding needed
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

    val missingWidth = formatted.widthOr(-1) - lhs2.length - mhs.length - rhs.length

    if (missingWidth <= 0) {
      // Avoid unnecessary string concatenation when parts are empty
      if (lhs2.isEmpty && mhs.isEmpty) rhs
      else if (lhs2.isEmpty) mhs + rhs
      else lhs2 + mhs + rhs
    } else {
      val padding =
        if (formatted.zeroPadded && numeric) Platform.repeatString("0", missingWidth)
        else Platform.repeatString(" ", missingWidth)
      if (formatted.zeroPadded && numeric) lhs2 + mhs + padding + rhs
      else if (formatted.leftAdjusted) lhs2 + mhs + rhs + padding
      else padding + lhs2 + mhs + rhs
    }
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

  private def sameRegion(s: String, start: Int, len: Int, value: String): Boolean = {
    if (value.length != len) false
    else {
      var i = 0
      while (i < len && s.charAt(start + i) == value.charAt(i)) i += 1
      i == len
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
    val specsBuilder = new scala.collection.mutable.ArrayBuilder.ofLong
    var labelsBuilder: java.util.ArrayList[String] = null
    val litStartsBuilder = new scala.collection.mutable.ArrayBuilder.ofInt
    val litEndsBuilder = new scala.collection.mutable.ArrayBuilder.ofInt
    var staticChars = 0
    var hasAnyStar = false
    var specCount = 0
    var allSimpleNamed = true
    var lastLabel: String = null
    var firstNamedLabel: String = null
    var allNamedLabelsSame = true

    // Find the first '%' to extract the leading literal
    var pos = s.indexOf('%')
    val leadingStart = 0
    val leadingEnd = if (pos < 0) len else pos
    staticChars += leadingEnd - leadingStart

    while (pos >= 0 && pos < len) {
      pos += 1 // skip the '%'
      if (pos >= len) throw new Exception("Truncated format code at end of string")

      // Parse format spec: %(label)flags width.precision [hlL] conversion
      // 1. Optional label: (key)
      val label: String =
        if (s.charAt(pos) == '(') {
          val labelStart = pos + 1
          val closeIdx = s.indexOf(')', pos + 1)
          if (closeIdx < 0) throw new Exception("Unterminated ( in format spec")
          val labelLen = closeIdx - labelStart
          val key =
            if (lastLabel != null && sameRegion(s, labelStart, labelLen, lastLabel)) lastLabel
            else {
              val nextLabel = s.substring(labelStart, closeIdx)
              lastLabel = nextLabel
              nextLabel
            }
          pos = closeIdx + 1
          if (labelsBuilder == null) {
            labelsBuilder = new java.util.ArrayList[String]()
            var fill = 0
            while (fill < specCount) {
              labelsBuilder.add(null)
              fill += 1
            }
          }
          if (firstNamedLabel == null) firstNamedLabel = key
          else if (allNamedLabelsSame && key != firstNamedLabel) allNamedLabelsSame = false
          key
        } else { allSimpleNamed = false; null }

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
      var width = FormatSpec.NoNumber
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
            w = FormatSpec.appendNumberDigit(w, s.charAt(pos) - '0')
            pos += 1
          }
          width = w
        } else if (c == '0' && zeroPadded) {
          // '0' was already consumed as a flag, but '0' followed by digits could be width
          // The flag loop already handled leading '0', skip
        }
      }

      // 4. Precision: .integer or .*
      var precision = FormatSpec.NoNumber
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
              p = FormatSpec.appendNumberDigit(p, s.charAt(pos) - '0')
              pos += 1
            }
            precision = p
          } else {
            // "." with no digits = precision 0
            precision = 0
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

      val spec = FormatSpec(
        label != null,
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
      specsBuilder += spec.bits
      if (labelsBuilder != null) labelsBuilder.add(label)
      specCount += 1
      hasAnyStar ||= widthStar || precisionStar
      if (!spec.isSimpleString || label == null) allSimpleNamed = false

      // Find next '%' to extract the literal between this spec and the next
      val nextPct = s.indexOf('%', pos)
      val litStart = pos
      val litEnd = if (nextPct < 0) len else nextPct
      litStartsBuilder += litStart
      litEndsBuilder += litEnd
      staticChars += litEnd - litStart

      pos = nextPct
    }

    val specs = specsBuilder.result()
    val size = specs.length
    val singleNamedLabel =
      if (allSimpleNamed && allNamedLabelsSame && size > 0) firstNamedLabel else null
    val labels =
      if (labelsBuilder == null || singleNamedLabel != null) null else new Array[String](size)
    val litStarts = litStartsBuilder.result()
    val litEnds = litEndsBuilder.result()
    if (labels != null) {
      var idx = 0
      while (idx < size) {
        labels(idx) = labelsBuilder.get(idx)
        idx += 1
      }
    }
    new RuntimeFormat(
      null,
      specs,
      labels,
      null,
      hasAnyStar,
      staticChars,
      s,
      leadingStart,
      leadingEnd,
      litStarts,
      litEnds,
      singleNamedLabel,
      allSimpleNamed
    )
  }

  /** Convert a parsed format (leading + Seq of tuples) into a RuntimeFormat with arrays. */
  private def lowerParsedFormat(
      parsed: (String, scala.Seq[(FormatSpec, String)])): RuntimeFormat = {
    val (leading, chunks) = parsed
    val size = chunks.size
    val specs = new Array[Long](size)
    val literals = new Array[String](size)
    val emptyStarts = new Array[Int](size)
    val emptyEnds = new Array[Int](size)
    var staticChars = leading.length
    var hasAnyStar = false
    var allSimpleNamed = true
    var idx = 0
    while (idx < size) {
      val (formatted, literal) = chunks(idx)
      if (formatted.hasLabel) {
        throw new Exception("Lowered FormatSpec labels require label strings")
      }
      specs(idx) = formatted.bits
      literals(idx) = literal
      staticChars += literal.length
      hasAnyStar ||= formatted.widthStar || formatted.precisionStar
      allSimpleNamed = false
      idx += 1
    }
    // No source string available for fastparse path; offset arrays are unused
    new RuntimeFormat(
      leading,
      specs,
      null,
      literals,
      hasAnyStar,
      staticChars,
      null,
      0,
      0,
      emptyStarts,
      emptyEnds,
      null,
      allSimpleNamed
    )
  }

  def format(leading: String, chunks: scala.Seq[(FormatSpec, String)], values0: Val, pos: Position)(
      implicit evaluator: EvalScope): String = {
    format(lowerParsedFormat((leading, chunks)), values0, pos)
  }

  private def appendLeading(output: java.lang.StringBuilder, parsed: RuntimeFormat): Unit = {
    val source = parsed.source
    if (source != null) output.append(source, parsed.leadingStart, parsed.leadingEnd)
    else output.append(parsed.leading)
  }

  private def appendLiteral(
      output: java.lang.StringBuilder,
      parsed: RuntimeFormat,
      idx: Int): Unit = {
    val source = parsed.source
    if (source != null) output.append(source, parsed.literalStarts(idx), parsed.literalEnds(idx))
    else output.append(parsed.literals(idx))
  }

  private def format(parsed: RuntimeFormat, values0: Val, pos: Position)(implicit
      evaluator: EvalScope): String = {

    // Super-fast path: all specs are simple %(key)s with an object value.
    // Avoids per-spec pattern matching, widenRaw, and uses offset-based literal appends.
    if (parsed.allSimpleNamedString && values0.isInstanceOf[Val.Obj]) {
      return formatSimpleNamedString(parsed, values0.asInstanceOf[Val.Obj], pos)
    }

    val values = values0 match {
      case x: Val.Arr => x
      case x: Val.Obj => x
      case x          => Val.Arr(pos, Array[Eval](x))
    }
    val valuesArr = values match {
      case x: Val.Arr => x
      case _          => null
    }
    val valuesObj = values match {
      case x: Val.Obj => x
      case _          => null
    }
    val labels = parsed.labels
    val specBits = parsed.specBits
    val numSpecs = specBits.length
    if (numSpecs == 0) {
      if (valuesArr != null && valuesArr.length > 0) {
        Error.fail("Too many values to format: %d, expected %d".format(valuesArr.length, 0))
      }
      val source = parsed.source
      return if (source != null) {
        if (parsed.leadingStart == 0 && parsed.leadingEnd == source.length) source
        else source.substring(parsed.leadingStart, parsed.leadingEnd)
      } else parsed.leading
    }

    val formattedValues = new Array[String](numSpecs)
    var i = 0
    var idx = 0
    while (idx < numSpecs) {
      val rawFormatted = FormatSpec.fromBits(specBits(idx))
      var formatted = rawFormatted
      val cooked0 = formatted.conversion match {
        case '%' => widenRaw(formatted, "%")
        case _   =>
          if (valuesArr != null && i >= valuesArr.length) {
            Error.fail(
              "Too few values to format: %d, expected at least %d".format(
                valuesArr.length,
                i + 1
              )
            )
          }
          val key = if (labels == null) null else labels(idx)
          val raw =
            if (key == null) {
              if (valuesArr == null) Error.fail("Invalid format values")
              // Fast path: skip star checks when format has no * specifiers
              if (!parsed.hasAnyStar) valuesArr.value(i)
              else
                (formatted.widthStar, formatted.precisionStar) match {
                  case (false, false) => valuesArr.value(i)
                  case (true, false)  =>
                    val width = valuesArr.value(i)
                    if (!width.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a width"
                          .format(
                            idx
                          )
                      )
                    }
                    i += 1
                    formatted = formatted.withStarWidth(width.asInt)
                    valuesArr.value(i)
                  case (false, true) =>
                    val precision = valuesArr.value(i)
                    if (!precision.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a precision"
                          .format(idx)
                      )
                    }
                    i += 1
                    formatted = formatted.withStarPrecision(precision.asInt)
                    valuesArr.value(i)
                  case (true, true) =>
                    val width = valuesArr.value(i)
                    if (!width.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a width"
                          .format(
                            idx
                          )
                      )
                    }
                    i += 1
                    val precision = valuesArr.value(i)
                    if (!precision.isInstanceOf[Val.Num]) {
                      Error.fail(
                        "A * was specified at position %d. An integer is expected for a precision"
                          .format(idx)
                      )
                    }
                    i += 1
                    formatted = formatted.withStarValues(width.asInt, precision.asInt)
                    valuesArr.value(i)
                }
            } else {
              if (valuesArr != null) valuesArr.value(i)
              else if (valuesObj != null) valuesObj.value(key, pos)
              else Error.fail("Invalid format values")
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

    if (valuesArr != null && i < valuesArr.length) {
      Error.fail(
        "Too many values to format: %d, expected %d".format(valuesArr.length, i)
      )
    }

    var totalLen = parsed.staticChars
    idx = 0
    while (idx < numSpecs) {
      totalLen += formattedValues(idx).length
      idx += 1
    }

    val chars = new Array[Char](totalLen)
    var cPos = 0
    val source = parsed.source
    if (source != null) {
      val leadLen = parsed.leadingEnd - parsed.leadingStart
      if (leadLen > 0) {
        source.getChars(parsed.leadingStart, parsed.leadingEnd, chars, cPos)
        cPos += leadLen
      }
    } else {
      val leading = parsed.leading
      val leadLen = leading.length
      if (leadLen > 0) {
        leading.getChars(0, leadLen, chars, cPos)
        cPos += leadLen
      }
    }
    idx = 0
    while (idx < numSpecs) {
      val fv = formattedValues(idx)
      val fvLen = fv.length
      if (fvLen > 0) {
        fv.getChars(0, fvLen, chars, cPos)
        cPos += fvLen
      }
      if (source != null) {
        val litStart = parsed.literalStarts(idx)
        val litEnd = parsed.literalEnds(idx)
        val litLen = litEnd - litStart
        if (litLen > 0) {
          source.getChars(litStart, litEnd, chars, cPos)
          cPos += litLen
        }
      } else {
        val lit = parsed.literals(idx)
        val litLen = lit.length
        if (litLen > 0) {
          lit.getChars(0, litLen, chars, cPos)
          cPos += litLen
        }
      }
      idx += 1
    }
    new String(chars)
  }

  /**
   * Super-fast path for format strings where ALL specs are simple `%(key)s` with a `Val.Obj`. This
   * avoids per-spec pattern matching, widenRaw overhead, and caches repeated key lookups. For the
   * large_string_template benchmark (605KB, 256 `%(x)s` interpolations), this eliminates 256
   * redundant object lookups and the generic dispatch overhead.
   */
  private def formatSimpleNamedString(parsed: RuntimeFormat, obj: Val.Obj, pos: Position)(implicit
      evaluator: EvalScope): String = {
    val output = new java.lang.StringBuilder(parsed.staticChars + parsed.specBits.length * 16)

    // Append leading literal using offsets if source is available, else use string
    appendLeading(output, parsed)

    val singleLabel = parsed.singleNamedLabel
    if (singleLabel != null) {
      val str = simpleStringValue(obj.value(singleLabel, pos)(evaluator).value)
      var idx = 0
      while (idx < parsed.specBits.length) {
        output.append(str)
        appendLiteral(output, parsed, idx)
        idx += 1
      }
      return output.toString
    }

    // Cache for repeated key lookups: most format strings reuse the same key many times
    var cachedKey: String = null
    var cachedStr: String = null

    var idx = 0
    while (idx < parsed.specBits.length) {
      val key = parsed.labels(idx)

      // Look up and cache the string value for this key
      // String.equals already does identity check (eq) internally
      val str =
        if (key == cachedKey) cachedStr
        else {
          val rawVal = obj.value(key, pos)(evaluator).value
          val s = simpleStringValue(rawVal)
          cachedKey = key
          cachedStr = s
          s
        }

      output.append(str)

      // Append trailing literal using offsets if source is available
      appendLiteral(output, parsed, idx)

      idx += 1
    }
    output.toString
  }

  private def simpleStringValue(rawVal: Val)(implicit evaluator: EvalScope): String =
    rawVal match {
      case vs: Val.Str => vs.str
      case vn: Val.Num =>
        if (vn.asDouble.toLong.toDouble == vn.asDouble) vn.asDouble.toLong.toString
        else vn.asDouble.toString
      case _: Val.True  => "true"
      case _: Val.False => "false"
      case _: Val.Null  => "null"
      case f: Val.Func  => Error.fail("Cannot format function value", f)
      case other        =>
        // Complex types: materialize via Renderer
        val value = other match {
          case r: Val.Arr => Materializer.apply0(r, new Renderer(indent = -1))
          case r: Val.Obj => Materializer.apply0(r, new Renderer(indent = -1))
          case _          => Materializer(other)
        }
        value.toString
    }

  private def formatInteger(formatted: FormatSpec, s: Double): String = {
    // Fast path: if the value fits in a Long (and isn't Long.MinValue where
    // negation overflows), avoid BigInt allocation entirely
    val sl = s.toLong
    if (sl.toDouble == s && sl != Long.MinValue) {
      val negative = sl < 0
      val lhs = if (negative) "-" else ""
      val rhs = java.lang.Long.toString(if (negative) -sl else sl)
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
      widen(formatted, lhs, "", rhs2, numeric = true, signedConversion = !negative)
    } else {
      val i = BigDecimal(s).toBigInt
      val negative = i.signum < 0
      val lhs = if (negative) "-" else ""
      val rhs = i.abs.toString(10)
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
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
          formatted.precisionOr(6),
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
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
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
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
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
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
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
      val rhs2 = precisionPad(lhs, rhs, formatted.precisionValue)
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

  private def precisionPad(lhs: String, rhs: String, precision: Int): String = {
    if (precision == FormatSpec.NoNumber) rhs
    else {
      val shortage = precision - rhs.length
      if (shortage > 0) Platform.repeatString("0", shortage) + rhs else rhs
    }
  }

  private def formatGeneric(formatted: FormatSpec, s: Double): String = {
    val precision = formatted.precisionOr(6)
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
          formatted.precisionOr(6),
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
