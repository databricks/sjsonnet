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
    widen(formatted, "", "", txt, numeric = false, signedConversion = false)
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

    if (missingWidth <= 0) lhs2 + mhs + rhs
    else if (formatted.zeroPadded) {
      if (numeric) lhs2 + mhs + "0" * missingWidth + rhs
      else {
        if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
        else " " * missingWidth + lhs2 + mhs + rhs
      }
    } else if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
    else " " * missingWidth + lhs2 + mhs + rhs
  }

  def format(s: String, values0: Val, pos: Position)(implicit evaluator: EvalScope): String = {
    val (leading, chunks) = fastparse.parse(s, format(_)).get.value
    format(leading, chunks, values0, pos)
  }

  def format(leading: String, chunks: scala.Seq[(FormatSpec, String)], values0: Val, pos: Position)(
      implicit evaluator: EvalScope): String = {
    val values = values0 match {
      case x: Val.Arr => x
      case x: Val.Obj => x
      case x          => Val.Arr(pos, Array[Lazy](x))
    }
    val output = new StringBuilder
    output.append(leading)
    var i = 0
    for (((rawFormatted, literal), idx) <- chunks.zipWithIndex) {
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
              (formatted.widthStar, formatted.precisionStar) match {
                case (false, false) => values.cast[Val.Arr].force(i)
                case (true, false)  =>
                  val width = values.cast[Val.Arr].force(i)
                  if (!width.isInstanceOf[Val.Num]) {
                    Error.fail(
                      "A * was specified at position %d. An integer is expected for a width".format(
                        idx
                      )
                    )
                  }
                  i += 1
                  formatted = formatted.updateWithStarValues(Some(width.asInt), None)
                  values.cast[Val.Arr].force(i)
                case (false, true) =>
                  val precision = values.cast[Val.Arr].force(i)
                  if (!precision.isInstanceOf[Val.Num]) {
                    Error.fail(
                      "A * was specified at position %d. An integer is expected for a precision"
                        .format(idx)
                    )
                  }
                  i += 1
                  formatted = formatted.updateWithStarValues(None, Some(precision.asInt))
                  values.cast[Val.Arr].force(i)
                case (true, true) =>
                  val width = values.cast[Val.Arr].force(i)
                  if (!width.isInstanceOf[Val.Num]) {
                    Error.fail(
                      "A * was specified at position %d. An integer is expected for a width".format(
                        idx
                      )
                    )
                  }
                  i += 1
                  val precision = values.cast[Val.Arr].force(i)
                  if (!precision.isInstanceOf[Val.Num]) {
                    Error.fail(
                      "A * was specified at position %d. An integer is expected for a precision"
                        .format(idx)
                    )
                  }
                  i += 1
                  formatted =
                    formatted.updateWithStarValues(Some(width.asInt), Some(precision.asInt))
                  values.cast[Val.Arr].force(i)
              }
            case Some(key) =>
              values match {
                case v: Val.Arr => v.force(i)
                case v: Val.Obj => v.value(key, pos)
                case _          => Error.fail("Invalid format values")
              }
          }
          val value = raw.force match {
            case f: Val.Func => Error.fail("Cannot format function value", f)
            case r: Val.Arr  => Materializer.apply0(r, new Renderer(indent = -1))
            case r: Val.Obj  => Materializer.apply0(r, new Renderer(indent = -1))
            case raw         => Materializer(raw)
          }
          val formattedValue = value match {
            case ujson.Str(s) =>
              if (formatted.conversion != 's' && formatted.conversion != 'c')
                Error.fail("Format required a number at %d, got string".format(i))
              widenRaw(formatted, s)
            case ujson.Num(s) =>
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
                  Error.fail("Format required a %s at %d, got string".format(raw.prettyName, i))
              }
            case ujson.Bool(s) =>
              formatted.conversion match {
                case 'd' | 'i' | 'u' => formatInteger(formatted, s.compareTo(false))
                case 'o'             => formatOctal(formatted, s.compareTo(false))
                case 'x'             => formatHexadecimal(formatted, s.compareTo(false))
                case 'X'             => formatHexadecimal(formatted, s.compareTo(false)).toUpperCase
                case 'e'             => formatExponent(formatted, s.compareTo(false)).toLowerCase
                case 'E'             => formatExponent(formatted, s.compareTo(false))
                case 'f' | 'F'       => formatFloat(formatted, s.compareTo(false))
                case 'g'             => formatGeneric(formatted, s.compareTo(false)).toLowerCase
                case 'G'             => formatGeneric(formatted, s.compareTo(false))
                case 'c' => widenRaw(formatted, Character.forDigit(s.compareTo(false), 10).toString)
                case 's' => widenRaw(formatted, s.toString)
                case _   =>
                  Error.fail("Format required a %s at %d, got string".format(raw.prettyName, i))
              }
            case v => widenRaw(formatted, v.toString)
          }
          i += 1
          formattedValue
      }
      output.append(cooked0)
      output.append(literal)
    }

    if (values.isInstanceOf[Val.Arr] && i < values.cast[Val.Arr].length) {
      Error.fail(
        "Too many values to format: %d, expected %d".format(values.cast[Val.Arr].length, i)
      )
    }
    output.toString()
  }

  // Truncate a double toward zero, returning the integer part as a BigInt.
  // Uses Long as a fast path (mirrors RenderUtils.renderDouble); falls back to
  // BigDecimal for values that exceed Long range (~9.2e18).
  private def truncateToInteger(s: Double): BigInt = {
    val sl = s.toLong
    if (sl.toDouble == s) BigInt(sl)
    else BigDecimal(s).toBigInt
  }

  private def formatInteger(formatted: FormatSpec, s: Double): String = {
    val i = truncateToInteger(s)
    val negative = i.signum < 0
    val lhs = if (negative) "-" else ""
    val rhs = i.abs.toString(10)
    val rhs2 = precisionPad(lhs, rhs, formatted.precision)
    widen(
      formatted,
      lhs,
      "",
      rhs2,
      numeric = true,
      signedConversion = !negative
    )
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
    val i = truncateToInteger(s)
    val negative = i.signum < 0
    val lhs = if (negative) "-" else ""
    val rhs = i.abs.toString(8)
    val rhs2 = precisionPad(lhs, rhs, formatted.precision)
    widen(
      formatted,
      lhs,
      if (!formatted.alternate || rhs2(0) == '0') "" else "0",
      rhs2,
      numeric = true,
      signedConversion = !negative
    )
  }

  private def formatHexadecimal(formatted: FormatSpec, s: Double): String = {
    val i = truncateToInteger(s)
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

  class PartialApplyFmt(fmt: String) extends Val.Builtin1("format", "values") {
    val (leading, chunks) = fastparse.parse(fmt, format(_)).get.value
    def evalRhs(values0: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, format(leading, chunks, values0.force, pos)(ev))
  }
}
