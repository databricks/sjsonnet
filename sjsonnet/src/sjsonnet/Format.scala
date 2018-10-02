package sjsonnet

import java.io.StringWriter
import java.text.DecimalFormat

import ammonite.ops.Path
import ujson.Js

import scala.collection.mutable.ArrayBuffer

object Format{
  case class FormatSpec(label: Option[String],
                        alternate: Boolean,
                        zeroPadded: Boolean,
                        leftAdjusted: Boolean,
                        blankBeforePositive: Boolean,
                        signCharacter: Boolean,
                        width: Option[Int],
                        precision: Option[Int],
                        conversion: Char)
  import fastparse.all._
  val integer           = P( CharIn('1' to '9') ~ CharsWhileIn('0' to '9', min = 0) | "0" )

  val label = P( ("(" ~ CharsWhile(_ != ')').! ~ ")").? )
  val flags = P( CharsWhileIn("#0- +", min = 0).! )
  val width = P( (integer | "*").!.? )
  val precision = P( ("." ~/ integer.!).? )
  val conversion = P( CharIn("diouxXeEfFgGcrsa%").! )
  val formatSpec = P( label ~ flags ~ width ~ precision ~ CharIn("hlL").? ~ conversion ).map{
    case (label, flags, width, precision, conversion) =>
      FormatSpec(
        label,
        flags.contains('#'),
        flags.contains('0'),
        flags.contains('-'),
        flags.contains(' '),
        flags.contains('+'),
        width.map(_.toInt),
        precision.map(_.toInt),
        conversion.charAt(0)
      )
  }


  val plain = P( CharsWhile(_ != '%', min = 0).! )
  val format = P( plain ~ (("%" ~/ formatSpec) ~ plain).rep ~ End)



  def widenRaw(formatted: FormatSpec, txt: String) = widen(formatted, "", "", txt, false, false)
  def widen(formatted: FormatSpec,
            lhs: String,
            mhs: String,
            rhs: String,
            numeric: Boolean,
            signedConversion: Boolean) = {

    val lhs2 =
      if(signedConversion && formatted.blankBeforePositive) " " + lhs
      else if(signedConversion && formatted.signCharacter) "+" + lhs
      else lhs

    val missingWidth = formatted.width.getOrElse(-1) - lhs2.length - mhs.length - rhs.length

    if (missingWidth <= 0) lhs2 + mhs + rhs
    else if (formatted.zeroPadded) {
      if (numeric) lhs2 + mhs + "0" * missingWidth + rhs
      else {
        if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
        else " " * missingWidth + lhs2 + mhs + rhs
      }
    }
    else if (formatted.leftAdjusted) lhs2 + mhs + rhs + " " * missingWidth
    else " " * missingWidth + lhs2 + mhs + rhs
  }
  def format(s: String,
             values0: Val,
             fileName: Path,
             currentRoot: Path,
             offset: Int,
             extVars: Map[String, ujson.Js],
             wd: Path): String = synchronized{
    val values = values0 match{
      case x: Val.Arr => x
      case x: Val.Obj => x
      case x => Val.Arr(Seq(Lazy(x)))
    }
    val (leading, chunks) = format.parse(s).get.value
    val output = new StringBuilder
    output.append(leading)
    var i = 0
    for((formatted, literal) <- chunks){
      val cooked0 = formatted.conversion match{
        case '%' => widenRaw(formatted, "%")
        case _ =>

          val value = formatted.label match{
            case None => Materializer(values.asInstanceOf[Val.Arr].value(i).force, extVars, wd)
            case Some(key) =>
              values match{
                case v: Val.Arr => Materializer(v.value(i).force, extVars, wd)
                case v: Val.Obj => Materializer(v.value(key, fileName, currentRoot, offset, wd, extVars).force, extVars, wd)
              }
          }
          i += 1
          value match{
            case Js.Str(s) => widenRaw(formatted, s)
            case Js.Num(s) =>
              formatted.conversion match{
                case 'd' | 'i' | 'u' => formatInteger(formatted, s)
                case 'o' => formatOctal(formatted, s)
                case 'x' => formatHexadecimal(formatted, s)
                case 'X' => formatHexadecimal(formatted, s).toUpperCase
                case 'e' => formatExponent(formatted, s).toLowerCase
                case 'E' => formatExponent(formatted, s)
                case 'f' | 'F' => formatFloat(formatted, s)
                case 'g' => formatGeneric(formatted, s).toLowerCase
                case 'G' => formatGeneric(formatted, s)
                case 'c' => widenRaw(formatted, s.toChar.toString)
                case 's' =>
                  if (s.toLong == s) widenRaw(formatted, s.toLong.toString)
                  else widenRaw(formatted, s.toString)
              }
            case Js.True => widenRaw(formatted, "true")
            case Js.False => widenRaw(formatted, "false")
            case v => widenRaw(formatted, v.toString)
          }

      }

      output.append(cooked0)
      output.append(literal)


    }

    output.toString()
  }

  def formatInteger(formatted: FormatSpec, s: Double) = {
    val (lhs, rhs) = (if (s < 0) "-" else "", math.abs(s.toInt).toString)
    val rhs2 = precisionPad(lhs, rhs, formatted.precision)
    widen(
      formatted,
      lhs, "", rhs2,
      true, s > 0
    )
  }

  def formatFloat(formatted: FormatSpec, s: Double) = {
    widenDecimalFormat(
      formatted,
      "0" + decimalPoint(formatted) + "0" * formatted.precision.getOrElse(6),
      s
    )
  }

  def formatOctal(formatted: FormatSpec, s: Double) = {
    val (lhs, rhs) = (if (s < 0) "-" else "", math.abs(s.toInt).toOctalString)
    val rhs2 = precisionPad(lhs, rhs, formatted.precision)
    widen(
      formatted,
      lhs, if (!formatted.alternate || rhs2(0) == '0') "" else "0", rhs2,
      true, s > 0
    )
  }

  def formatHexadecimal(formatted: FormatSpec, s: Double) = {
    val (lhs, rhs) = (if (s < 0) "-" else "", math.abs(s.toInt).toHexString)
    val rhs2 = precisionPad(lhs, rhs, formatted.precision)
    widen(
      formatted,
      lhs, if (!formatted.alternate) "" else "0x", rhs2,
      true, s > 0
    )
  }

  def precisionPad(lhs: String, rhs: String, precision: Option[Int]) = {
    precision match{
      case None => rhs
      case Some(p) =>
        val shortage = p - rhs.length
        if (shortage > 0) "0" * shortage + rhs else rhs
    }
  }

  def widenDecimalFormat(formatted: FormatSpec, template: String, s: Double) = {
    widen(
      formatted,
      if (s < 0) "-" else "", "",
      new DecimalFormat(template).format(math.abs(s)).replace("E", "E+"),
      true, s > 0
    )
  }

  def formatGeneric(formatted: FormatSpec, s: Double) = {
    val precision = formatted.precision.getOrElse(6)
    val leadingPrecision = math.floor(math.log10(s)).toInt + 1
    val trailingPrecision = math.max(0, precision - leadingPrecision)
    val trailingPlaceholders = if (formatted.alternate) "0" else "#"
    if (s < 0.0001 || math.pow(10, formatted.precision.getOrElse(6): Int) < s)
      widenDecimalFormat(
        formatted,
        "0" + decimalPoint(formatted) + trailingPlaceholders * (precision - 1) + "E00",
        s
      )
    else
      widenDecimalFormat(
        formatted,
        "0" * leadingPrecision + decimalPoint(formatted) + trailingPlaceholders * trailingPrecision,
        s
      )
  }

  def formatExponent(formatted: FormatSpec, s: Double) = {
    widenDecimalFormat(
      formatted,
      "0" + decimalPoint(formatted) + "0" * formatted.precision.getOrElse(6) + "E00",
      s
    )
  }

  def decimalPoint(formatted: FormatSpec) = {
    if (formatted.precision.contains(0) && !formatted.alternate) "" else "."
  }
}
