package sjsonnet

import java.io.StringWriter
import java.text.DecimalFormat

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
  val integer           = P( CharIn('1' to '9') ~ CharsWhileIn('0' to '9', min = 0) )

  val label = P( ("(" ~ CharsWhile(_ != ')').! ~ ")").? )
  val flags = P( CharsWhileIn("#0- +", min = 0).! ).log()
  val width = P( (integer | "*").!.? )
  val precision = P( ("." ~ integer.!).? )
  val conversion = P( CharIn("diouxXeEfFgGcrsa%").! )
  val formatSpec = P( label ~ flags ~ width ~ precision ~ CharIn("hlL").? ~ conversion ).map{
    case (label, flags, width, precision, conversion) =>
      pprint.log(flags)
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
  val format = P( plain ~ (("%" ~ formatSpec) ~ plain).rep ~ End)

  def zeroPad(lhs: String, rhs: String, precision: Option[Int]) = {
    precision match{
      case None => rhs
      case Some(p) =>
        val shortage = p - rhs.length
        (if (shortage > 0) ("0" * shortage + rhs) else rhs)
    }
  }

  def toDecimalString(s: Int, alternate: Boolean) = {
    val r = math.abs(s).toString
    (if (s < 0) "-" else "", r)
  }
  def toOctalString(s: Int, alternate: Boolean) = {
    val r = math.abs(s).toOctalString
    (if (s < 0) "-" else "", r)
  }
  def toHexString(s: Int, alternate: Boolean) = {
    val r = math.abs(s).toHexString
    (if (s < 0) "-" else "", r)
  }
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
  def format(s: String, values0: ujson.Js): String = synchronized{
    val values = values0 match{
      case x: ujson.Js.Arr => x.arr
      case x => ArrayBuffer(x)
    }
    val (leading, chunks) = format.parse(s).get.value
    val output = new StringBuilder
    output.append(leading)
    var i = 0
    for((formatted, literal) <- chunks){
      pprint.log(formatted)
      val cooked0 = formatted.conversion match{
        case '%' => widen(formatted, "", "", "%", false, false)
        case _ =>
          val value = values(i)
          i += 1
          value match{
            case Js.Str(s) => widen(formatted, "", "", s, false, false)
            case Js.Num(s) =>
              formatted.conversion match{
                case 'd' | 'i' | 'u' =>
                  val (lhs, rhs) = toDecimalString(s.toInt, formatted.alternate)
                  val rhs2 = zeroPad(lhs, rhs, formatted.precision)
                  widen(
                    formatted,
                    lhs, "", rhs2,
                    true, s > 0
                  )
                case 'o' =>
                  val (lhs, rhs) = toOctalString(s.toInt, formatted.alternate)
                  val rhs2 = zeroPad(lhs, rhs, formatted.precision)
                  widen(
                    formatted,
                    lhs, (if (!formatted.alternate || rhs2(0) == '0') "" else "0"), rhs2,
                    true, s > 0
                  )
                case 'x' =>
                  val (lhs, rhs) = toHexString(s.toInt, formatted.alternate)
                  val rhs2 = zeroPad(lhs, rhs, formatted.precision)
                  widen(
                    formatted,
                    lhs, (if (!formatted.alternate) "" else "0x"), rhs2,
                    true, s > 0
                  )
                case 'X' =>
                  val (lhs, rhs) = toHexString(s.toInt, formatted.alternate)
                  val rhs2 = zeroPad(lhs, rhs, formatted.precision)
                  widen(
                    formatted,
                    lhs, (if (!formatted.alternate) "" else "0x"), rhs2,
                    true, s > 0
                  ).toUpperCase()

                case 'e' => new DecimalFormat("0.#####E0").format(s).toLowerCase() -> false
                case 'E' => new DecimalFormat("0.#####E0").format(s) -> false

                case 'f' | 'F' => s.toString -> false

                case 'c' => widen(formatted, "", "", s.toChar.toString , false, false)
              }
            case Js.True => widen(formatted, "", "", "true", false, false)
            case Js.False => widen(formatted, "", "", "false", false, false)
            case v => widen(formatted, "", "", v.toString, false, false)
          }

      }

      output.append(cooked0)
      output.append(literal)


    }

    output.toString()
  }
}