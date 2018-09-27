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

  def zeroPad(s: Double, r: String, precision: Option[Int]) = {
    precision match{
      case None => r -> (s > 0)
      case Some(p) =>
        val shortage = p - r.length
        pprint.log(shortage)
        (if (shortage > 0) "0" * shortage + r else r) -> (s > 0)
    }
  }

  def toOctalString(s: Int, alternate: Boolean) = {
    val r = math.abs(s).toOctalString
    (if (s < 0) "-" else "") + (if (!alternate || r(0) == '0') r else "0" + r)
  }
  def toHexString(s: Int, alternate: Boolean) = {
    val r = math.abs(s).toHexString
    (if (s < 0) "-" else "") + (if (!alternate || r(0) == '0') r else "0x" + r)
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
      val (cooked0, numeric, signedConversion) = formatted.conversion match{
        case '%' => ("%", false, false)
        case _ =>
          val value = values(i)
          i += 1
          val (cooked, signedConversion) = value match{
            case Js.Str(s) => s -> false
            case Js.Num(s) =>
              formatted.conversion match{
                case 'd' | 'i' | 'u' => zeroPad(s, s.toInt.toString, formatted.precision)
                case 'o' => zeroPad(s, toOctalString(s.toInt, formatted.alternate), formatted.precision)
                case 'x' => zeroPad(s, toHexString(s.toInt, formatted.alternate), formatted.precision)
                case 'X' => zeroPad(s, toHexString(s.toInt, formatted.alternate), formatted.precision)

                case 'e' => new DecimalFormat("0.#####E0").format(s).toLowerCase() -> false
                case 'E' => new DecimalFormat("0.#####E0").format(s) -> false

                case 'f' | 'F' => s.toString -> false

//                case 'g' =>
//                case 'G' =>

                case 'c' => s.toChar.toString -> false
              }
            case Js.True => "true" -> false
            case Js.False => "false" -> false
            case v => v.toString -> false
          }
          (cooked, value.isInstanceOf[Js.Num], signedConversion)
      }

      val cooked =
        if(signedConversion && formatted.blankBeforePositive) " " + cooked0
        else if(signedConversion && formatted.signCharacter) "+" + cooked0
        else cooked0
      val missingWidth = formatted.width.getOrElse(-1) - cooked.length
      val widened =
        if (missingWidth <= 0) cooked
        else if (formatted.zeroPadded) {
          if (numeric) "0" * missingWidth + cooked
          else {
            if (formatted.leftAdjusted) cooked + " " * missingWidth
            else " " * missingWidth + cooked
          }
        }
        else if (formatted.leftAdjusted) cooked + " " * missingWidth
        else " " * missingWidth + cooked
      output.append(widened)
      output.append(literal)


    }

    output.toString()
  }
}