
package sjsonnet

/**
  * Minimal re-implementation of java.text.DecimalFormat, for Scala.js
  * compatibility (the existing shim for java.text scala-java-locales isn't
  * fully compliant, and adds tons of unnecessary code that bloats the JS bundle)
  */
object DecimalFormat {

  def trailingZeroes(n: Long): Int = {
    var count = 0
    var current = n
    var done = false
    while(!done && current > 0){
      if (current % 10 == 0) count += 1
      else done = true
      current /= 10
    }
    count
  }
  def leftPad(n: Long, targetWidth: Int): String = {
    val sign = if (n < 0) "-" else ""
    val absN = math.abs(n)
    val nWidth = if (absN == 0) 1 else Math.log10(absN).toInt + 1
    sign + "0" * (targetWidth - nWidth) + absN
  }
  def rightPad(n0: Long, minWidth: Int, maxWidth: Int): String = {
    if (n0 == 0 && minWidth == 0) ""
    else {
      val n = (n0 / Math.pow(10, trailingZeroes(n0))).toInt
      assert(n == math.abs(n))
      val nWidth = if (n == 0) 1 else Math.log10(n).toInt + 1
      ("" + n + "0" * (minWidth - nWidth)).take(maxWidth)
    }
  }
  def format(fracLengthOpt: Option[(Int, Int)], expLengthOpt: Option[Int], number: Double): String = {
    expLengthOpt match{
      case Some(expLength) =>
        val roundLog10 = Math.ceil(Math.log10(Math.abs(number))).toLong
        val expNum = roundLog10 - 1
        val scaled = number / math.pow(10, expNum)
        val prefix = scaled.toLong.toString
        val expFrag = leftPad(expNum, expLength)
        val fracFrag = fracLengthOpt.map{case (zeroes, hashes) =>
          if (zeroes == 0 && hashes == 0) ""
          else {
            val divided = number / Math.pow(10, expNum - zeroes - hashes)
            val scaledFrac = divided % Math.pow(10, zeroes + hashes)
            rightPad(Math.abs(Math.round(scaledFrac)), zeroes, zeroes + hashes)
          }
        }

        fracFrag match{
          case None  => prefix + "E" + expFrag
          case Some("") => if (fracLengthOpt.contains((0, 0))) prefix + ".E" + expFrag else prefix + "E" + expFrag
          case Some(frac) => prefix + "." + frac + "E" + expFrag
        }

      case None =>
        val prefix = number.toLong.toString
        val fracFrag = fracLengthOpt.map { case (zeroes, hashes) =>
          var fracNum = Math.round(number * math.pow(10, zeroes + hashes)) % math.pow(10, zeroes + hashes).toLong

          if (fracNum == 0 && zeroes == 0) ""
          else {
            var n = 0
            while(n < hashes && fracNum % 10 == 0 && fracNum != 0) {
              fracNum /= 10
              n += 1
            }
            leftPad(fracNum, zeroes + hashes - n)
          }
        }

        fracFrag match{
          case None  => prefix
          case Some("") => if (fracLengthOpt.contains((0, 0))) prefix + "." else prefix
          case Some(frac) => prefix + "." + frac
        }
    }
  }
}
