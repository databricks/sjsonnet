package sjsonnet

/**
 * Minimal re-implementation of java.text.DecimalFormat, for Scala.js compatibility (the existing
 * shim for java.text scala-java-locales isn't fully compliant, and adds tons of unnecessary code
 * that bloats the JS bundle)
 */
object DecimalFormat {

  private def leftPad(n: Long, targetWidth: Int): String = {
    val sign = if (n < 0) "-" else ""
    val absN = math.abs(n)
    val nWidth = if (absN == 0) 1 else Math.log10(absN.toDouble).toInt + 1
    sign + Platform.repeatString("0", targetWidth - nWidth) + absN
  }

  def format(
      zeroes: Int,
      hashes: Int,
      alternate: Boolean,
      expLengthOpt: Option[Int],
      number: Double): String = {
    expLengthOpt match {
      case Some(expLength) =>
        val expNum =
          if (number == 0.0) 0L else Math.floor(Math.log10(math.abs(number))).toLong
        val precision = zeroes + hashes
        // Scale so mantissa * 10^precision becomes a roundable integer
        val divided = number / Math.pow(10, (expNum - precision).toDouble)
        val rounded = Math.round(divided)
        val tenPowPrec = Math.pow(10, precision).toLong
        val intPart = rounded / tenPowPrec
        val fracNum = math.abs(rounded % tenPowPrec)
        val prefix = intPart.toString
        val expSign = if (expNum >= 0) "+" else ""
        val expFrag = expSign + leftPad(expNum, expLength)
        // Left-pad fractional digits with zeros to exact precision width
        val fracDigits = leftPad(fracNum, precision)

        (precision, alternate) match {
          case (0, false) => prefix + "E" + expFrag
          case (0, true)  => prefix + ".E" + expFrag
          case (_, _)     =>
            // Strip trailing zeros only for '#' (hash) positions, not '0' positions
            val stripped =
              if (hashes == 0) fracDigits
              else {
                var end = fracDigits.length
                var hashesLeft = hashes
                while (end > 0 && hashesLeft > 0 && fracDigits.charAt(end - 1) == '0') {
                  end -= 1
                  hashesLeft -= 1
                }
                fracDigits.substring(0, end)
              }
            if (stripped.isEmpty) prefix + "E" + expFrag
            else prefix + "." + stripped + "E" + expFrag
        }

      case None =>
        val precision = zeroes + hashes
        val denominator = math.pow(10, precision)
        val numerator = number * denominator + 0.5
        if (numerator.isInfinite)
          throw new sjsonnet.Error("overflow")
        val whole = math.floor(numerator / denominator)
        var fracNum = (math.floor(numerator) % denominator).toLong
        val prefix = whole.toLong.toString

        val frac =
          if (fracNum == 0 && zeroes == 0) ""
          else {
            var n = 0
            while (n < hashes && fracNum % 10 == 0 && fracNum != 0) {
              fracNum /= 10
              n += 1
            }
            leftPad(fracNum, precision - n)
          }

        (precision, alternate) match {
          case (0, false) => prefix
          case (0, true)  => prefix + "."
          case (_, _)     => if (frac.isEmpty) prefix else prefix + "." + frac
        }
    }
  }
}
