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

  private def formatExponentParts(
      precision: Int,
      hashes: Int,
      alternate: Boolean,
      expLength: Int,
      expNum: Long,
      prefix: String,
      fracDigits: String): String = {
    val expSign = if (expNum >= 0) "+" else ""
    val expFrag = expSign + leftPad(expNum, expLength)

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
  }

  def format(
      zeroes: Int,
      hashes: Int,
      alternate: Boolean,
      expLengthOpt: Option[Int],
      number: Double): String = {
    expLengthOpt match {
      case Some(expLength) =>
        var expNum =
          if (number == 0.0) 0L else Math.floor(Math.log10(math.abs(number))).toLong
        val precision = zeroes + hashes
        // Start from the exact binary64 value, shift the decimal point without an intermediate
        // rounding, then perform the one requested round using ties-to-even.
        val tenPowPrec = BigInt(10).pow(precision)
        val exactJava = BigDecimal.exact(number).abs.bigDecimal
        if (exactJava.signum() != 0) {
          while (
            exactJava.compareTo(
              java.math.BigDecimal.ONE.scaleByPowerOfTen(expNum.toInt + 1)
            ) >= 0
          ) expNum += 1
          while (exactJava.compareTo(java.math.BigDecimal.ONE.scaleByPowerOfTen(expNum.toInt)) < 0)
            expNum -= 1
        }
        var roundedMagnitude = BigInt(
          exactJava
            .scaleByPowerOfTen(precision - expNum.toInt)
            .setScale(0, java.math.RoundingMode.HALF_EVEN)
            .toBigInteger
        )
        if (roundedMagnitude >= tenPowPrec * 10) {
          roundedMagnitude /= 10
          expNum += 1
        }
        val rounded = if (number < 0) -roundedMagnitude else roundedMagnitude
        val fracStr = (rounded % tenPowPrec).abs.toString
        val fracDigits = Platform.repeatString("0", precision - fracStr.length) + fracStr
        formatExponentParts(
          precision,
          hashes,
          alternate,
          expLength,
          expNum,
          (rounded / tenPowPrec).toString,
          fracDigits
        )

      case None =>
        val precision = zeroes + hashes
        if (precision == 0) {
          val rounded = math.rint(number)
          val prefix =
            if (number != number) rounded.toLong.toString
            else RenderUtils.truncatedDoubleToString(rounded)
          if (alternate) prefix + "." else prefix
        } else {
          val denominator = BigInt(10).pow(precision)
          val scaled = BigInt(
            BigDecimal
              .exact(number)
              .abs
              .bigDecimal
              .scaleByPowerOfTen(precision)
              .setScale(0, java.math.RoundingMode.HALF_EVEN)
              .toBigInteger
          )
          val whole = scaled / denominator
          val fracMagnitude = scaled % denominator

          val sign = if (number < 0) "-" else ""
          val prefix = sign + whole.toString
          val fracStr = fracMagnitude.toString

          val frac =
            if (fracStr == "0" && zeroes == 0) ""
            else {
              val padded = Platform.repeatString("0", precision - fracStr.length) + fracStr
              if (hashes > 0) {
                var end = padded.length
                var hashesLeft = hashes
                while (end > 0 && hashesLeft > 0 && padded.charAt(end - 1) == '0') {
                  end -= 1
                  hashesLeft -= 1
                }
                padded.substring(0, end)
              } else padded
            }

          if (frac.isEmpty) prefix else prefix + "." + frac
        }
    }
  }
}
