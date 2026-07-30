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
      number: Double): String =
    format(zeroes, hashes, alternate, expLengthOpt, number, useExactDecimal = false)

  private[sjsonnet] def format(
      zeroes: Int,
      hashes: Int,
      alternate: Boolean,
      expLengthOpt: Option[Int],
      number: Double,
      useExactDecimal: Boolean): String = {
    expLengthOpt match {
      case Some(expLength) =>
        var expNum =
          if (number == 0.0) 0L else Math.floor(Math.log10(math.abs(number))).toLong
        val precision = zeroes + hashes
        if (precision > 15) {
          // A Double cannot represent every scaled mantissa once scientific formatting asks for
          // more than 16 significant digits. Math.round first loses low digits and then saturates
          // at Long.MaxValue. Start from the exact binary64 value and keep the scaled integer
          // outside Long.
          val tenPowPrec = BigInt(10).pow(precision)
          val exactMagnitude = BigDecimal.exact(number).abs
          val exactJava = exactMagnitude.bigDecimal
          if (exactJava.signum() != 0) {
            while (
              exactJava.compareTo(
                java.math.BigDecimal.ONE.scaleByPowerOfTen(expNum.toInt + 1)
              ) >= 0
            ) expNum += 1
            while (
              exactJava.compareTo(java.math.BigDecimal.ONE.scaleByPowerOfTen(expNum.toInt)) < 0
            ) expNum -= 1
          }
          val scaleExponent = precision - expNum.toInt
          val scaled =
            if (scaleExponent >= 0) exactMagnitude * BigDecimal(10).pow(scaleExponent)
            else exactMagnitude / BigDecimal(10).pow(-scaleExponent)
          var roundedMagnitude =
            scaled.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
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
        } else {
          // Scale so mantissa * 10^precision becomes a roundable integer
          val divided = number / Math.pow(10, (expNum - precision).toDouble)
          var rounded = Math.round(divided)
          val tenPowPrec = Math.pow(10, precision).toLong
          if (rounded.toDouble >= Math.pow(10, precision + 1)) {
            rounded = Math.round(rounded / 10.0)
            expNum += 1
          }
          val fracNum = math.abs(rounded % tenPowPrec)
          formatExponentParts(
            precision,
            hashes,
            alternate,
            expLength,
            expNum,
            (rounded / tenPowPrec).toString,
            leftPad(fracNum, precision)
          )
        }

      case None =>
        val precision = zeroes + hashes
        if (precision == 0) {
          // Round half away from zero (matching go-jsonnet/jrsonnet behavior)
          val rounded =
            if (number != number || math.abs(number) >= 4503599627370496.0) number
            else if (number >= 0) math.floor(number + 0.5)
            else math.ceil(number - 0.5)
          val prefix =
            if (number != number) rounded.toLong.toString
            else RenderUtils.truncatedDoubleToString(rounded)
          if (alternate) prefix + "." else prefix
        } else {
          val denominator = BigDecimal(10).pow(precision)
          val exactDecimal = useExactDecimal || precision > 15
          val bd =
            if (exactDecimal) BigDecimal.exact(number).abs
            else BigDecimal(number).abs
          val scaled =
            if (exactDecimal)
              (bd * denominator).setScale(0, BigDecimal.RoundingMode.HALF_EVEN)
            else
              (bd * denominator + BigDecimal("0.5")).setScale(0, BigDecimal.RoundingMode.FLOOR)
          val wholeBD = (scaled / denominator).setScale(0, BigDecimal.RoundingMode.FLOOR)
          val fracBD = (scaled - wholeBD * denominator).abs

          val sign = if (number < 0) "-" else ""
          val prefix = sign + wholeBD.toBigInt.toString
          val fracStr = fracBD.toBigInt.toString

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
