package sjsonnet

private[sjsonnet] object FastLongRenderer {
  final val MaxLongChars = 20

  private final val LongChunkSize = 100000000L
  // Same 1e8/1e16 reciprocal-multiply constants used by jsoniter-scala's JVM/Native writer.
  // The 1441151881 constant below is only used for jeaiii-style digit extraction inside
  // int-sized chunks; it is not a general replacement for direct Long / 100.
  private final val Div1e8Multiplier = 6189700196426901375L
  private final val Div1e16Multiplier = 8307674973655724206L
  private final val UInt32Mask = 0xffffffffL
  private final val UInt47Mask = 0x7fffffffffffL
  private final val UInt57Mask = 0x1ffffffffffffffL
  private[this] final val LongMinValueString = "-9223372036854775808"
  private[this] val LongMinValueBytes: Array[Byte] = {
    val a = new Array[Byte](LongMinValueString.length)
    var i = 0
    while (i < a.length) {
      a(i) = LongMinValueString.charAt(i).toByte
      i += 1
    }
    a
  }
  private[this] val DigitTens: Array[Byte] = {
    val a = new Array[Byte](100)
    var i = 0
    while (i < 100) {
      a(i) = ('0' + i / 10).toByte
      i += 1
    }
    a
  }
  private[this] val DigitOnes: Array[Byte] = {
    val a = new Array[Byte](100)
    var i = 0
    while (i < 100) {
      a(i) = ('0' + i % 10).toByte
      i += 1
    }
    a
  }

  def writeLong(v: Long, buf: Array[Byte], p: Int): Int = {
    if (v == Long.MinValue) {
      System.arraycopy(LongMinValueBytes, 0, buf, p, LongMinValueBytes.length)
      p + LongMinValueBytes.length
    } else {
      val negative = v < 0
      val abs = if (negative) -v else v
      var pos = p
      if (negative) {
        buf(pos) = '-'.toByte
        pos += 1
      }
      writePositiveLongDigits(abs, buf, pos)
    }
  }

  @inline private def divideBy1e8(x: Long): Long =
    Math.multiplyHigh(x, Div1e8Multiplier) >>> 25

  @inline private def divideBy1e16(x: Long): Long =
    Math.multiplyHigh(x, Div1e16Multiplier) >>> 52

  private def writePositiveLongDigits(abs: Long, buf: Array[Byte], p: Int): Int = {
    if (abs < LongChunkSize) writePositiveIntDigits(abs.toInt, buf, p)
    else {
      val q1 = divideBy1e8(abs)
      val r1 = (abs - q1 * LongChunkSize).toInt
      var pos =
        if (q1 < LongChunkSize) writePositiveIntDigits(q1.toInt, buf, p)
        else {
          val q2 = divideBy1e16(abs)
          val afterHigh = writePositiveIntDigits(q2.toInt, buf, p)
          write8Digits((q1 - q2 * LongChunkSize).toInt, buf, afterHigh)
        }
      pos = write8Digits(r1, buf, pos)
      pos
    }
  }

  private def writeDigitPair(i: Int, buf: Array[Byte], pos: Int): Int = {
    buf(pos) = DigitTens(i)
    buf(pos + 1) = DigitOnes(i)
    pos + 2
  }

  private def writePositiveIntDigits(q0: Int, buf: Array[Byte], p: Int): Int = {
    var pos = p
    if (q0 < 100) {
      if (q0 < 10) {
        buf(pos) = ('0' + q0).toByte
        pos + 1
      } else writeDigitPair(q0, buf, pos)
    } else if (q0 < 10000) {
      val q1 = q0 * 5243 >> 19
      if (q0 < 1000) {
        buf(pos) = ('0' + q1).toByte
        pos = writeDigitPair(q0 - q1 * 100, buf, pos + 1)
      } else {
        pos = writeDigitPair(q1, buf, pos)
        pos = writeDigitPair(q0 - q1 * 100, buf, pos)
      }
      pos
    } else if (q0 < 1000000) {
      val q1 = q0 * 429497L
      val q2 = (q1 & UInt32Mask) * 100L
      val q3 = (q2 & UInt32Mask) * 100L
      val r1 = (q1 >>> 32).toInt
      if (q0 < 100000) {
        buf(pos) = ('0' + r1).toByte
        pos += 1
      } else pos = writeDigitPair(r1, buf, pos)
      pos = writeDigitPair((q2 >>> 32).toInt, buf, pos)
      writeDigitPair((q3 >>> 32).toInt, buf, pos)
    } else if (q0 < 100000000) {
      val q1 = q0 * 140737489L
      val q2 = (q1 & UInt47Mask) * 100L
      val q3 = (q2 & UInt47Mask) * 100L
      val q4 = (q3 & UInt47Mask) * 100L
      val r1 = (q1 >>> 47).toInt
      if (q0 < 10000000) {
        buf(pos) = ('0' + r1).toByte
        pos += 1
      } else pos = writeDigitPair(r1, buf, pos)
      pos = writeDigitPair((q2 >>> 47).toInt, buf, pos)
      pos = writeDigitPair((q3 >>> 47).toInt, buf, pos)
      writeDigitPair((q4 >>> 47).toInt, buf, pos)
    } else {
      val q1 = q0 * 1441151881L
      val q2 = (q1 & UInt57Mask) * 100L
      val q3 = (q2 & UInt57Mask) * 100L
      val q4 = (q3 & UInt57Mask) * 100L
      val q5 = (q4 & UInt57Mask) * 100L
      val r1 = (q1 >>> 57).toInt
      if (q0 < 1000000000) {
        buf(pos) = ('0' + r1).toByte
        pos += 1
      } else pos = writeDigitPair(r1, buf, pos)
      pos = writeDigitPair((q2 >>> 57).toInt, buf, pos)
      pos = writeDigitPair((q3 >>> 57).toInt, buf, pos)
      pos = writeDigitPair((q4 >>> 57).toInt, buf, pos)
      writeDigitPair((q5 >>> 57).toInt, buf, pos)
    }
  }

  private def write8Digits(q0: Int, buf: Array[Byte], p: Int): Int = {
    val q1 = q0 * 140737489L
    val q2 = (q1 & UInt47Mask) * 100L
    val q3 = (q2 & UInt47Mask) * 100L
    val q4 = (q3 & UInt47Mask) * 100L
    var pos = writeDigitPair((q1 >>> 47).toInt, buf, p)
    pos = writeDigitPair((q2 >>> 47).toInt, buf, pos)
    pos = writeDigitPair((q3 >>> 47).toInt, buf, pos)
    writeDigitPair((q4 >>> 47).toInt, buf, pos)
  }
}
