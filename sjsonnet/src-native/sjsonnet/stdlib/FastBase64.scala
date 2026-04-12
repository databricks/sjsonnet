package sjsonnet.stdlib

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

/**
 * Scala Native base64 encoder/decoder using platform SIMD via C FFI.
 *
 * AArch64: NEON — vld3/vst4 interleaved load/store + vqtbl4q 64-byte lookup
 * x86_64:  SSSE3 — pshufb reshuffle + mulhi/mullo extract + maddubs/madd pack
 * Other:   Scalar fallback with loop unrolling
 *
 * RFC 4648 compliant: A-Za-z0-9+/ alphabet with '=' padding.
 */
object FastBase64 {

  @extern
  private object Native {
    def sjsonnet_base64_encode(
        input: Ptr[Byte],
        inputLen: CSize,
        output: Ptr[Byte]
    ): CSize = extern

    def sjsonnet_base64_decode(
        input: Ptr[Byte],
        inputLen: CSize,
        output: Ptr[Byte]
    ): CLong = extern
  }

  /**
   * Encode a String to base64.
   * Uses inline UTF-8 encoding to avoid getBytes("UTF-8") charset lookup overhead.
   */
  def encodeString(s: String): String = {
    val len = s.length
    if (len == 0) return ""
    val utf8 = encodeStringUtf8(s)
    encodeBytes(utf8)
  }

  /**
   * Encode a byte array to base64 string.
   * Delegates to C SIMD implementation via FFI.
   */
  def encodeBytes(bytes: Array[Byte]): String = {
    val len = bytes.length
    if (len == 0) return ""

    val outLen = 4 * ((len + 2) / 3)
    val outBuf = new Array[Byte](outLen + 16) // +16 for SIMD store overshoot

    val inPtr = bytes.atUnsafe(0)
    val outPtr = outBuf.atUnsafe(0)
    Native.sjsonnet_base64_encode(inPtr, len.toCSize, outPtr)

    // base64 output is pure ASCII — construct String from exact-length slice
    new String(outBuf, 0, outLen, "US-ASCII")
  }

  /**
   * Decode a base64 string to a UTF-8 string.
   */
  def decodeToString(s: String): String = {
    val bytes = decodeToBytes(s)
    new String(bytes, "UTF-8")
  }

  // Decode lookup: ASCII byte -> 6-bit value, -1 invalid, -2 padding '='
  private val DECODE_TABLE: Array[Int] = {
    val t = Array.fill[Int](256)(-1)
    var i = 0
    while (i < 26) { t('A' + i) = i; i += 1 }
    i = 0
    while (i < 26) { t('a' + i) = i + 26; i += 1 }
    i = 0
    while (i < 10) { t('0' + i) = i + 52; i += 1 }
    t('+') = 62
    t('/') = 63
    t('=') = -2
    t
  }

  /**
   * Decode a base64 string to a byte array.
   * Validates in Scala (for exact error messages), then delegates to C SIMD.
   */
  def decodeToBytes(s: String): Array[Byte] = {
    val len = s.length
    if (len == 0) return Array.emptyByteArray

    val table = DECODE_TABLE

    // Single pass: validate chars, convert to bytes, count valid/padding
    val inBuf = new Array[Byte](len)
    var validCount = 0
    var paddingCount = 0
    var ci = 0
    while (ci < len) {
      val c = s.charAt(ci)
      val b = if (c <= 0xff) c.toInt else 0x3f
      inBuf(ci) = b.toByte
      val v = table(b)
      if (v >= 0) validCount += 1
      else if (v == -2) paddingCount += 1
      else {
        throw new IllegalArgumentException(
          "Illegal base64 character " + Integer.toHexString(b)
        )
      }
      ci += 1
    }

    val totalChars = validCount + paddingCount
    if (totalChars % 4 == 1) {
      throw new IllegalArgumentException(
        "Last unit does not have enough valid bits"
      )
    }

    // Calculate expected output length
    val fullGroups = validCount / 4
    val leftover = validCount % 4
    val outLen =
      fullGroups * 3 + (if (leftover == 3) 2 else if (leftover == 2) 1 else 0)
    val outBuf = new Array[Byte](outLen + 16) // +16 for SIMD store padding

    if (outLen > 0) {
      val inPtr = inBuf.atUnsafe(0)
      val outPtr = outBuf.atUnsafe(0)
      Native.sjsonnet_base64_decode(inPtr, len.toCSize, outPtr)
    }
    if (outLen == outBuf.length) outBuf
    else {
      val trimmed = new Array[Byte](outLen)
      System.arraycopy(outBuf, 0, trimmed, 0, outLen)
      trimmed
    }
  }

  /**
   * Inline UTF-8 encoding — avoids getBytes("UTF-8") allocation + charset lookup.
   */
  private def encodeStringUtf8(s: String): Array[Byte] = {
    val len = s.length
    val buf = new Array[Byte](len * 3) // worst case for BMP

    var i = 0
    var j = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c < 0x80) {
        buf(j) = c.toByte
        j += 1
      } else if (c < 0x800) {
        buf(j) = (0xc0 | (c >> 6)).toByte
        buf(j + 1) = (0x80 | (c & 0x3f)).toByte
        j += 2
      } else {
        buf(j) = (0xe0 | (c >> 12)).toByte
        buf(j + 1) = (0x80 | ((c >> 6) & 0x3f)).toByte
        buf(j + 2) = (0x80 | (c & 0x3f)).toByte
        j += 3
      }
      i += 1
    }

    if (j == buf.length) buf
    else {
      val result = new Array[Byte](j)
      System.arraycopy(buf, 0, result, 0, j)
      result
    }
  }
}
