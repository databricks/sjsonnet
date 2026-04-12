package sjsonnet.stdlib

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

/**
 * Scala Native base64 encoder/decoder using platform SIMD via C FFI.
 *
 * AArch64: NEON — vld3/vst4 interleaved load/store + vqtbl4q 64-byte lookup
 * x86_64:  SSSE3/AVX2/AVX-512 — pshufb/vpshufb/vpermi2b
 * Other:   Scalar fallback with loop unrolling
 *
 * RFC 4648 compliant: A-Za-z0-9+/ alphabet with '=' padding.
 *
 * Performance: reusable buffers eliminate per-call allocations (safe because
 * Scala Native is single-threaded with nativeMultithreading = None).
 */
object FastBase64 {

  @extern
  private object Native {
    def sjsonnet_base64_encode(
        input: Ptr[Byte],
        inputLen: CSize,
        output: Ptr[Byte]
    ): CSize = extern

    def sjsonnet_base64_decode_validated(
        input: Ptr[Byte],
        inputLen: CSize,
        output: Ptr[Byte],
        errorInfo: Ptr[CInt]
    ): CLong = extern
  }

  // Reusable buffers — safe because Scala Native is single-threaded.
  // Grow-only: once sized for the largest input, never re-allocated.
  private var _buf1: Array[Byte] = new Array[Byte](4096)
  private var _buf2: Array[Byte] = new Array[Byte](4096)
  private var _charBuf: Array[Char] = new Array[Char](4096)

  @inline private def ensureBuf1(size: Int): Array[Byte] = {
    var b = _buf1
    if (b.length < size) { b = new Array[Byte](size); _buf1 = b }
    b
  }
  @inline private def ensureBuf2(size: Int): Array[Byte] = {
    var b = _buf2
    if (b.length < size) { b = new Array[Byte](size); _buf2 = b }
    b
  }
  @inline private def ensureCharBuf(size: Int): Array[Char] = {
    var c = _charBuf
    if (c.length < size) { c = new Array[Char](size); _charBuf = c }
    c
  }

  /**
   * Encode a String to base64.
   * ASCII fast-path avoids UTF-8 encoding overhead for the common case.
   */
  def encodeString(s: String): String = {
    val len = s.length
    if (len == 0) return ""

    // Fast path: pure ASCII — cast chars to bytes directly (no UTF-8 encoding)
    val buf = ensureBuf1(len)
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c >= 0x80) {
        // Slow path: non-ASCII, need UTF-8 encoding
        val utf8 = encodeStringUtf8(s)
        return encodeBytesReusable(utf8, utf8.length)
      }
      buf(i) = c.toByte
      i += 1
    }
    encodeBytesReusable(buf, len)
  }

  /**
   * Encode a byte array to base64 string.
   */
  def encodeBytes(bytes: Array[Byte]): String = {
    val len = bytes.length
    if (len == 0) return ""
    encodeBytesReusable(bytes, len)
  }

  /**
   * Internal encode using reusable output buffer + char array for String construction.
   * Only 1 allocation per call: the final String (which copies the char array internally).
   */
  private def encodeBytesReusable(bytes: Array[Byte], len: Int): String = {
    val outLen = 4 * ((len + 2) / 3)
    val outBuf = ensureBuf2(outLen + 16) // +16 for SIMD store overshoot

    Native.sjsonnet_base64_encode(bytes.atUnsafe(0), len.toCSize, outBuf.atUnsafe(0))

    // Direct char array construction — avoids charset lookup overhead
    val chars = ensureCharBuf(outLen)
    var i = 0
    while (i < outLen) {
      chars(i) = (outBuf(i) & 0xff).toChar
      i += 1
    }
    new String(chars, 0, outLen)
  }

  /**
   * Decode a base64 string to a UTF-8 string.
   * ASCII fast-path avoids charset lookup for the common case.
   */
  def decodeToString(s: String): String = {
    val len = s.length
    if (len == 0) return ""

    // Convert String chars to bytes in reusable buffer
    val inBuf = ensureBuf1(len)
    var ci = 0
    while (ci < len) {
      val c = s.charAt(ci)
      inBuf(ci) = (if (c <= 0xff) c else 0x3f).toByte
      ci += 1
    }

    // Single-pass validated decode in C
    val maxOutLen = ((len.toLong * 3) / 4 + 16).toInt
    val outBuf = ensureBuf2(maxOutLen)

    val errorInfo = stackalloc[CInt]()
    val result = Native.sjsonnet_base64_decode_validated(
      inBuf.atUnsafe(0),
      len.toCSize,
      outBuf.atUnsafe(0),
      errorInfo
    )

    if (result == -1L) {
      throw new IllegalArgumentException(
        "Illegal base64 character " + Integer.toHexString(!errorInfo & 0xff)
      )
    } else if (result == -2L) {
      throw new IllegalArgumentException(
        "Last unit does not have enough valid bits"
      )
    }

    val outLen = result.toInt
    if (outLen == 0) return ""

    // Fast ASCII String construction — avoid charset lookup
    val chars = ensureCharBuf(outLen)
    var i = 0
    var allAscii = true
    while (i < outLen) {
      val b = outBuf(i)
      if (b < 0) { allAscii = false; i = outLen } // break: high bit set
      else { chars(i) = b.toChar; i += 1 }
    }

    if (allAscii) new String(chars, 0, outLen)
    else {
      // Slow path: UTF-8 decode via charset
      val tmp = new Array[Byte](outLen)
      System.arraycopy(outBuf, 0, tmp, 0, outLen)
      new String(tmp, "UTF-8")
    }
  }

  /**
   * Decode a base64 string to a byte array.
   * Uses C-side validated decode for single-pass processing.
   */
  def decodeToBytes(s: String): Array[Byte] = {
    val len = s.length
    if (len == 0) return Array.emptyByteArray

    // Convert String chars to bytes in reusable buffer
    val inBuf = ensureBuf1(len)
    var ci = 0
    while (ci < len) {
      val c = s.charAt(ci)
      inBuf(ci) = (if (c <= 0xff) c else 0x3f).toByte
      ci += 1
    }

    // Single-pass validated decode in C
    val maxOutLen = ((len.toLong * 3) / 4 + 16).toInt
    val outBuf = ensureBuf2(maxOutLen)

    val errorInfo = stackalloc[CInt]()
    val result = Native.sjsonnet_base64_decode_validated(
      inBuf.atUnsafe(0),
      len.toCSize,
      outBuf.atUnsafe(0),
      errorInfo
    )

    if (result == -1L) {
      throw new IllegalArgumentException(
        "Illegal base64 character " + Integer.toHexString(!errorInfo & 0xff)
      )
    } else if (result == -2L) {
      throw new IllegalArgumentException(
        "Last unit does not have enough valid bits"
      )
    }

    val outLen = result.toInt
    if (outLen == 0) return Array.emptyByteArray

    // Copy to exact-size result (caller takes ownership)
    val out = new Array[Byte](outLen)
    System.arraycopy(outBuf, 0, out, 0, outLen)
    out
  }

  /**
   * Inline UTF-8 encoding for non-ASCII strings.
   * Only called on the slow path when encodeString detects non-ASCII chars.
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
