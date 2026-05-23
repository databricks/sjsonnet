package sjsonnet.stdlib

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.string.memcpy

/**
 * Scala Native implementation of base64 encode/decode.
 *
 * Uses the aklomp/base64 C library (BSD-2-Clause) which provides SIMD-accelerated base64 via
 * runtime CPU detection:
 *   - x86_64: SSSE3 / SSE4.1 / SSE4.2 / AVX / AVX2 / AVX-512
 *   - AArch64: NEON
 *   - Fallback: optimized generic C implementation
 *
 * The static library is built by CMake and linked via nativeLinkingOptions.
 *
 * Both aklomp/base64 and C++ jsonnet (the reference implementation) use strict RFC 4648 mode:
 * padding is required, unpadded input is rejected. This differs from java.util.Base64 on JVM which
 * is more lenient (accepts unpadded input) — that JVM leniency is a pre-existing sjsonnet bug, not
 * something we replicate here.
 */
@extern
private[stdlib] object libbase64 {
  def base64_encode(
      src: Ptr[CChar],
      srclen: CSize,
      out: Ptr[CChar],
      outlen: Ptr[CSize],
      flags: CInt
  ): Unit = extern

  def base64_decode(
      src: Ptr[CChar],
      srclen: CSize,
      out: Ptr[CChar],
      outlen: Ptr[CSize],
      flags: CInt
  ): CInt = extern
}

object PlatformBase64 {

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
    t
  }

  /**
   * Diagnose why base64 decode failed and throw a JVM-compatible error message. Only called on the
   * error path (after aklomp/base64 returns failure), so zero overhead on the hot path.
   *
   * Error messages match java.util.Base64.Decoder behavior for golden test compatibility:
   *   - Invalid character: "Illegal base64 character XX" (hex)
   *   - Wrong length/padding: "Last unit does not have enough valid bits"
   */
  private def throwDecodeError(srcBytes: Array[Byte]): Nothing = {
    val len = srcBytes.length

    var i = 0
    while (i < len) {
      val b = srcBytes(i) & 0xff
      if (b != '='.toInt) {
        if (DECODE_TABLE(b) < 0) {
          throw new IllegalArgumentException(
            "Illegal base64 character " + Integer.toHexString(b)
          )
        }
      }
      i += 1
    }

    throw new IllegalArgumentException(
      "Last unit does not have enough valid bits"
    )
  }

  def encodeToString(input: Array[Byte]): String = {
    if (input.length == 0) return ""
    val maxOutLen = ((input.length.toLong + 2) / 3) * 4
    if (maxOutLen > Int.MaxValue)
      throw new IllegalArgumentException("Input too large for base64 encoding")
    val outSize = maxOutLen.toInt
    Zone.acquire { implicit z =>
      val srcPtr = alloc[Byte](input.length.toUSize)
      memcpy(srcPtr, input.at(0), input.length.toUSize)
      val outPtr = alloc[Byte]((outSize + 1).toUSize)
      val outLenPtr = alloc[CSize](1.toUSize)
      libbase64.base64_encode(srcPtr, input.length.toUSize, outPtr, outLenPtr, 0)
      val actualLen = (!outLenPtr).toInt
      val result = new Array[Byte](actualLen)
      memcpy(result.at(0), outPtr, actualLen.toUSize)
      new String(result, "US-ASCII")
    }
  }

  /**
   * Encode a `String` directly to base64 without materialising an intermediate `Array[Byte]` for
   * the input side. On Scala Native, `String.getBytes(UTF_8)` for an ASCII-only input still has to
   * walk every char checking for non-ASCII codepoints and then allocate a `Array[Byte]` of equal
   * length; for a 3.5 KB Lorem-ipsum-style input that's two full passes over the data before the
   * SIMD encoder even sees it.
   *
   * The `asciiSafe` flag is a hot-path contract: when `true` the caller (e.g. `std.base64` for a
   * [[sjsonnet.Val.AsciiSafeStr]] input) has already proven every char is 0x00–0x7F. We then write
   * the input straight into the zone-allocated source buffer with a single tight `char.toByte`
   * loop, skipping both the UTF-8 codec and the heap `Array[Byte]`. When `false`, we keep the
   * original `getBytes(UTF_8)` slow path for correctness on non-ASCII strings.
   */
  def encodeStringToString(input: String, asciiSafe: Boolean): String = {
    if (input.isEmpty) return ""
    if (!asciiSafe) return encodeToString(input.getBytes(java.nio.charset.StandardCharsets.UTF_8))

    val len = input.length
    val maxOutLen = ((len.toLong + 2) / 3) * 4
    if (maxOutLen > Int.MaxValue)
      throw new IllegalArgumentException("Input too large for base64 encoding")
    val outSize = maxOutLen.toInt
    Zone.acquire { implicit z =>
      val srcPtr = alloc[Byte](len.toUSize)
      // Narrow ASCII chars directly into the zone buffer. The AsciiSafeStr contract guarantees
      // every char fits in 0x20..0x7F (minus quote/backslash) per Parser.constructString +
      // CharSWAR.isAsciiJsonSafe, so the high byte of each Char is zero and `.toByte` is lossless.
      var i = 0
      while (i < len) {
        !(srcPtr + i.toUSize) = input.charAt(i).toByte
        i += 1
      }
      val outPtr = alloc[Byte]((outSize + 1).toUSize)
      val outLenPtr = alloc[CSize](1.toUSize)
      libbase64.base64_encode(srcPtr, len.toUSize, outPtr, outLenPtr, 0)
      val actualLen = (!outLenPtr).toInt
      val result = new Array[Byte](actualLen)
      memcpy(result.at(0), outPtr, actualLen.toUSize)
      new String(result, "US-ASCII")
    }
  }

  def decode(input: String): Array[Byte] = {
    if (input.isEmpty) return Array.emptyByteArray
    val srcBytes = input.getBytes("US-ASCII")
    val maxOutLen = ((srcBytes.length.toLong / 4) * 3) + 3
    if (maxOutLen > Int.MaxValue)
      throw new IllegalArgumentException("Input too large for base64 decoding")
    val outSize = maxOutLen.toInt
    Zone.acquire { implicit z =>
      val srcPtr = alloc[Byte](srcBytes.length.toUSize)
      memcpy(srcPtr, srcBytes.at(0), srcBytes.length.toUSize)
      val outPtr = alloc[Byte]((outSize + 1).toUSize)
      val outLenPtr = alloc[CSize](1.toUSize)
      val ret =
        libbase64.base64_decode(srcPtr, srcBytes.length.toUSize, outPtr, outLenPtr, 0)
      if (ret != 1) {
        throwDecodeError(srcBytes)
      }
      val actualLen = (!outLenPtr).toInt
      val result = new Array[Byte](actualLen)
      memcpy(result.at(0), outPtr, actualLen.toUSize)
      result
    }
  }
}
