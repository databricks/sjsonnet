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
