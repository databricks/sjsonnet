package sjsonnet

import scala.scalanative.runtime.{ByteArray, Intrinsics}

/**
 * SWAR (SIMD Within A Register) escape-char scanner for Scala Native.
 *
 * Uses Scala Native's `Intrinsics.loadLong` + `ByteArray.atRawUnsafe` for zero-overhead 8-byte bulk
 * reads directly from Array[Byte] memory, matching the JVM VarHandle SWAR performance.
 *
 * For String scanning, uses `getBytes(UTF-8)` + byte[] SWAR. On Scala Native compact strings are
 * UTF-16, so converting to bytes first is necessary.
 *
 * Inspired by netty's SWARUtil (io.netty.util.SWARUtil) and Hacker's Delight Ch. 6 zero-detection
 * formula.
 */
object CharSWAR {

  // --- 8-bit SWAR constants ---
  private final val HOLE = 0x7f7f7f7f7f7f7f7fL
  private final val QUOTE = 0x2222222222222222L
  private final val BSLAS = 0x5c5c5c5c5c5c5c5cL
  private final val CTRL = 0xe0e0e0e0e0e0e0e0L
  private final val LITTLE_ENDIAN =
    java.nio.ByteOrder.nativeOrder() == java.nio.ByteOrder.LITTLE_ENDIAN
  private final val U16_HOLE = 0x7fff7fff7fff7fffL
  private final val U16_QUOTE = 0x0022002200220022L
  private final val U16_BSLAS = 0x005c005c005c005cL
  private final val U16_CTRL = 0xffe0ffe0ffe0ffe0L
  private final val U16_ASCII = 0xff80ff80ff80ff80L

  /**
   * SWAR: returns a mask for byte lanes in `word` containing '"' (0x22), '\\' (0x5C), or a control
   * char (< 0x20).
   */
  @inline private def swarMatchMask(word: Long): Long = {
    // 1. Detect '"' via XOR + zero-detection
    val q = word ^ QUOTE
    val qz = ~((q & HOLE) + HOLE | q | HOLE)

    // 2. Detect '\\' via XOR + zero-detection
    val b = word ^ BSLAS
    val bz = ~((b & HOLE) + HOLE | b | HOLE)

    // 3. Detect control chars: byte & 0xE0 == 0 → c < 32
    val c = word & CTRL
    val cz = ~((c & HOLE) + HOLE | c | HOLE)

    qz | bz | cz
  }

  @inline private def firstMatchedByte(mask: Long): Int =
    (if (LITTLE_ENDIAN) java.lang.Long.numberOfTrailingZeros(mask)
     else java.lang.Long.numberOfLeadingZeros(mask)) >>> 3

  def hasEscapeChar(s: String): Boolean = {
    val len = s.length
    if (len < 128) {
      hasEscapeCharScalar(s, len)
    } else {
      val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      hasEscapeChar(bytes, 0, bytes.length)
    }
  }

  def hasEscapeChar(arr: Array[Char], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
  }

  def isAsciiJsonSafe(s: String): Boolean = {
    val len = s.length
    if (len < 8) return isAsciiJsonSafeScalar(s, len)

    var i = 0
    val limit = len - 3
    while (i < limit) {
      val word =
        (s.charAt(i).toLong) |
        (s.charAt(i + 1).toLong << 16) |
        (s.charAt(i + 2).toLong << 32) |
        (s.charAt(i + 3).toLong << 48)
      if (swarHasUnsafeAsciiChar(word)) return false
      i += 4
    }
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\' || c >= 128) return false
      i += 1
    }
    true
  }

  /**
   * SWAR scan for byte[] using Intrinsics.loadLong for zero-overhead bulk reads. Processes 8 bytes
   * per iteration — same throughput as the JVM VarHandle path. UTF-8 multi-byte sequences never
   * produce bytes matching '"', '\', or < 0x20.
   */
  def hasEscapeChar(arr: Array[Byte], from: Int, to: Int): Boolean = {
    val len = to - from
    if (len < 8) {
      return hasEscapeCharScalarBytes(arr, from, to)
    }
    val barr = arr.asInstanceOf[ByteArray]
    var i = from
    val limit = to - 7
    while (i < limit) {
      val word = Intrinsics.loadLong(barr.atRawUnsafe(i))
      if (swarMatchMask(word) != 0L) return true
      i += 8
    }
    // Tail: remaining 0-7 bytes
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return true
      i += 1
    }
    false
  }

  def findFirstEscapeChar(arr: Array[Byte], from: Int, to: Int): Int = {
    val len = to - from
    if (len < 8) return findFirstEscapeCharScalar(arr, from, to)
    val barr = arr.asInstanceOf[ByteArray]
    var i = from
    val limit = to - 7
    while (i < limit) {
      val word = Intrinsics.loadLong(barr.atRawUnsafe(i))
      val mask = swarMatchMask(word)
      if (mask != 0L) {
        return i + firstMatchedByte(mask)
      }
      i += 8
    }
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return i
      i += 1
    }
    -1
  }

  @inline private def hasEscapeCharScalar(s: String, len: Int): Boolean = {
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
  }

  @inline private def swarHasUnsafeAsciiChar(word: Long): Boolean = {
    if ((word & U16_ASCII) != 0L) return true
    val qz = zero16(word ^ U16_QUOTE)
    val bz = zero16(word ^ U16_BSLAS)
    val cz = zero16(word & U16_CTRL)
    (qz | bz | cz) != 0L
  }

  @inline private def zero16(word: Long): Long =
    ~((word & U16_HOLE) + U16_HOLE | word | U16_HOLE)

  @inline private def isAsciiJsonSafeScalar(s: String, len: Int): Boolean = {
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\' || c >= 128) return false
      i += 1
    }
    true
  }

  @inline private def hasEscapeCharScalarBytes(arr: Array[Byte], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return true
      i += 1
    }
    false
  }

  @inline private def findFirstEscapeCharScalar(arr: Array[Byte], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return i
      i += 1
    }
    -1
  }
}
