package sjsonnet

import scala.scalanative.runtime.{CharArray, Intrinsics}
import scala.scalanative.annotation.alwaysinline

/**
 * SWAR (SIMD Within A Register) escape-char scanner for Scala Native.
 *
 * P0 optimizations:
 *   - Eliminate getBytes(UTF-8) allocation in hasEscapeChar(String) by using 16-bit lane SWAR
 *     directly on the char[] backing store.
 *   - The chunked rendering path in BaseByteRenderer already does getBytes once; for the
 *     hasEscapeChar check in BaseCharRenderer we avoid any allocation entirely.
 *
 * P1 optimizations:
 *   - 16-bit lane SWAR (4 chars per Long) for char[] scanning instead of scalar loop.
 *   - Tighter comparison loops with @alwaysinline hints for LLVM auto-vectorization.
 *   - Pre-allocated buffers for compareStrings with bounds-check elimination.
 *
 * Inspired by netty's SWARUtil (io.netty.util.SWARUtil) and Hacker's Delight Ch. 6.
 */
object CharSWAR {

  // =========================================================================
  // 8-bit SWAR constants (for byte[] scanning)
  // =========================================================================
  private final val HOLE_8 = 0x7f7f7f7f7f7f7f7fL
  private final val QUOTE_8 = 0x2222222222222222L
  private final val BSLAS_8 = 0x5c5c5c5c5c5c5c5cL
  private final val CTRL_8 = 0xe0e0e0e0e0e0e0e0L

  @inline private def swarHasMatch8(word: Long): Boolean = {
    val q = word ^ QUOTE_8
    val qz = ~((q & HOLE_8) + HOLE_8 | q | HOLE_8)
    val b = word ^ BSLAS_8
    val bz = ~((b & HOLE_8) + HOLE_8 | b | HOLE_8)
    val c = word & CTRL_8
    val cz = ~((c & HOLE_8) + HOLE_8 | c | HOLE_8)
    (qz | bz | cz) != 0L
  }

  // =========================================================================
  // 16-bit SWAR constants (P1: 4 x 16-bit lanes per Long)
  // =========================================================================
  // Each lane is 16 bits. We detect:
  //   '"'  = 0x0022  -> broadcast = 0x0022002200220022L
  //   '\\' = 0x005C  -> broadcast = 0x005C005C005C005CL
  //   c < 0x20       -> bits 5-15 of each 16-bit lane are zero
  //     mask = 0xFFE0 -> broadcast = 0xFFE0FFE0FFE0FFE0L
  private final val HOLE_16 = 0x7fff7fff7fff7fffL
  private final val QUOTE_16 = 0x0022002200220022L
  private final val BSLAS_16 = 0x005c005c005c005cL
  private final val CTRL_16 = 0xffe0ffe0ffe0ffe0L

  /**
   * 16-bit SWAR: returns true if any 16-bit lane in `word` contains '"' (0x0022), '\\' (0x005C), or
   * a control char (< 0x0020).
   */
  @inline private def swarHasMatch16(word: Long): Boolean = {
    val q = word ^ QUOTE_16
    val qz = ~((q & HOLE_16) + HOLE_16 | q | HOLE_16)
    val b = word ^ BSLAS_16
    val bz = ~((b & HOLE_16) + HOLE_16 | b | HOLE_16)
    val c = word & CTRL_16
    val cz = ~((c & HOLE_16) + HOLE_16 | c | HOLE_16)
    (qz | bz | cz) != 0L
  }

  // =========================================================================
  // hasEscapeChar(String) — P0: avoid getBytes allocation for long strings
  // =========================================================================
  /**
   * Check if a String needs JSON escaping.
   *   - Short strings (< 128 chars): scalar scan, zero allocation.
   *   - Long strings (>= 128 chars): toCharArray + 16-bit SWAR. One allocation but SWAR scans 4x
   *     faster than scalar for long strings, and toCharArray is a simple memcpy (cheaper than
   *     getBytes(UTF-8) encoding).
   */
  private final val SWAR_THRESHOLD = 128

  def hasEscapeChar(s: String): Boolean = {
    val len = s.length
    if (len < SWAR_THRESHOLD) {
      hasEscapeCharScalar(s, len)
    } else {
      hasEscapeCharCharSWAR(s, len)
    }
  }

  /**
   * 16-bit SWAR scan on String via toCharArray. Processes 4 chars per Long iteration. toCharArray
   * is cheaper than getBytes(UTF-8) because it's a raw memcpy.
   */
  private def hasEscapeCharCharSWAR(s: String, len: Int): Boolean = {
    val carr = s.toCharArray
    val cArr = carr.asInstanceOf[CharArray]
    var i = 0
    val limit = len - 3 // 4 chars per loadLong
    while (i < limit) {
      val word = Intrinsics.loadLong(cArr.atRawUnsafe(i))
      if (swarHasMatch16(word)) {
        var j = i
        while (j < i + 4) {
          val c = carr(j)
          if (c < 32 || c == '"' || c == '\\') return true
          j += 1
        }
      }
      i += 4
    }
    // Tail: remaining 0-3 chars
    while (i < len) {
      val c = carr(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
  }

  // =========================================================================
  // hasEscapeChar(char[]) — P1: 16-bit lane SWAR
  // =========================================================================
  def hasEscapeChar(arr: Array[Char], from: Int, to: Int): Boolean = {
    val len = to - from
    if (len < 4) {
      return hasEscapeCharScalarChars(arr, from, to)
    }
    val cArr = arr.asInstanceOf[CharArray]
    var i = from
    val limit = to - 3
    while (i < limit) {
      val word = Intrinsics.loadLong(cArr.atRawUnsafe(i))
      if (swarHasMatch16(word)) {
        var j = i
        while (j < i + 4) {
          val c = arr(j)
          if (c < 32 || c == '"' || c == '\\') return true
          j += 1
        }
      }
      i += 4
    }
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
  }

  // =========================================================================
  // hasEscapeChar(byte[]) — 8-bit SWAR (unchanged, already optimal)
  // =========================================================================
  def hasEscapeChar(arr: Array[Byte], from: Int, to: Int): Boolean = {
    import scala.scalanative.runtime.ByteArray
    val len = to - from
    if (len < 8) {
      return hasEscapeCharScalarBytes(arr, from, to)
    }
    val barr = arr.asInstanceOf[ByteArray]
    var i = from
    val limit = to - 7
    while (i < limit) {
      val word = Intrinsics.loadLong(barr.atRawUnsafe(i))
      if (swarHasMatch8(word)) return true
      i += 8
    }
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return true
      i += 1
    }
    false
  }

  // =========================================================================
  // findFirstEscapeChar(byte[]) — 8-bit SWAR (for BaseByteRenderer chunked path)
  // =========================================================================
  def findFirstEscapeChar(arr: Array[Byte], from: Int, to: Int): Int = {
    import scala.scalanative.runtime.ByteArray
    val len = to - from
    if (len < 8) return findFirstEscapeCharScalarBytes(arr, from, to)
    val barr = arr.asInstanceOf[ByteArray]
    var i = from
    val limit = to - 7
    while (i < limit) {
      val word = Intrinsics.loadLong(barr.atRawUnsafe(i))
      if (swarHasMatch8(word)) {
        var j = i
        while (j < i + 8) {
          val b = arr(j) & 0xff
          if (b < 32 || b == '"' || b == '\\') return j
          j += 1
        }
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

  // =========================================================================
  // findFirstEscapeChar(char[]) — P1: 16-bit lane SWAR for char[]
  // =========================================================================
  def findFirstEscapeCharChar(arr: Array[Char], from: Int, to: Int): Int = {
    val len = to - from
    if (len < 4) return findFirstEscapeCharScalarChars(arr, from, to)
    val cArr = arr.asInstanceOf[CharArray]
    var i = from
    val limit = to - 3
    while (i < limit) {
      val word = Intrinsics.loadLong(cArr.atRawUnsafe(i))
      if (swarHasMatch16(word)) {
        var j = i
        while (j < i + 4) {
          val c = arr(j)
          if (c < 32 || c == '"' || c == '\\') return j
          j += 1
        }
      }
      i += 4
    }
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\') return i
      i += 1
    }
    -1
  }

  // =========================================================================
  // Scalar fallbacks
  // =========================================================================
  @inline private def hasEscapeCharScalar(s: String, len: Int): Boolean = {
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
  }

  @inline private def hasEscapeCharScalarChars(arr: Array[Char], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
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

  @inline private def findFirstEscapeCharScalarBytes(arr: Array[Byte], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return i
      i += 1
    }
    -1
  }

  @inline private def findFirstEscapeCharScalarChars(arr: Array[Char], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\') return i
      i += 1
    }
    -1
  }

  // =========================================================================
  // isAllAscii — SWAR-accelerated ASCII detection (4 chars per Long)
  // =========================================================================

  /** Mask for non-ASCII bits in 16-bit lanes: bit 7-15 set means char >= 0x80. */
  private final val NON_ASCII_16 = 0xff80ff80ff80ff80L

  /**
   * Returns true if all characters in the string are ASCII (< 0x80). Uses 16-bit SWAR to check 4
   * chars per Long iteration. For ASCII-only strings, codepoint operations (codePointCount,
   * offsetByCodePoints) can be replaced with direct char indexing.
   */
  def isAllAscii(s: String): Boolean = {
    val len = s.length
    if (len < 16) return isAllAsciiScalar(s, len)
    val carr = s.toCharArray
    val cArr = carr.asInstanceOf[CharArray]
    var i = 0
    val limit = len - 3
    while (i < limit) {
      val word = Intrinsics.loadLong(cArr.atRawUnsafe(i))
      if ((word & NON_ASCII_16) != 0L) return false
      i += 4
    }
    while (i < len) {
      if (carr(i) >= 0x80) return false
      i += 1
    }
    true
  }

  @inline private def isAllAsciiScalar(s: String, len: Int): Boolean = {
    var i = 0
    while (i < len) {
      if (s.charAt(i) >= 0x80) return false
      i += 1
    }
    true
  }

  // =========================================================================
  // compareStrings — P1: LLVM auto-vectorization friendly
  // =========================================================================

  private final val CMP_BUF_SIZE = 32768
  private val cmpBuf1: Array[Char] = new Array[Char](CMP_BUF_SIZE)
  private val cmpBuf2: Array[Char] = new Array[Char](CMP_BUF_SIZE)

  /**
   * Compare two strings by Unicode codepoint values. Uses bulk getChars + tight array loop for LLVM
   * auto-vectorization. Pre-allocated module-level buffers avoid per-call allocation.
   */
  def compareStrings(s1: String, s2: String): Int = {
    if (s1 eq s2) return 0
    val n1 = s1.length
    val n2 = s2.length
    val minLen = if (n1 < n2) n1 else n2

    if (minLen < 16 || n1 > CMP_BUF_SIZE || n2 > CMP_BUF_SIZE)
      return compareStringsScalar(s1, n1, s2, n2)

    val c1 = cmpBuf1
    val c2 = cmpBuf2
    s1.getChars(0, n1, c1, 0)
    s2.getChars(0, n2, c2, 0)

    // Tight comparison loop — bounds checks eliminated by length guarantee
    var i = 0
    while (i < minLen) {
      if (c1(i) != c2(i)) {
        val a = c1(i)
        val b = c2(i)
        if (!Character.isSurrogate(a) && !Character.isSurrogate(b)) {
          return a - b
        }
        var pos = i
        if (pos > 0 && Character.isLowSurrogate(a) && Character.isHighSurrogate(c1(pos - 1))) {
          pos -= 1
        }
        return compareCodepointsFrom(c1, n1, c2, n2, pos)
      }
      i += 1
    }
    if (n1 < n2) -1 else if (n1 > n2) 1 else 0
  }

  private def compareStringsScalar(s1: String, n1: Int, s2: String, n2: Int): Int = {
    val minLen = if (n1 < n2) n1 else n2
    var i = 0
    while (i < minLen) {
      val c1 = s1.charAt(i)
      val c2 = s2.charAt(i)
      if (c1 == c2) {
        i += 1
      } else if (!Character.isSurrogate(c1) && !Character.isSurrogate(c2)) {
        return c1 - c2
      } else {
        val cp1 = Character.codePointAt(s1, i)
        val cp2 = Character.codePointAt(s2, i)
        if (cp1 != cp2) return if (cp1 < cp2) -1 else 1
        i += Character.charCount(cp1)
      }
    }
    if (n1 < n2) -1 else if (n1 > n2) 1 else 0
  }

  private def compareCodepointsFrom(
      c1: Array[Char],
      n1: Int,
      c2: Array[Char],
      n2: Int,
      from: Int): Int = {
    var i1 = from
    var i2 = from
    while (i1 < n1 && i2 < n2) {
      val cp1 = Character.codePointAt(c1, i1)
      val cp2 = Character.codePointAt(c2, i2)
      if (cp1 != cp2) return if (cp1 < cp2) -1 else 1
      i1 += Character.charCount(cp1)
      i2 += Character.charCount(cp2)
    }
    if (i1 < n1) 1 else if (i2 < n2) -1 else 0
  }
}
