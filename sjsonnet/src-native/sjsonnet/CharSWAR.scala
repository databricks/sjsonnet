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

  /**
   * SWAR: returns true if any byte lane in `word` contains '"' (0x22), '\\' (0x5C), or a control
   * char (< 0x20).
   */
  @inline private def swarHasMatch(word: Long): Boolean = {
    // 1. Detect '"' via XOR + zero-detection
    val q = word ^ QUOTE
    val qz = ~((q & HOLE) + HOLE | q | HOLE)

    // 2. Detect '\\' via XOR + zero-detection
    val b = word ^ BSLAS
    val bz = ~((b & HOLE) + HOLE | b | HOLE)

    // 3. Detect control chars: byte & 0xE0 == 0 → c < 32
    val c = word & CTRL
    val cz = ~((c & HOLE) + HOLE | c | HOLE)

    (qz | bz | cz) != 0L
  }

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
      if (swarHasMatch(word)) return true
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

  @inline private def hasEscapeCharScalar(s: String, len: Int): Boolean = {
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
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

  // =========================================================================
  // findFirstEscapeChar — position-returning SWAR scan for chunked rendering
  // =========================================================================

  /**
   * Find the index of the first byte in `arr(from until to)` that needs JSON string escaping.
   * Returns -1 if no escape char is found. Uses SWAR via Intrinsics.loadLong.
   */
  def findFirstEscapeChar(arr: Array[Byte], from: Int, to: Int): Int = {
    val len = to - from
    if (len < 8) return findFirstEscapeCharScalar(arr, from, to)
    val barr = arr.asInstanceOf[ByteArray]
    var i = from
    val limit = to - 7
    while (i < limit) {
      val word = Intrinsics.loadLong(barr.atRawUnsafe(i))
      if (swarHasMatch(word)) {
        // Pinpoint exact byte within the matched 8-byte word
        var j = i
        while (j < i + 8) {
          val b = arr(j) & 0xff
          if (b < 32 || b == '"' || b == '\\') return j
          j += 1
        }
      }
      i += 8
    }
    // Tail
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return i
      i += 1
    }
    -1
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

  // =========================================================================
  // compareStrings — codepoint-correct string comparison
  // =========================================================================

  // Pre-allocated char buffers for string comparison.
  // Scala Native is single-threaded, so module-level buffers are safe.
  private final val CMP_BUF_SIZE = 32768
  private val cmpBuf1: Array[Char] = new Array[Char](CMP_BUF_SIZE)
  private val cmpBuf2: Array[Char] = new Array[Char](CMP_BUF_SIZE)

  /**
   * Compare two strings by Unicode codepoint values. Uses bulk getChars + tight array loop for
   * LLVM auto-vectorization. Surrogate checks deferred to mismatch point only.
   * Pre-allocated module-level buffers avoid per-call allocation overhead.
   */
  def compareStrings(s1: String, s2: String): Int = {
    if (s1 eq s2) return 0
    val n1 = s1.length
    val n2 = s2.length
    val minLen = math.min(n1, n2)

    if (minLen < 16 || n1 > CMP_BUF_SIZE || n2 > CMP_BUF_SIZE)
      return compareStringsScalar(s1, n1, s2, n2)

    // Bulk-copy to pre-allocated arrays — zero allocation, enables LLVM auto-vectorization
    val c1 = cmpBuf1
    val c2 = cmpBuf2
    s1.getChars(0, n1, c1, 0)
    s2.getChars(0, n2, c2, 0)

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
    Integer.compare(n1, n2)
  }

  private def compareStringsScalar(s1: String, n1: Int, s2: String, n2: Int): Int = {
    val minLen = math.min(n1, n2)
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
        if (cp1 != cp2) return Integer.compare(cp1, cp2)
        i += Character.charCount(cp1)
      }
    }
    Integer.compare(n1, n2)
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
      if (cp1 != cp2) return Integer.compare(cp1, cp2)
      i1 += Character.charCount(cp1)
      i2 += Character.charCount(cp2)
    }
    if (i1 < n1) 1 else if (i2 < n2) -1 else 0
  }
}
