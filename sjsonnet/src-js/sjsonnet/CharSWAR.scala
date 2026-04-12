package sjsonnet

/** Scalar fallback for Scala.js — no SWAR, per-char scan. */
object CharSWAR {
  def hasEscapeChar(s: String): Boolean = {
    var i = 0
    val len = s.length
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\') return true
      i += 1
    }
    false
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

  /** Scalar scan for byte[] — used by ByteRenderer for UTF-8 encoded data. */
  def hasEscapeChar(arr: Array[Byte], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return true
      i += 1
    }
    false
  }

  /** Scalar scan returning position of first escape char, or -1 if none. */
  def findFirstEscapeChar(arr: Array[Byte], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\') return i
      i += 1
    }
    -1
  }

  /**
   * Compare two strings by Unicode codepoint values. Scalar fallback for Scala.js.
   * Uses equal-char-skip fast path with deferred surrogate check.
   */
  def compareStrings(s1: String, s2: String): Int = {
    if (s1 eq s2) return 0
    val n1 = s1.length
    val n2 = s2.length
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
}
