package sjsonnet

/** Scalar fallback for Scala.js — no SWAR, per-char scan. */
object CharSWAR {
  def hasEscapeChar(s: String): Boolean = {
    var i = 0
    val len = s.length
    while (i < len) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\' || (c >= 0x7f && c <= 0x9f)) return true
      i += 1
    }
    false
  }

  def hasEscapeChar(arr: Array[Char], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val c = arr(i)
      if (c < 32 || c == '"' || c == '\\' || (c >= 0x7f && c <= 0x9f)) return true
      i += 1
    }
    false
  }

  def isAsciiJsonSafe(s: String): Boolean = isAsciiJsonSafe(s, 0, s.length)

  def isAsciiJsonSafe(s: String, from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val c = s.charAt(i)
      if (c < 32 || c == '"' || c == '\\' || c >= 0x7f) return false
      i += 1
    }
    true
  }

  /** Scalar scan for byte[] — used by ByteRenderer for UTF-8 encoded data. */
  def hasEscapeChar(arr: Array[Byte], from: Int, to: Int): Boolean = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\' || b == 0x7f) return true
      i += 1
    }
    false
  }

  def findFirstEscapeChar(arr: Array[Byte], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      val b = arr(i) & 0xff
      if (b < 32 || b == '"' || b == '\\' || b == 0x7f) return i
      i += 1
    }
    -1
  }
}
