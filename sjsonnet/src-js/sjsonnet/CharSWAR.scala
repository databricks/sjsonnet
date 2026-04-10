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
}
