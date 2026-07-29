package sjsonnet

/** Lowercase hex encoding, shared across platforms. */
object Hex {
  private val hexChars = "0123456789abcdef".toCharArray

  def encode(bytes: Array[Byte]): String = {
    val out = new Array[Char](bytes.length * 2)
    var i = 0
    var j = 0
    while (i < bytes.length) {
      val b = bytes(i) & 0xff
      out(j) = hexChars(b >>> 4)
      out(j + 1) = hexChars(b & 0x0f)
      i += 1
      j += 2
    }
    new String(out)
  }
}
