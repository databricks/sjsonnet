package sjsonnet.stdlib

/**
 * High-performance Base64 encoder/decoder optimized for the Jsonnet use case.
 *
 * Avoids the overhead of java.util.Base64 on Scala Native by using:
 *   - Direct char→char encoding (no intermediate byte[] for ASCII strings)
 *   - Pre-computed lookup tables as char arrays
 *   - Tight while-loops processing 3→4 (encode) or 4→3 (decode) units
 *
 * Falls back to standard getBytes("UTF-8") + java.util.Base64 only for non-ASCII input strings,
 * which is rare in practice.
 */
object FastBase64 {

  // Encoding lookup table (6-bit index → base64 char)
  private val ENCODE_TABLE: Array[Char] = {
    val t = new Array[Char](64)
    var i = 0
    while (i < 26) { t(i) = ('A' + i).toChar; i += 1 }
    while (i < 52) { t(i) = ('a' + i - 26).toChar; i += 1 }
    while (i < 62) { t(i) = ('0' + i - 52).toChar; i += 1 }
    t(62) = '+'
    t(63) = '/'
    t
  }

  // Decoding lookup table (byte value → 6-bit decoded value, -1 for invalid, -2 for padding '=')
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
    t('=') = -2
    t
  }

  /**
   * Encode a String to base64. Uses a fast ASCII path that avoids the intermediate byte[]
   * allocation when all characters are in the 0–127 range (common case for Jsonnet).
   */
  def encodeString(s: String): String = {
    val len = s.length
    if (len == 0) return ""

    // Check if all chars are ASCII (< 128) in a single pass
    var allAscii = true
    var check = 0
    while (check < len) {
      if (s.charAt(check) >= 128) { allAscii = false; check = len }
      check += 1
    }

    if (allAscii) {
      encodeAsciiDirect(s, len)
    } else {
      // Fallback for non-ASCII: use standard UTF-8 encoding
      encodeBytes(s.getBytes("UTF-8"))
    }
  }

  /**
   * Encode a byte array to base64 string. Used for both the byte-array input path and as fallback
   * for non-ASCII strings.
   */
  def encodeBytes(bytes: Array[Byte]): String = {
    val len = bytes.length
    if (len == 0) return ""
    val outLen = 4 * ((len + 2) / 3)
    val out = new Array[Char](outLen)
    val table = ENCODE_TABLE

    var i = 0
    var j = 0
    // Process 3-byte groups
    val limit = len - 2
    while (i < limit) {
      val a = bytes(i) & 0xff
      val b = bytes(i + 1) & 0xff
      val c = bytes(i + 2) & 0xff
      out(j) = table(a >>> 2)
      out(j + 1) = table(((a << 4) | (b >>> 4)) & 0x3f)
      out(j + 2) = table(((b << 2) | (c >>> 6)) & 0x3f)
      out(j + 3) = table(c & 0x3f)
      i += 3
      j += 4
    }
    // Handle remaining 1 or 2 bytes with padding
    val remaining = len - i
    if (remaining == 1) {
      val a = bytes(i) & 0xff
      out(j) = table(a >>> 2)
      out(j + 1) = table((a << 4) & 0x3f)
      out(j + 2) = '='
      out(j + 3) = '='
    } else if (remaining == 2) {
      val a = bytes(i) & 0xff
      val b = bytes(i + 1) & 0xff
      out(j) = table(a >>> 2)
      out(j + 1) = table(((a << 4) | (b >>> 4)) & 0x3f)
      out(j + 2) = table((b << 2) & 0x3f)
      out(j + 3) = '='
    }
    new String(out)
  }

  /**
   * Fast path: encode an ASCII string directly to base64 without creating an intermediate byte
   * array. Each char is treated as a single byte (valid since all chars are < 128).
   */
  private def encodeAsciiDirect(s: String, len: Int): String = {
    val outLen = 4 * ((len + 2) / 3)
    val out = new Array[Char](outLen)
    val table = ENCODE_TABLE

    var i = 0
    var j = 0
    val limit = len - 2
    while (i < limit) {
      val a = s.charAt(i).toInt
      val b = s.charAt(i + 1).toInt
      val c = s.charAt(i + 2).toInt
      out(j) = table(a >>> 2)
      out(j + 1) = table(((a << 4) | (b >>> 4)) & 0x3f)
      out(j + 2) = table(((b << 2) | (c >>> 6)) & 0x3f)
      out(j + 3) = table(c & 0x3f)
      i += 3
      j += 4
    }
    val remaining = len - i
    if (remaining == 1) {
      val a = s.charAt(i).toInt
      out(j) = table(a >>> 2)
      out(j + 1) = table((a << 4) & 0x3f)
      out(j + 2) = '='
      out(j + 3) = '='
    } else if (remaining == 2) {
      val a = s.charAt(i).toInt
      val b = s.charAt(i + 1).toInt
      out(j) = table(a >>> 2)
      out(j + 1) = table(((a << 4) | (b >>> 4)) & 0x3f)
      out(j + 2) = table((b << 2) & 0x3f)
      out(j + 3) = '='
    }
    new String(out)
  }

  /**
   * Decode a base64 string to a UTF-8 string. Decodes directly to a byte array, then constructs the
   * string.
   */
  def decodeToString(s: String): String = {
    val bytes = decodeToBytes(s)
    new String(bytes, "UTF-8")
  }

  /**
   * Map a char to a byte value matching ISO-8859-1 behavior: chars 0–255 map to their code point,
   * chars > 255 map to '?' (0x3F), matching java.util.Base64 which uses String.getBytes(ISO_8859_1)
   * internally.
   */
  @inline private def charToIso8859(c: Char): Int =
    if (c <= 0xff) c.toInt else 0x3f

  /**
   * Decode a base64 string to a byte array. Uses a tight loop that processes 4 input characters at
   * a time, producing 3 output bytes.
   *
   * Matches java.util.Base64 behavior for edge cases:
   *   - Non-ASCII chars (> 0xFF) are replaced with '?' (ISO-8859-1 behavior)
   *   - Incomplete final groups (e.g. 1 extra char) throw with descriptive message
   */
  def decodeToBytes(s: String): Array[Byte] = {
    val len = s.length
    if (len == 0) return Array.emptyByteArray

    val table = DECODE_TABLE

    // First pass: count valid base64 chars (skip padding and whitespace for MIME)
    // to determine output size and validate structure.
    var validCount = 0
    var paddingCount = 0
    var ci = 0
    while (ci < len) {
      val b = charToIso8859(s.charAt(ci))
      val v = table(b)
      if (v >= 0) validCount += 1
      else if (v == -2) paddingCount += 1
      else {
        throw new IllegalArgumentException(
          "Illegal base64 character " + Integer.toHexString(b)
        )
      }
      ci += 1
    }

    val totalChars = validCount + paddingCount
    val remainder = totalChars % 4
    // If the total (valid + padding) is not a multiple of 4, check the leftover portion
    if (remainder == 1) {
      throw new IllegalArgumentException(
        "Last unit does not have enough valid bits"
      )
    }

    // Calculate output length based on valid groups
    val fullGroups = validCount / 4
    val leftover = validCount % 4
    val outLen = fullGroups * 3 + (if (leftover == 3) 2 else if (leftover == 2) 1 else 0)
    val out = new Array[Byte](outLen)

    var i = 0
    var j = 0

    // Process complete 4-char groups (ignoring padding chars)
    val groupLimit = len - 3
    while (i < groupLimit && j + 2 < outLen) {
      val a = table(charToIso8859(s.charAt(i)))
      val b = table(charToIso8859(s.charAt(i + 1)))
      val c = table(charToIso8859(s.charAt(i + 2)))
      val d = table(charToIso8859(s.charAt(i + 3)))
      // All values should be >= 0 here since we already validated above

      // 3 output bytes (no padding)
      out(j) = ((a << 2) | (b >>> 4)).toByte
      out(j + 1) = (((b << 4) | (c >>> 2)) & 0xff).toByte
      out(j + 2) = (((c << 6) | d) & 0xff).toByte
      j += 3
      i += 4
    }

    // Handle final group with padding
    if (j < outLen) {
      val remaining = len - i
      if (remaining >= 2) {
        val a = table(charToIso8859(s.charAt(i)))
        val b = table(charToIso8859(s.charAt(i + 1)))
        out(j) = ((a << 2) | (b >>> 4)).toByte
        j += 1
        if (remaining >= 3 && j < outLen) {
          val c = table(charToIso8859(s.charAt(i + 2)))
          if (c >= 0) {
            out(j) = (((b << 4) | (c >>> 2)) & 0xff).toByte
            j += 1
          }
        }
      }
    }
    out
  }
}
