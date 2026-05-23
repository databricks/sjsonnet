package sjsonnet.stdlib

import java.nio.charset.StandardCharsets

/**
 * JVM implementation of base64 encode/decode. Delegates to java.util.Base64 which has HotSpot
 * intrinsics for high performance.
 */
object PlatformBase64 {

  def encodeToString(input: Array[Byte]): String =
    java.util.Base64.getEncoder.encodeToString(input)

  /**
   * Encode a `String` directly to a base64 string. The `asciiSafe` flag is a hot-path hint: when
   * `true`, the caller has already proven every char fits in 0x00–0x7F, so we can use ISO-8859-1
   * instead of UTF-8 — both produce byte-identical output for ASCII, but ISO-8859-1 skips the UTF-8
   * encoder's pre-count scan over the input. On JVMs with compact strings (Java 9+) a pure-ASCII
   * string is already LATIN1-tagged, so `getBytes(ISO_8859_1)` is essentially an array copy. The
   * intermediate `Array[Byte]` allocation is unavoidable on the JVM (the platform Base64 encoder
   * takes a byte array); the bigger win lives on the Scala Native side.
   */
  def encodeStringToString(input: String, asciiSafe: Boolean): String = {
    val charset = if (asciiSafe) StandardCharsets.ISO_8859_1 else StandardCharsets.UTF_8
    java.util.Base64.getEncoder.encodeToString(input.getBytes(charset))
  }

  def decode(input: String): Array[Byte] = {
    Base64Validation.requireStrictPadding(input)
    java.util.Base64.getDecoder.decode(input)
  }
}
