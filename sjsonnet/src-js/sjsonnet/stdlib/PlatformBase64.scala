package sjsonnet.stdlib

import java.nio.charset.StandardCharsets

/**
 * Scala.js implementation of base64 encode/decode. Delegates to java.util.Base64 (provided by
 * Scala.js stdlib emulation).
 */
object PlatformBase64 {

  def encodeToString(input: Array[Byte]): String =
    java.util.Base64.getEncoder.encodeToString(input)

  /** See JVM `PlatformBase64.encodeStringToString` — same contract. */
  def encodeStringToString(input: String, asciiSafe: Boolean): String = {
    val charset = if (asciiSafe) StandardCharsets.ISO_8859_1 else StandardCharsets.UTF_8
    java.util.Base64.getEncoder.encodeToString(input.getBytes(charset))
  }

  def decode(input: String): Array[Byte] = {
    Base64Validation.requireStrictPadding(input)
    java.util.Base64.getDecoder.decode(input)
  }
}
