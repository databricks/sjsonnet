package sjsonnet.stdlib

/**
 * JVM implementation of base64 encode/decode. Delegates to java.util.Base64 which has HotSpot
 * intrinsics for high performance.
 */
object PlatformBase64 {

  def encodeToString(input: Array[Byte]): String =
    java.util.Base64.getEncoder.encodeToString(input)

  def decode(input: String): Array[Byte] = {
    Base64Validation.requireStrictPadding(input)
    java.util.Base64.getDecoder.decode(input)
  }
}
