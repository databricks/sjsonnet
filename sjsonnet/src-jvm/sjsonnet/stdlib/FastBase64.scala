package sjsonnet.stdlib

import java.util.Base64

/**
 * JVM-specific Base64 implementation. Delegates to java.util.Base64 which is
 * highly optimized on the JVM via C2 intrinsics. No point reinventing this.
 */
object FastBase64 {

  def encodeString(s: String): String =
    Base64.getEncoder.encodeToString(s.getBytes("UTF-8"))

  def encodeBytes(bytes: Array[Byte]): String =
    Base64.getEncoder.encodeToString(bytes)

  def decodeToString(s: String): String =
    new String(Base64.getDecoder.decode(s))

  def decodeToBytes(s: String): Array[Byte] =
    Base64.getDecoder.decode(s)
}
