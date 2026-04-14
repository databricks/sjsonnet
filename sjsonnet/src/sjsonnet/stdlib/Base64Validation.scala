package sjsonnet.stdlib

/**
 * Shared base64 validation logic used by JVM and JS platforms. The Native platform uses
 * aklomp/base64 which enforces strict padding natively.
 */
object Base64Validation {

  /**
   * Validate strict RFC 4648 padding: reject ASCII input whose length is not a multiple of 4.
   *
   * java.util.Base64 is lenient and accepts unpadded input (e.g. "YQ" instead of "YQ=="), but the
   * Jsonnet spec requires strict compliance. Both go-jsonnet and C++ jsonnet reject unpadded input:
   *   - go-jsonnet: checks `len(str) % 4 != 0` before calling base64.StdEncoding.DecodeString
   *   - C++ jsonnet: rejects with "Not a base64 encoded string"
   *
   * We only apply this check when all characters are ASCII. Non-ASCII characters (e.g. "ĀQ=") are
   * never valid base64 and should be caught by java.util.Base64 with a more specific "Illegal
   * base64 character" error message. This matches go-jsonnet's behavior where len() counts UTF-8
   * bytes (so "ĀQ=" is 4 bytes, passes the length check, and fails on the invalid character).
   */
  def requireStrictPadding(input: String): Unit = {
    val len = input.length
    if (len > 0 && len % 4 != 0) {
      var allAscii = true
      var i = 0
      while (i < len && allAscii) {
        if (input.charAt(i) > 127) allAscii = false
        i += 1
      }
      if (allAscii) {
        throw new IllegalArgumentException(
          "Last unit does not have enough valid bits"
        )
      }
    }
  }
}
