package sjsonnet.stdlib

import java.nio.charset.StandardCharsets.UTF_8

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-md5 std.md5(s)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-base64 std.base64(input)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-base64Decode std.base64Decode(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-base64DecodeBytes std.base64DecodeBytes(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sha1 std.sha1(s)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sha256 std.sha256(s)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sha512 std.sha512(s)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sha3 std.sha3(s)]]
 */
object EncodingModule extends AbstractFunctionModule {
  def name = "encoding"

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-md5 std.md5(s)]].
   *
   * Since: 0.10.0. Group: Encoding.
   *
   * Encodes the given value into an MD5 string.
   */
  private object MD5 extends Val.Builtin1("md5", "s") {
    def evalRhs(s: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Platform.md5(s.value.asString))
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(MD5),
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-base64 std.base64(input)]].
     *
     * Since: 0.10.0. Group: Encoding.
     *
     * Encodes the given value into a base64 string. The encoding sequence is A-Za-z0-9+/ with = to
     * pad the output to a multiple of 4 characters. The value can be a string or an array of
     * numbers, but the codepoints / numbers must be in the 0 to 255 range. The resulting string has
     * no line breaks.
     */
    builtin("base64", "input") { (pos, _, input: Val) =>
      (input match {
        case Val.Str(_, value) =>
          Val.Str.asciiSafe(pos, PlatformBase64.encodeToString(value.getBytes(UTF_8)))
        case ba: Val.ByteArr =>
          Val.Str.asciiSafe(pos, PlatformBase64.encodeToString(ba.rawBytes))
        case arr: Val.Arr =>
          val len = arr.length
          val byteArr = new Array[Byte](len)
          var i = 0
          while (i < len) {
            arr.value(i) match {
              case v: Val.Num =>
                val vInt = v.asInt
                if (vInt < 0 || vInt > 255) {
                  Error.fail(
                    f"Found an invalid codepoint value at position $i (must be 0 <= X <= 255), got $vInt"
                  )
                }
                byteArr(i) = vInt.toByte
              case v =>
                Error.fail(
                  f"Expected an array of numbers, got a ${v.prettyName} at position $i"
                )
            }
            i += 1
          }
          Val.Str.asciiSafe(pos, PlatformBase64.encodeToString(byteArr))
        case x => Error.fail("Cannot base64 encode " + x.prettyName)
      }): Val
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-base64Decode std.base64Decode(str)]].
     *
     * Since: 0.10.0. Group: Encoding.
     *
     * Deprecated, use std.base64DecodeBytes and decode the string explicitly (e.g. with
     * std.decodeUTF8) instead.
     *
     * Behaves like std.base64DecodeBytes() except returns a naively encoded string instead of an
     * array of bytes.
     */
    builtin("base64Decode", "str") { (_, _, str: String) =>
      try {
        new String(PlatformBase64.decode(str), UTF_8)
      } catch {
        case e: IllegalArgumentException =>
          Error.fail("Invalid base64 string: " + e.getMessage)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-base64DecodeBytes std.base64DecodeBytes(str)]].
     *
     * Since: 0.10.0. Group: Encoding.
     *
     * Decodes the given base64 string into an array of bytes (number values). Currently assumes the
     * input string has no linebreaks and is padded to a multiple of 4 (with the = character). In
     * other words, it consumes the output of std.base64().
     */
    builtin("base64DecodeBytes", "str") { (pos, _, str: String) =>
      try {
        Val.Arr.fromBytes(pos, PlatformBase64.decode(str))
      } catch {
        case e: IllegalArgumentException =>
          Error.fail("Invalid base64 string: " + e.getMessage)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sha1 std.sha1(s)]].
     *
     * Since: 0.21.0. Group: Encoding.
     *
     * Encodes the given value into an SHA1 string.
     *
     * This function is only available in Go version of jsonnet.
     */
    builtin("sha1", "str") { (_, _, str: String) =>
      Platform.sha1(str)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sha256 std.sha256(s)]].
     *
     * Since: 0.21.0. Group: Encoding.
     *
     * Encodes the given value into an SHA256 string.
     *
     * This function is only available in Go version of jsonnet.
     */
    builtin("sha256", "str") { (_, _, str: String) =>
      Platform.sha256(str)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sha512 std.sha512(s)]].
     *
     * Since: 0.21.0. Group: Encoding.
     *
     * Encodes the given value into an SHA512 string.
     *
     * This function is only available in Go version of jsonnet.
     */
    builtin("sha512", "str") { (_, _, str: String) =>
      Platform.sha512(str)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sha3 std.sha3(s)]].
     *
     * Since: 0.21.0. Group: Encoding.
     *
     * Encodes the given value into an SHA3 string.
     *
     * This function is only available in Go version of jsonnet.
     */
    builtin("sha3", "str") { (_, _, str: String) =>
      Platform.sha3(str)
    }
  )
}
