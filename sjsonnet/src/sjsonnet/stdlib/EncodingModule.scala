package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import java.util.Base64

class EncodingModule extends AbstractFunctionModule {
  def name = "encoding"

  private object MD5 extends Val.Builtin1("md5", "s") {
    def evalRhs(s: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Platform.md5(s.force.asString))
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(MD5),
    builtin("base64", "input") { (_, _, input: Val) =>
      input match {
        case Val.Str(_, value) =>
          Base64.getEncoder.encodeToString(value.getBytes("UTF-8"))
        case arr: Val.Arr =>
          val byteArr = new Array[Byte](arr.length)
          var i = 0
          while (i < arr.length) {
            val v = arr.force(i)
            if (!v.isInstanceOf[Val.Num]) {
              Error.fail(f"Expected an array of numbers, got a ${v.prettyName} at position $i")
            }
            val vInt = v.asInt
            if (vInt < 0 || vInt > 255) {
              Error.fail(
                f"Found an invalid codepoint value at position $i (must be 0 <= X <= 255), got $vInt"
              )
            }
            byteArr(i) = vInt.toByte
            i += 1
          }
          Base64.getEncoder.encodeToString(byteArr)
        case x => Error.fail("Cannot base64 encode " + x.prettyName)
      }
    },
    builtin("base64Decode", "str") { (_, _, str: String) =>
      try {
        new String(Base64.getDecoder.decode(str))
      } catch {
        case e: IllegalArgumentException =>
          Error.fail("Invalid base64 string: " + e.getMessage)
      }
    },
    builtin("base64DecodeBytes", "str") { (pos, _, str: String) =>
      try {
        Val.Arr(pos, Base64.getDecoder.decode(str).map(i => Val.Num(pos, i)))
      } catch {
        case e: IllegalArgumentException =>
          Error.fail("Invalid base64 string: " + e.getMessage)
      }
    },
    builtin("sha1", "str") { (_, _, str: String) =>
      Platform.sha1(str)
    },
    builtin("sha256", "str") { (_, _, str: String) =>
      Platform.sha256(str)
    },
    builtin("sha512", "str") { (_, _, str: String) =>
      Platform.sha512(str)
    },
    builtin("sha3", "str") { (_, _, str: String) =>
      Platform.sha3(str)
    }
  )
}
