package sjsonnet.stdlib

import sjsonnet.Std
import sjsonnet.TestUtils.eval
import utest.*

object NativeXzTests extends TestSuite {
  private val std = new Std(nativeFunctions = Map() ++ new NativeXz().functions)

  val tests: Tests = Tests {
    test("xz") {
      eval("""std.native('xz')([1, 2])""", std = std) ==> ujson.Str(
        "/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla"
      )
      eval("""std.native('xz')("hi")""", std = std) ==> ujson.Str(
        "/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla"
      )
      eval("""std.native('xz')([1, 2], compressionLevel = 0)""", std = std) ==> ujson.Str(
        "/Td6WFoAAATm1rRGAgAhAQwAAACPmEGcAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla"
      )
      eval("""std.native('xz')("hi", compressionLevel = 1)""", std = std) ==> ujson.Str(
        "/Td6WFoAAATm1rRGAgAhARAAAACocI6GAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla"
      )
      val ex = intercept[Exception] {
        // Compression level 10 is invalid
        eval("""std.native('xz')("hi", 10)""", std = std)
      }
      assert(ex.getMessage.contains("Unsupported preset: 10"))
    }
  }
}
