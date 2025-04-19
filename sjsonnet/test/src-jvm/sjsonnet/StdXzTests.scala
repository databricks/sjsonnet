package sjsonnet

import utest._
import TestUtils.eval

object StdXzTests extends TestSuite {
  val tests: Tests = Tests {
    test("xz"){
      eval("""std.xz([1, 2])""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.xz("hi")""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.xz([1, 2], compressionLevel = 0)""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhAQwAAACPmEGcAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.xz("hi", compressionLevel = 1)""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARAAAACocI6GAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla")
      var ex = intercept[Exception] {
        // Compression level 10 is invalid
        eval("""std.xz("hi", 10)""")
      }
      assert(ex.getMessage.contains("Unsupported preset: 10"))
      eval("""std.native('xz')([1, 2])""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.native('xz')("hi")""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARYAAAB0L+WjAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.native('xz')([1, 2], compressionLevel = 0)""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhAQwAAACPmEGcAQABAQIAAADRC9qlUgJ94gABGgLcLqV+H7bzfQEAAAAABFla")
      eval("""std.native('xz')("hi", compressionLevel = 1)""") ==> ujson.Str("/Td6WFoAAATm1rRGAgAhARAAAACocI6GAQABaGkAAAD+qTgRvMqlSAABGgLcLqV+H7bzfQEAAAAABFla")
      ex = intercept[Exception] {
        // Compression level 10 is invalid
        eval("""std.native('xz')("hi", 10)""")
      }
      assert(ex.getMessage.contains("Unsupported preset: 10"))
    }
  }
}

