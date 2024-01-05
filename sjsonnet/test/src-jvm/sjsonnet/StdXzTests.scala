package sjsonnet

import utest._
import TestUtils.eval

object StdXzTests extends TestSuite {
  val tests = Tests {
    test("xz"){
      eval("""std.xz([1, 2])""")
      eval("""std.xz("hi")""")
      eval("""std.xz([1, 2], compressionLevel = 0)""")
      eval("""std.xz("hi", compressionLevel = 1)""")
      val ex = intercept[Exception] {
        // Compression level 10 is invalid
        eval("""std.xz("hi", 10)""")
      }
      assert(ex.getMessage.contains("Unsupported preset: 10"))
    }
  }
}

