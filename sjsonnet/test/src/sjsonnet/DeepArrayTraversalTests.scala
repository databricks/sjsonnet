package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object DeepArrayTraversalTests extends TestSuite {
  val tests: Tests = Tests {
    test("deepJoin and flattenDeepArray preserve default depth and honor larger maxStack") {
      val nested =
        """local nest(n, x) = if n == 0 then x else [nest(n - 1, x)];
          |""".stripMargin

      eval(nested + """std.deepJoin(nest(600, "x"))""") ==> ujson.Str("x")
      eval(nested + """std.flattenDeepArray(nest(600, "x"))""") ==> ujson.Arr("x")

      eval(nested + """std.deepJoin(nest(1100, "x"))""", maxStack = 1200) ==> ujson.Str("x")
      eval(nested + """std.flattenDeepArray(nest(1100, "x"))""", maxStack = 1200) ==>
      ujson.Arr("x")
    }
  }
}
