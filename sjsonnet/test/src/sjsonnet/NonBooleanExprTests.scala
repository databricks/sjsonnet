package sjsonnet

import utest._
import TestUtils.{eval, evalErr}
object NonBooleanExprTests extends TestSuite {
  def tests: Tests = Tests {
    test("nonBooleanExpr") {
      eval("local boo(x) = true; [x for x in [1,2,3] if boo(x)]").toString() ==> "[1,2,3]"
      eval("[x for x in [1,2,3] if x < 0]").toString() ==> "[]"
      val ex = assertThrows[Exception] {
        eval("[x for x in [1,2,3] if 'a']")
      }
      assert(ex.getMessage.contains("Condition must be boolean, got string"))
    }

    test("std.filter rejects non-boolean predicate result") {
      assert(
        evalErr("""std.filter(function(x) 42, [1, 2, 3])""")
          .contains("expected Boolean")
      )
    }

    test("std.filterMap rejects non-boolean predicate result") {
      assert(
        evalErr("""std.filterMap(function(x) 42, function(x) x, [1, 2, 3])""")
          .contains("expected Boolean")
      )
    }
  }
}
