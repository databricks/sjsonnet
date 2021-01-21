package sjsonnet

import utest._
import TestUtils.eval
object StdMergePatchTests extends TestSuite {

  def tests = Tests {
    test {
      eval("std.mergePatch([{a: 1}], [{b: 2}])") ==> ujson.Arr(ujson.Obj("b" -> 2))
    }
    test {
      eval("std.mergePatch(null, {a: 1})") ==> ujson.Obj("a" -> 1)
    }
    test {
      eval("std.mergePatch({a: null}, {b: 2})") ==> ujson.Obj("a" -> ujson.Null, "b" -> 2)
    }
    test {
      eval("{a:: 1} + std.mergePatch({}, {a: 2})") ==> ujson.Obj("a" -> 2)
    }
    test {
      eval("{a: 1} + std.mergePatch({}, {a+: 2})") ==> ujson.Obj("a" -> 2)
    }

    test {
      eval("""std.mergePatch({"a": {b: "B"}}, {a: {c: "C"}})""") ==>
        ujson.Obj("a" -> ujson.Obj("b" -> "B", "c" -> "C"))
    }
  }
}
