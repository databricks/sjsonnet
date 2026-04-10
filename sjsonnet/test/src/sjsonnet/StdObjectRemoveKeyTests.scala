package sjsonnet

import utest._
import TestUtils.eval

object StdObjectRemoveKeyTests extends TestSuite {

  def tests: Tests = Tests {

    test("basic removal") {
      eval("std.objectRemoveKey({a: 1, b: 2}, 'a')") ==> ujson.Obj("b" -> 2)
      eval("std.objectRemoveKey({a: 1, b: 2}, 'b')") ==> ujson.Obj("a" -> 1)
      eval("std.objectRemoveKey({a: 1}, 'a')") ==> ujson.Obj()
    }

    test("removing non-existent key is a no-op") {
      eval("std.objectRemoveKey({a: 1}, 'b')") ==> ujson.Obj("a" -> 1)
    }

    test("re-adding removed key via object inheritance") {
      eval("""std.objectRemoveKey({foo: "bar"}, "foo") + {foo: "bar2"}""") ==>
      ujson.Obj("foo" -> "bar2")

      eval("""std.objectRemoveKey({a: 1, b: 2}, "a") + {a: 3}""") ==>
      ujson.Obj("b" -> 2, "a" -> 3)

      eval("""std.objectRemoveKey({a: 1, b: 2, c: 3}, "b") + {b: 99, d: 4}""") ==>
      ujson.Obj("a" -> 1, "c" -> 3, "b" -> 99, "d" -> 4)
    }

    test("double removal then re-add") {
      eval(
        """std.objectRemoveKey(std.objectRemoveKey({a: 1, b: 2, c: 3}, "a"), "b") + {a: 10}"""
      ) ==> ujson.Obj("c" -> 3, "a" -> 10)
    }

    test("remove then re-add then remove again") {
      eval(
        """std.objectRemoveKey(std.objectRemoveKey({foo: 1, bar: 2}, "foo") + {foo: 3}, "foo")"""
      ) ==> ujson.Obj("bar" -> 2)
    }

    test("internal super chain preserved after removal") {
      eval("std.objectRemoveKey({a: 1} + {b: super.a}, 'a')") ==> ujson.Obj("b" -> 1)
    }

    test("external inheritance preserved after removal") {
      eval("{a: 1} + std.objectRemoveKey({b: super.a}, 'a')") ==>
      ujson.Obj("a" -> 1, "b" -> 1)
    }

    test("LHS key preserved when RHS removes it") {
      eval("""{a: 1} + std.objectRemoveKey({a: 2}, "a")""") ==>
      ujson.Obj("a" -> 1)

      eval("""{a: 1} + std.objectRemoveKey({a: 2, b: 3}, "a")""") ==>
      ujson.Obj("a" -> 1, "b" -> 3)

      eval("""{a: 10} + std.objectRemoveKey({a: 1} + {b: super.a}, "a")""") ==>
      ujson.Obj("a" -> 10, "b" -> 1)
    }

    test("containsKey and objectFields reflect re-added key") {
      eval(
        """local r = std.objectRemoveKey({a: 1}, "a") + {a: 2};
          |std.objectHas(r, "a")""".stripMargin
      ) ==> ujson.True

      eval(
        """local r = std.objectRemoveKey({a: 1}, "a") + {a: 2};
          |std.objectFields(r)""".stripMargin
      ) ==> ujson.Arr("a")
    }
  }
}
