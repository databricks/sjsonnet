package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object StdLibOfficialCompatibilityTests extends TestSuite {
  def tests: Tests = Tests {
    test("mapWithIndex accepts strings") {
      eval("""std.mapWithIndex(function(i, x) i + std.codepoint(x), "ab")""") ==>
      ujson.Arr(97, 99)
    }

    test("flatten arrays follows std.jsonnet fold semantics") {
      eval("""std.flattenArrays([[1], [], [2]])""") ==> ujson.Arr(1, 2)
      assert(evalErr("""std.flattenArrays([[1], null, [2]])""").contains("array and null"))
    }

    test("flattenDeepArray wraps scalar values") {
      eval("""std.flattenDeepArray(1)""") ==> ujson.Arr(1)
      eval("""std.flattenDeepArray(null)""") ==> ujson.Arr(ujson.Null)
      eval("""std.flattenDeepArray([1, [2, [3]]])""") ==> ujson.Arr(1, 2, 3)
    }

    test("repeat reports official negative count error") {
      assert(evalErr("""std.repeat("a", -1)""").contains("makeArray requires size >= 0, got -1"))
    }

    test("removeAt filters by exact index equality") {
      eval("""std.removeAt([1, 2, 3], 1)""") ==> ujson.Arr(1, 3)
      eval("""std.removeAt([1, 2, 3], 1.5)""") ==> ujson.Arr(1, 2, 3)
      eval("""std.removeAt([1, 2, 3], -1)""") ==> ujson.Arr(1, 2, 3)
      eval("""std.removeAt([1, 2, 3], 9)""") ==> ujson.Arr(1, 2, 3)
      eval("""std.removeAt([1, 2, 3], "1")""") ==> ujson.Arr(1, 2, 3)
    }

    test("isEmpty delegates to std.length") {
      eval("""std.isEmpty("")""") ==> ujson.True
      eval("""std.isEmpty([])""") ==> ujson.True
      eval("""std.isEmpty({})""") ==> ujson.True
      eval("""std.isEmpty(function() 1)""") ==> ujson.True
      eval("""std.isEmpty(function(a, b) a)""") ==> ujson.False
      assert(evalErr("""std.isEmpty(10)""").contains("length operates on strings"))
    }

    test("escape string helpers stringify non-string inputs") {
      eval("""std.escapeStringPython(10)""") ==> ujson.Str("\"10\"")
      eval("""std.escapeStringBash(10)""") ==> ujson.Str("'10'")
      eval("""std.escapeStringDollars("$a")""") ==> ujson.Str("$$a")
      eval("""std.escapeStringDollars(10)""") ==> ujson.Str("10")
      eval("""std.escapeStringXML(10)""") ==> ujson.Str("10")
    }

    test("resolvePath is available") {
      eval("""std.resolvePath("a/b/c.jsonnet", "d.jsonnet")""") ==> ujson.Str("a/b/d.jsonnet")
      eval("""std.resolvePath("c.jsonnet", "d.jsonnet")""") ==> ujson.Str("d.jsonnet")
      eval("""std.resolvePath("/c.jsonnet", "d.jsonnet")""") ==> ujson.Str("/d.jsonnet")
    }

    test("hidden comparison helpers match std.jsonnet") {
      eval(
        """[
          |  std.__compare(1, 2),
          |  std.__compare("b", "a"),
          |  std.__compare([1], [1, 2]),
          |  std.__array_less([1], [2]),
          |  std.__array_greater_or_equal([1, 2], [1]),
          |]
          |""".stripMargin
      ) ==> ujson.Arr(-1, 1, -1, true, true)
      assert(evalErr("""std.__compare(false, false)""").contains("boolean"))
      assert(evalErr("""std.__compare(null, null)""").contains("null"))
    }

    test("minArray and maxArray distinguish explicit false from defaults") {
      eval("""[std.minArray([], onEmpty=false), std.maxArray([], onEmpty=false)]""") ==>
      ujson.Arr(false, false)
      assert(evalErr("""std.minArray([1], keyF=false)""").contains("boolean"))
      assert(evalErr("""std.maxArray([1], keyF=false)""").contains("boolean"))
      assert(evalErr("""std.minArray([true])""").contains("boolean"))
      assert(evalErr("""std.maxArray([null])""").contains("null"))
    }

    test("set keyF defaults do not collide with explicit false") {
      eval(
        """{
          |  sortOne: std.sort([1], keyF=false),
          |  uniqOne: std.uniq([1], keyF=false),
          |  setOne: std.set([1], keyF=false),
          |  setUnionEmpty: std.setUnion([], [1], keyF=false),
          |  setInterEmpty: std.setInter([], [1], keyF=false),
          |  setDiffEmpty: std.setDiff([], [1], keyF=false),
          |  setMemberEmpty: std.setMember(1, [], keyF=false),
          |}
          |""".stripMargin
      ) ==> ujson.Obj(
        "sortOne" -> ujson.Arr(1),
        "uniqOne" -> ujson.Arr(1),
        "setOne" -> ujson.Arr(1),
        "setUnionEmpty" -> ujson.Arr(1),
        "setInterEmpty" -> ujson.Arr(),
        "setDiffEmpty" -> ujson.Arr(),
        "setMemberEmpty" -> ujson.False
      )
      assert(evalErr("""std.sort([2, 1], keyF=false)""").contains("boolean"))
      assert(evalErr("""std.uniq([1, 1], keyF=false)""").contains("boolean"))
      assert(evalErr("""std.setUnion([1], [2], keyF=false)""").contains("boolean"))
      assert(evalErr("""std.setMember(1, [1], keyF=false)""").contains("boolean"))
    }
  }
}
