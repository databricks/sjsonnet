package sjsonnet

import utest._
import TestUtils.{eval, evalErr}
object Std0150FunctionsTests extends TestSuite {

  def tests: Tests = Tests {
    test("stdClamp") {
      eval("std.clamp(-3, 0, 5)") ==> ujson.Num(0)
      eval("std.clamp(4, 0, 5)") ==> ujson.Num(4)
      eval("std.clamp(7, 0, 5)") ==> ujson.Num(5)
    }
    test("member") {
      eval("std.member('foo', 'o')") ==> ujson.True
      eval("std.member('foo', 'f')") ==> ujson.True
      eval("std.member('foo', 'x')") ==> ujson.False
      eval("std.member([], 'o')") ==> ujson.False
      eval("std.member(['f'], 'o')") ==> ujson.False
      eval("std.member(['f', 'o', 'o'], 'o')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'f')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'g')") ==> ujson.False

      eval("std.member([1, 2, 3], 1)") ==> ujson.True
      eval("std.member([1, 2, 3], 4)") ==> ujson.False

      eval("std.member([['f', 'o', 'o'], ['b', 'a', 'r']], ['f', 'o', 'o'])") ==> ujson.True
    }
    test("repeat") {
      eval("std.repeat([], 0)") ==> ujson.Arr()
      eval("std.repeat([1], 1)") ==> ujson.Arr(1)
      eval("std.repeat([1, 2], 1)") ==> ujson.Arr(1, 2)
      eval("std.repeat([1], 2)") ==> ujson.Arr(1, 1)
      eval("std.repeat([1, 2], 2)") ==> ujson.Arr(1, 2, 1, 2)
      eval("std.repeat('a', 1)") ==> ujson.Str("a")
      eval("std.repeat('a', 4)") ==> ujson.Str("aaaa")
      eval("std.repeat('ab', 4)") ==> ujson.Str("abababab")
      eval("std.repeat('a', 0)") ==> ujson.Str("")
    }

    test("join") {
      eval("std.join(' ', ['', 'foo'])") ==> ujson.Str(" foo")
      eval("std.join(' ', [null, 'foo'])") ==> ujson.Str("foo")
    }

    test("slice") {
      eval("std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1)") ==> ujson.read("[ 1, 2, 3, 4 ]")
      eval("std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2)") ==> ujson.read("[ 2, 4, 6 ]")
      eval("""std.slice("jsonnet", 0, 4, 1)""") ==> ujson.Str("json")
    }

    test("manifestJsonMinified") {
      eval(
        """std.manifestJsonMinified( { x: [1, 2, 3, true, false, null, "string\nstring", []], y: { a: 1, b: 2, c: [1, 2], d: {} }, })"""
      ) ==>
      ujson.Str(
        "{\"x\":[1,2,3,true,false,null,\"string\\nstring\",[]],\"y\":{\"a\":1,\"b\":2,\"c\":[1,2],\"d\":{}}}"
      )
    }

    test("manifestXmlJsonml") {
      eval(
        """std.manifestXmlJsonml([
          |  'svg', { height: 100, width: 100 },
          |  [
          |    'circle', {
          |      cx: 50, cy: 50, r: 40,
          |       stroke: 'black', 'stroke-width': 3,
          |       fill: 'red',
          |    }
          |  ],
          |])
          |""".stripMargin
      ) ==>
      ujson.Str(
        """<svg height="100" width="100"><circle cx="50" cy="50" fill="red" r="40" stroke="black" stroke-width="3"></circle></svg>""".stripMargin
      )
    }

    test("extVars") {
      val interpreter = new Interpreter(
        Map(
          "num" -> "1",
          "str" -> "\"hello\"",
          "bool" -> "true",
          "jsonArrNums" -> """[1, 2, 3]""",
          "jsonObjBools" -> """{"hello": false}""",
          "code" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)""",
          "std" -> """std.length("hello")""",
          "stdExtVar" -> """std.extVar("std") + 10""",
          "stdExtVarRecursive" -> """std.extVar("stdExtVar") + 100"""
        ),
        Map(),
        DummyPath(),
        Importer.empty,
        parseCache = new DefaultParseCache
      )

      def check(s: String, expected: ujson.Value): Unit =
        interpreter.interpret(s, DummyPath("(memory)")) ==> Right(expected)

      check("""std.extVar("num")""", 1)
      check("""std.extVar("str")""", "hello")
      check("""std.extVar("bool")""", ujson.True)
      check("""std.extVar("jsonArrNums")""", ujson.Arr(1, 2, 3))
      check("""std.extVar("jsonObjBools")""", ujson.Obj("hello" -> false))
      check("""std.extVar("code")""", ujson.Obj("x" -> 1, "y" -> 2))
      check("""std.extVar("std")""", 5)
      check("""std.extVar("stdExtVar")""", 15)
      check("""std.extVar("stdExtVarRecursive")""", 115)
    }

    test("tlaVars") {
      val interpreter = new Interpreter(
        Map(),
        Map(
          "num" -> "1",
          "str" -> "\"hello\"",
          "bool" -> "true",
          "jsonArrNums" -> """[1, 2, 3]""",
          "jsonObjBools" -> """{"hello": false}""",
          "code" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)""",
          "std" -> """std.length("hello")"""
        ),
        DummyPath(),
        new Importer {
          override def resolve(docBase: Path, importName: String): Option[Path] = importName match {
            case "bar.json" => Some(DummyPath("bar"))
            case _          => None
          }
          override def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = path match {
            case DummyPath("bar") => Some(StaticResolvedFile("""{"x": "y"}"""))
            case _                => None
          }
        },
        parseCache = new DefaultParseCache
      )

      def check(s: String, expected: ujson.Value): Unit =
        interpreter.interpret(s, DummyPath("(memory)")) ==> Right(expected)

      check("""function(num) num""", 1)
      check("""function(str) str""", "hello")
      check("""function(bool) bool""", ujson.True)
      check("""function(jsonArrNums) jsonArrNums""", ujson.Arr(1, 2, 3))
      check("""function(jsonObjBools) jsonObjBools""", ujson.Obj("hello" -> false))
      check("""function(code) code""", ujson.Obj("x" -> 1, "y" -> 2))
      check("""function(std) std""", 5)
      // Make sure top-level-vars which use their in-code defined default
      // values work
      //
      // For some reason, this needs to go through the import codepath to
      // trigger the failure if it's not properly implemented. Not sure why
      check("""local foo = import "bar.json"; function(qux = foo) qux""", ujson.Obj("x" -> "y"))
    }

    test("fold") {
      eval("""std.foldr(function (acc, it) acc + " " + it, "jsonnet", "this is")""") ==>
      ujson.Str("j s o n n e t this is")
      eval("std.foldr(function(v, i) i + v + v, 'bcd', 'a')") ==> ujson.Str("addccbb")


      eval("""std.foldl(function (acc, it) acc + " " + it, "jsonnet", "this is")""") ==>
      ujson.Str("this is j s o n n e t")
    }
    test("reverse") {
      eval("""std.reverse([])""") ==> ujson.Arr()
      eval("""std.reverse([1])""") ==> ujson.Arr(1)
      eval("""std.reverse(["1", true, null])""") ==> ujson.Arr(ujson.Null, true, "1")
    }

    test("get") {
      eval("""std.get({a: 1}, "a")""") ==> ujson.Num(1)
      eval("""std.get({a:: 1}, "a")""") ==> ujson.Num(1)
      eval("""std.get({a: 1}, "b")""") ==> ujson.Null
      eval("""std.get({a: 1}, "b", default=2)""") ==> ujson.Num(2)
      eval("""std.get({a:: 1}, "a", inc_hidden=false)""") ==> ujson.Null
    }

    test("any") {
      eval("""std.any([])""") ==> ujson.Bool(false)
      eval("""std.any([true, true, true])""") ==> ujson.Bool(true)
      eval("""std.any([false, true, false])""") ==> ujson.Bool(true)
      eval("""std.any([false, false, false])""") ==> ujson.Bool(false)
    }

    test("all") {
      eval("""std.all([])""") ==> ujson.Bool(true)
      eval("""std.all([true, true, true])""") ==> ujson.Bool(true)
      eval("""std.all([false, true, false])""") ==> ujson.Bool(false)
      eval("""std.all([false, false, false])""") ==> ujson.Bool(false)
    }

    test("isEmpty") {
      eval("""std.isEmpty("")""") ==> ujson.Bool(true)
      eval("""std.isEmpty("non-empty string")""") ==> ujson.Bool(false)
      assert(
        evalErr("""std.isEmpty(10)""")
          .startsWith("sjsonnet.Error: Wrong parameter type: expected String, got number")
      )
    }

    test("trim") {
      eval("""std.trim("already trimmed string")""") ==> ujson.Str("already trimmed string")
      eval("""std.trim("    string with spaces on both ends     ")""") ==> ujson.Str(
        "string with spaces on both ends"
      )
      eval("""std.trim("string with newline character at end\n")""") ==> ujson.Str(
        "string with newline character at end"
      )
      eval("""std.trim("string with tabs at end\t\t")""") ==> ujson.Str("string with tabs at end")
      assert(
        evalErr("""std.trim(10)""").startsWith(
          "sjsonnet.Error: Wrong parameter type: expected String, got number"
        )
      )
    }

    test("xnor") {
      eval("""std.xnor(false, true)""") ==> ujson.False
      eval("""std.xnor(false, false)""") ==> ujson.True
      assert(
        evalErr("""std.xnor("false", false)""")
          .startsWith("sjsonnet.Error: Wrong parameter type: expected Boolean, got string")
      )
    }

    test("xor") {
      eval("""std.xor(false, true)""") ==> ujson.True
      eval("""std.xor(true, true)""") ==> ujson.False
      assert(
        evalErr("""std.xor("false", false)""")
          .startsWith("sjsonnet.Error: Wrong parameter type: expected Boolean, got string")
      )
    }

    test("equalsIgnoreCase") {
      eval("""std.equalsIgnoreCase("hello", "HELLO")""") ==> ujson.True
      eval("""std.equalsIgnoreCase("hello", "world")""") ==> ujson.False
    }

    test("get") {
      eval("""std.get({a: 1}, "a", error "a")""") ==> ujson.Num(1)
    }
  }
}
