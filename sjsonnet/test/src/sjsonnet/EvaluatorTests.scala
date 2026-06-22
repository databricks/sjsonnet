package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object EvaluatorTests extends TestSuite {
  private def evalWithTraces(s: String): (ujson.Value, Vector[String]) = {
    var traces = Vector.empty[String]
    val interpreter = new Interpreter(
      Map(),
      Map(),
      DummyPath(),
      Importer.empty,
      parseCache = new DefaultParseCache,
      logger = (isTrace, msg) => if (isTrace) traces :+= msg
    )
    val result = interpreter.interpret(s, DummyPath("(memory)")) match {
      case Right(value) => value
      case Left(err)    => throw new Exception(err)
    }
    (result, traces)
  }

  def tests: Tests = Tests {
    test("arithmetic") {
      eval("1 + 2 + 3") ==> ujson.Num(6)
      eval("1 + 2 * 3") ==> ujson.Num(7)
      eval("-1 + 2 * 3") ==> ujson.Num(5)
      eval("6 - 3 + 2") ==> ujson.Num(5)
    }
    test("divisionByZeroErrorMessage") {
      // go-jsonnet uses "Division by zero." (capitalized, with period) for both / and %
      assert(evalErr("1 / 0").contains("Division by zero."))
      assert(evalErr("1.0 / 0.0").contains("Division by zero."))
      assert(evalErr("1 % 0").contains("Division by zero."))
      assert(evalErr("1.0 % 0.0").contains("Division by zero."))
      // Ensure the old lowercase format is not used
      assert(!evalErr("1 / 0").contains("division by zero"))
    }
    test("objects") {
      eval("{x: 1}.x") ==> ujson.Num(1)
      eval("std.objectKeysValues({a: error 'unused'})[0].key") ==> ujson.Str("a")
      assert(evalErr("std.objectKeysValues({a: error 'boom'})[0].value").contains("boom"))
    }
    test("arrays") {
      eval("[1, [2, 3], 4][1][0]") ==> ujson.Num(2)
      eval("([1, 2, 3] + [4, 5, 6])[3]") ==> ujson.Num(4)
      evalErr("[][0]") ==>
      """sjsonnet.Error: Array bounds error: array is empty
        |at [<root>].(:1:3)""".stripMargin
      eval("std.slice(std.range(1,4), 0, null, 2)") ==> ujson
        .Arr(1, 3)
      eval("std.slice(std.range(1,4), null, null, 2)") ==> ujson
        .Arr(1, 3)
      eval(
        "std.slice(std.range(1,4), null, null, null)"
      ) ==> ujson.Arr(1, 2, 3, 4)
      eval("""
             |std.slice("jsonnet", -3, null, null)
             |""".stripMargin) ==> ujson.Str("net")
      eval("std.range(1,4)[0::2]") ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[0:null:2]") ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[null:null:2]") ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[null:null:null]") ==> ujson.Arr(
        1,
        2,
        3,
        4
      )
      eval("std.range(1,4)[::2]") ==> ujson.Arr(1, 3)
    }
    test("functions") {
      eval("(function(x) x)(1)") ==> ujson.Num(1)
      eval("function() 1") ==> ujson.Num(1)
      eval("function(a=1) a") ==> ujson.Num(1)
      eval("(function(x, y = x + 1) y)(x = 10)") ==> ujson.Num(
        11
      )
      eval("local f(x) = function() true; f(42)") ==> ujson.True
      eval(
        "local f(x) = function() true; f(42) == true"
      ) ==> ujson.False
      eval(
        "local f(x) = function() true; f(42)() == true"
      ) ==> ujson.True
      assert(
        evalErr("{foo: function() true}").startsWith(
          "sjsonnet.Error: Couldn't manifest function with params"
        )
      )
      eval("{foo: (function() true)()}") ==> ujson.Obj {
        "foo" -> ujson.True
      }
      eval(
        """
          |local f2(f) = function(x) f(f(x));
          |local g = f2(error "should stay lazy");
          |std.type(g)
          |""".stripMargin
      ) ==> ujson.Str("function")
      assert(
        evalErr(
          """
            |local f2(f) = function(x) f(f(x));
            |local g = f2(error "call should force base");
            |g(1)
            |""".stripMargin
        ).contains("call should force base")
      )
      assert(
        evalErr(
          """
            |local f2(f) = function(x) f(f(x));
            |f2(1)(1)
            |""".stripMargin
        ).contains("Expected function, found number")
      )
      assert(
        evalErr(
          """
            |local f2(f) = function(x) f(f(x));
            |f2(error "tailstrict should force") tailstrict
            |""".stripMargin
        ).contains("tailstrict should force")
      )
      eval(
        """
          |local f2(f) = function(x) f(f(x));
          |f2(function(x) x + 1)(1)
          |""".stripMargin
      ) ==> ujson.Num(3)
      eval(
        """
          |local f2(f) = function(x) f(f(x));
          |local plus1(x) = x + 1;
          |local chain = std.makeArray(5, function(i) if i == 0 then plus1 else f2(chain[i - 1]));
          |chain[4](1)
          |""".stripMargin,
        maxStack = 100000
      ) ==> ujson.Num(17)
      eval(
        """
          |local f2(f) = function(x) f(f(x));
          |local id(x) = x;
          |local slowId = std.makeArray(20, function(i) if i == 0 then id else f2(slowId[i - 1]));
          |slowId[15](42)
          |""".stripMargin,
        maxStack = 100000
      ) ==> ujson.Num(42)
      assert(
        evalErr(
          """
            |local f2(f) = function(x) f(f(x));
            |local rec = f2(rec);
            |rec(1)
            |""".stripMargin
        ).contains("Max stack frames exceeded")
      )
      assert(
        evalErr(
          """
            |local f2(f) = function(x) f(f(x));
            |local o = { a: f2(self.b), b: f2(self.a) };
            |o.a(1)
            |""".stripMargin
        ).contains("Max stack frames exceeded")
      )
    }
    test("members") {
      eval("{local x = 1, x: x}['x']") ==> ujson.Num(1)
      eval("{local x(y) = y + '1', x: x('2')}['x']") ==> ujson
        .Str("21")
      eval("{local x(y) = y + '1', x: x(y='2')}['x']") ==> ujson
        .Str("21")
      eval("{local x(y='2') = y + '1', x: x()}['x']") ==> ujson
        .Str("21")
      eval(
        """{[{local x = $.y + "lol", y: x, z: "1"}.z]: 2}["1"]"""
      ) ==> ujson.Num(2)
      eval("{local x = 1, y: { z: x }}.y.z") ==> ujson.Num(1)
    }
    test("extends") {
      eval("(function(a) a.x + a.y)({x: 1}{y: 2})") ==> ujson
        .Num(3)
      eval(
        "(function(a) a.x + a.y)({x: 1}{x: 2, y: 3})"
      ) ==> ujson.Num(5)
      eval("({x: 1}{x+: 2}).x") ==> ujson.Num(3)
      eval("({x+: 1}{x: 2}).x") ==> ujson.Num(2)
      eval("({x+: 1}{x+: 2}).x") ==> ujson.Num(3)
      eval("({x+: 1} + {x+: 2}).x") ==> ujson.Num(3)
      eval(
        "(function(a, b) a + b)({x+: 1}, {x+: 2}).x"
      ) ==> ujson.Num(3)
      eval("""({a: [1]} + {a+: "1"}).a""") ==> ujson.Str("[1]1")
      eval("""({a: 1} + {a+: "1"}).a""") ==> ujson.Str("11")
      eval("""({a: "1"} + {a+: 1}).a""") ==> ujson.Str("11")
    }
    test("ifElse") {
      eval("if true then 1 else 0") ==> ujson.Num(1)
      eval("if 2 > 1 then 1 else 0") ==> ujson.Num(1)
      eval("if false then 1 else 0") ==> ujson.Num(0)
      eval("if 1 > 2 then 1 else 0") ==> ujson.Num(0)
    }
    test("self") {
      eval("{x: 1, y: $.x + 10}.y") ==> ujson.Num(11)
      eval("{x: 1, y: self.x}.y") ==> ujson.Num(1)
      eval("{x: 1, y: {x: 2, z: $.x + 10}}.y.z") ==> ujson.Num(
        11
      )
      eval("{x: 1, y: {x: 2, z: self.x + 10}}.y.z") ==> ujson
        .Num(12)
      eval("{x: 1, y: {x: 0, y: self.x}.y}.y") ==> ujson.Num(0)
      eval("""{x: 1, y: "x" in self}.y""") ==> ujson.True
      eval("""{x: 1, y: "z" in self}.y""") ==> ujson.False
      eval("""{y: "y" in self}.y""") ==> ujson.True
    }
    test("topLevel") {
      eval("local p(n='A') = {w: 'H'+n}; {p: p()}.p.w") ==> ujson
        .Str("HA")
    }
    test("lazy") {
      eval("[{x: $.y, y: $.x}.x, 2][1]") ==> ujson.Num(2)
      eval(
        "{x: $.y, y: $.x, local z(z0) = [3], w: z($.x)}.w[0]"
      ) ==> ujson.Num(3)
      eval(
        "(function(a=[1, b[1]], b=[a[0], 2]) [a, b])()[0][1]"
      ) ==> ujson.Num(2)
    }
    test("comprehensions") {
      eval("[x + 1 for x in [1, 2, 3]][2]") ==> ujson.Num(4)
      eval("[x + 1, for x in [1, 2, 3]][2]") ==> ujson.Num(4)
      eval(
        "[x + y for x in [1, 2, 3] for y in [4, 5, 6] if x + y != 7][3]"
      ) ==> ujson.Num(8)
      eval(
        """{[""+x]: x * x for x in [1, 2, 3]}["3"]"""
      ) ==> ujson.Num(9)
      eval(
        """{local y = $["2"], [x]: if x == "1" then y else 0, for x in ["1", "2"]}["1"]"""
      ) ==> ujson.Num(0)
      // References between locals in an object comprehension:
      eval(
        """{local a = 1, local b = a + 1, [k]: b + 1 for k in ["x"]}"""
      ) ==> ujson.Obj("x" -> ujson.Num(3))
      // Locals which reference variables from the comprehension:
      eval(
        """{local x2 = k*2, [std.toString(k)]: x2 for k in [1]}"""
      ) ==> ujson.Obj("1" -> ujson.Num(2))
      // Regression test for https://github.com/databricks/sjsonnet/issues/357
      // self references in object comprehension locals are properly rebound during inheritance:
      eval(
        """
          |local lib = {
          |  foo()::
          |    {
          |      local global = self,
          |
          |      [iterParam]: global.base {
          |        foo: iterParam
          |      }
          |      for iterParam in ["foo"]
          |    },
          |};
          |
          |{
          | base:: {}
          |}
          |+ lib.foo()
          |""".stripMargin
      ) ==> ujson.Obj("foo" -> ujson.Obj("foo" -> "foo"))
      // Regression test for a related bug involving local references to `super`:
      eval(
        """
          |local lib = {
          |  foo():: {
          |    local sx = super.x,
          |    [k]: sx + 1
          |    for k in ["x"]
          |  },
          |};
          |
          |{ x: 2 }
          |+ lib.foo()
          |""".stripMargin
      ) ==> ujson.Obj("x" -> ujson.Num(3))
      // Yet another related bug involving super references _not_ in locals:
      eval(
        """
          |local lib = {
          |  foo():: {
          |    [k]: super.x + 1
          |    for k in ["x"]
          |  },
          |};
          |
          |{ x: 2 }
          |+ lib.foo()
          |""".stripMargin
      ) ==> ujson.Obj("x" -> ujson.Num(3))
      // Regression test for a bug in handling of non-string field names:
      evalErr("{[k]: k for k in [1]}") ==>
      """sjsonnet.Error: [object comprehension] Field name must be string or null, not number
          |at [<root>].(:1:1)""".stripMargin
      // Basic function support:
      eval(
        """
          |local funcs = {
          |  [a](x): x * 2
          |  for a in ["f1", "f2", "f3"]
          |};
          |funcs.f1(10)
          |""".stripMargin
      ) ==> ujson.Num(20)
      // Functions which use locals from the comprehension:
      eval(
        """
          |local funcs = {
          |  local y = b,
          |  [a](x): x * y
          |  for a in ["f1", "f2", "f3"] for b in [2]
          |};
          |funcs.f1(10)
          |""".stripMargin
      ) ==> ujson.Num(20)
    }
    test("super") {
      test("implicit") {

        eval("({ x: 1, y: self.x } + { x: 2 }).y") ==> ujson.Num(
          2
        )
        eval(
          "({ local x = $.y, y: 1, z: x} + { y: 2 }).z"
        ) ==> ujson.Num(2)
        eval(
          "({ local x = self.y, y: 1, z: x} + { y: 2 }).z"
        ) ==> ujson.Num(2)
        eval(
          "local A = {x: 1, local outer = self, y: A{z: outer}}; A.y.z.x"
        ) ==> ujson.Num(1)
        eval(
          "{local x = self, y: 1, z: {a: x, y: 2}}.z.a.y"
        ) ==> ujson.Num(1)
        eval(
          "local A = {x: 1, local outer = self, y: A{x: outer.x}}; A.y.x"
        ) ==> ujson.Num(1)
        eval(
          "local A = {x: 1, local outer = self, y: A{x: outer.x + 1}}; A.y.y.x"
        ) ==> ujson.Num(3)
        eval("""("a" in ({a: 1}{b: 2}))""") ==> ujson.True
      }
      test("explicit") {

        eval("{ x: 1, y: self.x } + { x: 2, y: super.y + 1}") ==>
        ujson.read("""{ "x": 2, "y": 3 }""")

        eval(
          "{ x: 1 } + { x: 2, y: super.x } + { x: 3, z: super.x }"
        ) ==>
        ujson.read("""{ "x": 3, "y": 1, "z": 2 }""")

        eval(
          """({a: 1} + {b: 2} + {c: ["a" in super, "b" in super]}).c"""
        ) ==>
        ujson.Arr(true, true)

        eval("""{a: ["a" in super, "b" in super]}.a""") ==> ujson
          .Arr(false, false)

        eval("""(({a: 1}{b: 2}){c: super.a}).c""") ==> ujson.Num(
          1
        )
        eval("""(({a: 1}{b: 2}){c: super.b}).c""") ==> ujson.Num(
          2
        )

        eval(
          """(({a: 1}{b: 2, f: super.a}){c: super.f}).c"""
        ) ==> ujson.Num(1)
        val ex = assertThrows[Exception] {
          eval("""(({a: 1}{b: 2, f: super.b}){c: super.f}).c""")
        }
        assert(ex.getMessage.contains("Field does not exist: b"))

        eval(
          """(({a: 1}{b: 2}) + ({c: super.b}{d: super.a})).c"""
        ) ==> ujson.Num(2)
        eval(
          """(({a: 1}{b: 2}) + ({c: super.b}{d: super.a})).d"""
        ) ==> ujson.Num(1)

        eval(
          """local x = {a: 1}; local y = {b: super.a}; x + y"""
        ) ==> ujson.read("""{"a": 1, "b": 1}""")

        eval(
          """local x = { a: 1, b: { c: 2 }}; x { a: super.a * 10, b:: { c: super.b.c * 10 } }"""
        ) ==>
        ujson.Obj("a" -> ujson.Num(10))
        evalErr(
          """local x = { a: 1, b: { c: 2 }}; x { a: super.a * 10, b:: { c: super.b.c * 10 } }.b"""
        ) ==>
        """sjsonnet.Error: Attempt to use `super` when there is no super class
          |at [<root>].(:1:7)""".stripMargin
      }
    }
    test("hidden") {
      eval("{i::1 }") ==>
      ujson.read("""{}""")

      eval("{i:: 1} + {i: 2}") ==>
      ujson.read("""{}""")

      eval("{i: 1} + {i:: 2}") ==>
      ujson.read("""{}""")

      eval("{i:: 1} + {i:: 2}") ==>
      ujson.read("""{}""")

      eval("{i:::1} + {i::2}") ==>
      ujson.read("""{}""")

      eval("{i::1} + {i:::2}") ==>
      ujson.read("""{"i": 2}""")

      eval("{i:1} + {i:::2}") ==>
      ujson.read("""{"i": 2}""")

      eval("local M = {x+: self.i, i :: 1}; { x: 1 } + M") ==>
      ujson.read("""{ "x": 2 }""")

      eval("""("%(hello)s" % {hello::"world"})""") ==> ujson.Str(
        "world"
      )

      eval(
        """("%(hello)s" % {hello::"world", bad:: error "lol"})"""
      ) ==> ujson.Str("world")
    }
    test("objectEqualityIgnoresHiddenFields") {
      // Hidden field in y should not be matched during equality comparison.
      // C++ jsonnet and jrsonnet both return false here.
      eval("{a: 1} == {a:: 1, b: 2}") ==> ujson.False
      // Different visible key counts: x has {a, b}, y has only {a} visible.
      eval("{a: 1, b: 2} == {a: 1, b:: 2}") ==> ujson.False
      // x has no visible keys, y has {a} visible.
      eval("{a:: 1} == {a: 1}") ==> ujson.False
      // Hidden field in y does not affect equality when visible keys match.
      eval("{a: 1} == {a: 1, b:: 2}") ==> ujson.True
      // Both sides have the same visible key with same value; hidden fields ignored.
      eval("{a: 1, b:: 3} == {a: 1, c:: 4}") ==> ujson.True
    }
    test("evaluator2") {
      eval(
        """{local x = 1, [x]: x, for x in ["foo"]}.foo"""
      ) ==> ujson.Num(1)
      eval(
        """{[x]: x, local x = 1, for x in ["foo"]}.foo"""
      ) ==> ujson.Num(1)
      eval(
        """local foo = ["foo"]; {local foo = 1, [x]: x, for x in foo}.foo"""
      ) ==> ujson.Str("foo")
      eval(
        """local foo = ["foo"]; {[x]: x, local foo = 2, for x in foo}.foo"""
      ) ==> ujson.Str("foo")

      eval(
        """{ [x + ""]: if x == 1 then 1 else x + $["1"] for x in [1, 2, 3] }"""
      ) ==>
      ujson.read("""{ "1": 1, "2": 3, "3": 4 }""")

      eval(
        """local x = "baz"; { local x = "bar", [x]: x for x in ["foo"] }"""
      ) ==>
      ujson.read("""{ "foo": "bar" }""")

      eval(
        """{ [x + ""]: x + foo, local foo = 3 for x in [1, 2, 3] }"""
      ) ==>
      ujson.read("""{ "1": 4, "2": 5, "3": 6 }""")
      eval(
        """{local y = x, ["foo"]: y, for x in ["foo"]}.foo"""
      ) ==> ujson.Str("foo")
    }
    test("shadowing") {
      eval("local x = 1; local x = 2; x") ==> ujson.Num(2)
      eval("local x = 1; x + local x = 2; x") ==> ujson.Num(3)
      eval(
        """local str1 = |||
          |        text
          |    |||;
          |
          |local str1 = |||
          |        \n
          |    |||;
          |
          |(str1 == "\\n\n")
          |""".stripMargin
      ) ==> ujson.True
    }
    test("stdLib") {
      eval("std.pow(2, 3)") ==> ujson.Num(8)
      eval("std.pow(x=2, n=3)") ==> ujson.Num(8)
      eval("std.pow(n=3, x=2)") ==> ujson.Num(8)
      eval("({a:: 1} + {a+:::2}).a") ==> ujson.Num(3)
      eval("(std.prune({a:: 1}) + {a+:::2}).a") ==> ujson.Num(2)
      eval(
        "std.toString(std.mapWithIndex(function(idx, elem) elem, [2,1,0]))"
      ) ==> ujson.Str("[2, 1, 0]")
    }
    test("unboundParam") {
      val ex = assertThrows[Exception] {
        eval(
          """local newParams(x, y) = {
            |  x: x,
            |  y: y,
            |};
            |
            |local params = newParams("a");
            |
            |params.y
            |
        """.stripMargin
        )
      }

      assert(ex.getMessage.contains("Parameter y not bound in call"))
    }

    test("invalidParam") {
      val ex = assertThrows[Exception] {
        eval(
          """local Person(name='Alice') = {
            |  name: name,
            |  welcome: 'Hello ' + name + '!',
            |};
            |{
            |  person2: Person('Bob', hello=123),
            |}
        """.stripMargin
        )
      }

      assert(ex.getMessage.contains("Has no parameter hello"))
    }

    test("unknownVariable") {
      evalErr("x") ==>
      """sjsonnet.StaticError: Unknown variable: x
        |at [<root>].(:1:1)""".stripMargin
      evalErr("self.x") ==>
      """sjsonnet.StaticError: Can't use self outside of an object
        |at [<root>].(:1:1)""".stripMargin
      evalErr("$.x") ==>
      """sjsonnet.StaticError: Can't use $ outside of an object
        |at [<root>].(:1:1)""".stripMargin
      evalErr("super.x") ==>
      """sjsonnet.StaticError: Can't use super outside of an object
        |at [<root>].(:1:1)""".stripMargin
    }

    test("validParam") {
      val res = eval(
        """local Person(name='Alice') = {
          |  name: name,
          |  welcome: 'Hello ' + name + '!',
          |};
          |{
          |  person2: Person(name='Bob'),
          |}.person2.welcome
        """.stripMargin
      )

      res ==> ujson.Str("Hello Bob!")
    }

    test("equalDollar") {
      eval(
        "local f(x) = x; {hello: 123, world: f(x=$.hello)}"
      ) ==>
      ujson.Obj("hello" -> 123, "world" -> 123)
    }
    test("stdSubstr") {
      eval("std.substr('cookie', 6, 2)") ==> ujson.Str("")
    }
    test("manifestIni") {
      eval(
        """std.manifestIni({
          |  main: { a: "1", b: 2, c: true, d: null, e: [1, {"2": 2}, [3]], f: {"hello": "world"} },
          |  sections: {}
          |})""".stripMargin
      ) ==>
      ujson.Str(
        "a = 1\nb = 2\nc = true\nd = null\ne = 1\ne = {\"2\": 2}\ne = [3]\nf = {\"hello\": \"world\"}\n"
      )
    }
    test("format") {
      eval("\"%s\" % \"world\"") ==> ujson.Str("world")
      eval("\"%s\" % [\"world\"]") ==> ujson.Str("world")
      eval("\"%s %s\" % [\"hello\", \"world\"]") ==> ujson.Str(
        "hello world"
      )
      eval("\"%(hello)s\" % {hello: \"world\"}") ==> ujson.Str(
        "world"
      )
      eval("\"%()s %()s!\" % [\"Hello\", \"World\"]") ==> ujson
        .Str("Hello World!")
    }
    test("trace laziness") {
      val (unusedObj, unusedObjTraces) = evalWithTraces(
        """local x = {a: std.trace("unused object", "ok")}; 0"""
      )
      unusedObj ==> ujson.Num(0)
      unusedObjTraces ==> Vector.empty

      val (unusedFormat, unusedFormatTraces) = evalWithTraces(
        """local x = "%% %(a)s %%" % {a: std.trace("unused format", "ok")}; 0"""
      )
      unusedFormat ==> ujson.Num(0)
      unusedFormatTraces ==> Vector.empty

      val (used, usedTraces) = evalWithTraces("""std.trace("used trace", 1)""")
      used ==> ujson.Num(1)
      usedTraces ==> Vector("TRACE: (memory) used trace")
    }
    test("identityFunctionTraces") {
      // Issue #815: the identity-elision fast paths must force the argument exactly as a normal
      // call would — no dropped or duplicated side effects (traces).
      // Direct identity elision: the traced argument is forced exactly once.
      val (idVal, idTraces) = evalWithTraces("""(function(x) x)(std.trace("idtrace", 5))""")
      idVal ==> ujson.Num(5)
      idTraces ==> Vector("TRACE: (memory) idtrace")

      // Self-composition identity (g = id): still forced exactly once.
      val (compVal, compTraces) =
        evalWithTraces(
          """local g = function(x) x; local f = function(x) g(g(x)); f(std.trace("comp", 9))"""
        )
      compVal ==> ujson.Num(9)
      compTraces ==> Vector("TRACE: (memory) comp")

      // Laziness preserved: identity map stays lazy, so std.length does not force the element.
      val (lazyVal, lazyTraces) =
        evalWithTraces("""std.length(std.map(function(x) x, [std.trace("lz", 1), 2]))""")
      lazyVal ==> ujson.Num(2)
      lazyTraces ==> Vector.empty
    }
    test("binaryOps") {
      val ex = assertThrows[Exception](
        eval("1 && 2")
      )
      assert(ex.getMessage.contains("Binary operator && does not operate on numbers."))

      val ex2 = assertThrows[Exception](
        eval("1 || 2")
      )
      assert(ex2.getMessage.contains("Binary operator || does not operate on numbers."))
    }
    test("stdToString") {
      eval("""std.toString({k: "v"})""") ==> ujson.Str(
        """{"k": "v"}"""
      )
    }
    test("floatFormatRegression") {
      eval("'%.4f' % 0.01") ==> ujson.Str("0.0100")
      eval("'%05d' % 2") ==> ujson.Str("00002")
      eval("'%000d' % 2") ==> ujson.Str("2")
      eval("'%000d' % 2.123") ==> ujson.Str("2")
      eval("'%5d' % 2") ==> ujson.Str("    2")
      eval("'%5f' % 2") ==> ujson.Str("2.000000")

      eval("'%10d' % 2.123") ==> ujson.Str("         2")
      eval("'%+5.5f' % 123.456") ==> ujson.Str("+123.45600")
      eval("'%+5.5f' % -123.456") ==> ujson.Str("-123.45600")
      eval("'% 5.5f' % -123.456") ==> ujson.Str("-123.45600")
      eval("'%--+5.5f' % -123.456") ==> ujson.Str("-123.45600")
      eval("'%#-0- + 5.5f' % -123.456") ==> ujson.Str(
        "-123.45600"
      )
    }
    test("formatIntegerOverflow") {
      // %d, %o, %x should not overflow to Long.MAX_VALUE for large doubles
      eval("'%d' % 1e19") ==> ujson.Str("10000000000000000000")
      eval("'%d' % 1e20") ==> ujson.Str(
        "100000000000000000000"
      )
      eval("'%d' % -1e19") ==> ujson.Str(
        "-10000000000000000000"
      )
      eval("'%o' % 1e19") ==> ujson.Str(
        "1053071060221172000000"
      )
      eval("'%x' % 1e19") ==> ujson.Str("8ac7230489e80000")
      // Sign should be determined from the truncated integer, not the original double:
      // -0.3 truncates to 0, so no minus sign should appear.
      eval("'%5.3d' % -0.3") ==> ujson.Str("  000")
      eval("'%d' % -0.9") ==> ujson.Str("0")
    }
    test("formatNullErrors") {
      evalErr(
        "'%d' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%f' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%e' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%g' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%o' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%x' % null"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got null\nat [<root>].(:1:6)"
      evalErr(
        "'%c' % null"
      ) ==> "sjsonnet.Error: [std.format] %c expected number or string, got null\nat [<root>].(:1:6)"
      eval("'%s' % null") ==> ujson.Str("null")
    }
    test("formatTypeErrorMessages") {
      evalErr(
        "'%a' % 42"
      ) ==> "sjsonnet.Error: [std.format] unsupported format conversion at position 0, got number\nat [<root>].(:1:6)"
      evalErr(
        "'%a' % true"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got boolean\nat [<root>].(:1:6)"
      evalErr(
        "'%a' % false"
      ) ==> "sjsonnet.Error: [std.format] expected number or string at position 0, got boolean\nat [<root>].(:1:6)"
    }
    test("strict") {
      eval("({ a: 1 } { b: 2 }).a", strict = false) ==> ujson
        .Num(1)
      evalErr("({ a: 1 } { b: 2 }).a", strict = true) ==>
      """sjsonnet.StaticError: Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects
        |at [<root>].(:1:11)""".stripMargin
      eval(
        "local x = { c: 3 }; (x { a: 1 } { b: 2 }).a",
        strict = false
      ) ==> ujson.Num(1)
      eval(
        "local x = { c: 3 }; (x { a: 1 }).a",
        strict = true
      ) ==> ujson.Num(1)
      evalErr(
        "local x = { c: 3 }; ({ a: 1 } { b: 2 }).a",
        strict = true
      ) ==>
      """sjsonnet.StaticError: Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects
        |at [<root>].(:1:31)""".stripMargin
    }
    test("objectDeclaration") {
      eval("{ ['foo']: x for x in  []}", false) ==> ujson.Obj()
      eval("{ ['foo']: x for x in  [1]}", false) ==> ujson.Obj(
        "foo" -> 1
      )

      eval("{ ['foo']+: x for x in  []}", false) ==> ujson.Obj()
      eval("{ ['foo']+: x for x in  [1]}", false) ==> ujson.Obj(
        "foo" -> 1
      )
      eval(
        "{ ['foo']+: [x] for x in [1]} + { ['foo']+: [x] for x in [2]}",
        false
      ) ==> ujson.Obj("foo" -> ujson.Arr(1, 2))
    }
    test("givenNoDuplicateFieldsInListComprehension1_expectSuccess") {
      eval("""{ ["bar"]: x for x in [-876.89]}""") ==> ujson.Obj(
        "bar" -> -876.89
      )
    }
    test("givenNoDuplicateFieldsInListComprehension2_expectSuccess") {
      eval("""{ ["bar_" + x]: x for x in [5,12]}""") ==> ujson
        .Obj("bar_5" -> 5, "bar_12" -> 12)
    }
    test("givenDuplicateFieldsInListComprehension_expectFailure") {
      evalErr("""{ [x]: x for x in ["A", "A"]}""") ==>
      """sjsonnet.Error: [object comprehension] Duplicate key A in evaluated object comprehension.
        |at [<root>].(:1:1)""".stripMargin
    }
    test("givenDuplicateFieldsInIndirectListComprehension_expectFailure") {
      evalErr(
        """local y = { a: "A" };
          |local z = { a: "A" };
          |{ [x.a]: x for x in [y, z]}""".stripMargin
      ) ==>
      """sjsonnet.Error: [object comprehension] Duplicate key A in evaluated object comprehension.
        |at [<root>].(:1:7)""".stripMargin
    }
    test("functionEqualsNull") {
      eval("""local f(x)=null; f == null""") ==> ujson.False
      eval("""local f=null; f == null""") ==> ujson.True
    }

    test("dynamicDuplicateFields") {
      // Object evaluation should fail if the object contains duplicate field names.
      // This is a regression test for duplicate detection when one or more of the
      // duplicate field names is dynamically computed via a field name expression.

      // Cases where StaticOptimizer replaces the dynamic field names with fixed ones:
      test - (evalErr("""{ ["k"]: 1, ["k"]: 2 }""") ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at [<root>].(:1:1)""".stripMargin)

      test - (evalErr("""{ k: 1, ["k"]: 2 }""") ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at [<root>].(:1:1)""".stripMargin)

      test - (evalErr("""{ ["k"]: 1, k: 2 }""") ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at [<root>].(:1:1)""".stripMargin)

      // Test that lazy evaluation is preserved - duplicate fields should only error when accessed
      test - (eval(
        """{x: { ["k"]: 1, ["k"]: 2 }, y:1 }.y"""
      ) ==> ujson.Num(1))

      // But accessing the problematic field should still error
      test - (evalErr(
        """{x: { ["k"]: 1, ["k"]: 2 }, y:1 }.x"""
      ) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at [<root>].(:1:34)""".stripMargin)

      // Non-StaticOptimizable case:
      test - (evalErr("""{ k: 1, ["k" + ""]: 2 }""") ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at [<root>].(:1:1)""".stripMargin)
    }

    test("identifierStartsWithKeyword") {
      for (keyword <- Parser.keywords) {
        eval(
          s"""local ${keyword}Foo = 123; ${keyword}Foo"""
        ) ==> ujson.Num(123)
        eval(s"""{${keyword}Foo: 123}""") ==> ujson.Obj(
          s"${keyword}Foo" -> ujson.Num(123)
        )
      }
    }

    test("errorNonString") {
      assert(
        evalErr("""error {a: "b"}""").contains("""{"a": "b"}""")
      )
      assert(
        evalErr("""assert 1 == 2 : { a: "b"}; 1""").contains(
          """{"a": "b"}"""
        )
      )
    }

    test("assertInheritance") {
      test - assert(
        evalErr("""{ } + {assert false}""").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{assert false} + {}""").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{assert false} + {} + {}""").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{} + {assert false} + {}""").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{} + {} + {assert false}""").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      // Both own and inherited assertions are evaluated:
      test - assert(
        evalErr("{assert false} + {assert true}").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      // Accessing an object member should trigger computation of that object's own
      // assertions plus any inherited assertions, even if the accessed member happens
      // to be a constant.
      test - assert(
        evalErr("({assert false} + {x: 2}).x").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("({assert false} + {f(x): x}).f(1)").contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - {
        val problematicStrictInheritedAssertionsSnippet =
          """local template = { assert self.flag };
            |{ a: template { flag: true, }, b: template { flag: false, } }
            |""".stripMargin
        assert(
          evalErr(problematicStrictInheritedAssertionsSnippet)
            .contains("sjsonnet.Error: Assertion failed")
        )
      }
    }
    test("assertInheritanceWithBrokenAssertions") {
      test - assert(
        evalErr(
          """{ } + {assert false}""",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{assert false} + {}""",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{assert false} + {} + {}""",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{} + {assert false} + {}""",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{} + {} + {assert false}""",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          "({assert false} + {f(x): x}).f(1)",
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - {
        val problematicStrictInheritedAssertionsSnippet =
          """local template = { assert self.flag };
            |{ a: template { flag: true, }, b: template { flag: false, } }
            |""".stripMargin
        assert(
          evalErr(
            problematicStrictInheritedAssertionsSnippet,
            brokenAssertionLogic = true
          ).contains("sjsonnet.Error: Assertion failed")
        )
      }

      // Behavior is different from now on.
      test {
        eval(
          "{assert false} + {assert true}",
          brokenAssertionLogic = true
        ) ==> ujson.Obj()
        eval(
          "({assert false} + {x: 2}).x",
          brokenAssertionLogic = true
        ) ==> ujson.Num(2)
      }
    }

    test("builtinErrorMessageIncludesFunctionName") {
      // Issue #416: error messages for builtin functions should include the function name.

      // Too many args
      test("tooManyArgs") {
        val ex = assertThrows[Exception] {
          eval("std.length([1], [2])")
        }
        assert(ex.getMessage.contains("Too many args, has"))
      }

      // Parameter not bound in call (missing required arg)
      test("missingRequiredArg") {
        val ex = assertThrows[Exception] {
          eval("std.substr('hello')")
        }
        assert(ex.getMessage.contains("Parameter"))
        assert(ex.getMessage.contains("not bound in call"))
      }

      // Function has no parameter (invalid named arg)
      test("invalidNamedArg") {
        val ex = assertThrows[Exception] {
          eval("std.length(x=[1], noSuchParam=2)")
        }
        assert(ex.getMessage.contains("Has no parameter noSuchParam"))
      }

      // Binding parameter a second time
      test("duplicateArg") {
        val ex = assertThrows[Exception] {
          eval("std.pow(2, x=3)")
        }
        assert(ex.getMessage.contains("Binding parameter a second time: x in function pow"))
      }

      // User-defined function should include function name from local binding
      test("userDefinedFunctionIncludesFunctionName") {
        val ex = assertThrows[Exception] {
          eval(
            """local myFunc(x, y) = x + y;
              |myFunc("a")
            """.stripMargin
          )
        }
        assert(ex.getMessage.contains("Parameter y not bound in call"))
      }

      // User-defined function: too many args should include function name
      test("userDefinedTooManyArgs") {
        val ex = assertThrows[Exception] {
          eval(
            """local add(x, y) = x + y;
              |add(1, 2, 3)
            """.stripMargin
          )
        }
        assert(ex.getMessage.contains("Too many args, has 2 parameter(s)"))
      }

      // Anonymous function should NOT include function name in error message
      test("anonymousFunctionNoFunctionName") {
        val ex = assertThrows[Exception] {
          eval("(function(x, y) x + y)('a')")
        }
        assert(ex.getMessage.contains("Parameter y not bound in call"))
      }

      // Anonymous function: too many args should NOT include function name
      test("anonymousFunctionTooManyArgs") {
        val ex = assertThrows[Exception] {
          eval("(function(x) x)(1, 2)")
        }
        assert(ex.getMessage.contains("Too many args, has 1 parameter(s)"))
      }
    }

    test("maxStack") {
      test("recursiveFunction") {
        val err = evalErr(
          "local f(x) = f(x + 1); f(0)",
          maxStack = 10
        )
        assert(err.contains("Max stack frames exceeded."))
      }
      test("deepButWithinLimit") {
        eval(
          "local f(x) = if x <= 0 then 0 else f(x - 1); f(5)",
          maxStack = 10
        ) ==> ujson.Num(0)
      }
      test("mutualRecursion") {
        val err = evalErr(
          "local a(x) = b(x + 1), b(x) = a(x + 1); a(0)",
          maxStack = 10
        )
        assert(err.contains("Max stack frames exceeded."))
      }
      test("builtinCallsCounted") {
        eval(
          "std.length([1, 2, 3])",
          maxStack = 5
        ) ==> ujson.Num(3)
      }
    }
    test("negativeZeroComparison") {
      // IEEE 754: -0.0 == 0.0, so neither is less than or greater than the other.
      eval("[-0.0] < [0]") ==> ujson.False
      eval("[-0.0] > [0]") ==> ujson.False
      eval("[-0.0] <= [0]") ==> ujson.True
      eval("[-0.0] >= [0]") ==> ujson.True
      eval("[-0.0] == [0]") ==> ujson.True
    }

    test("negativeZeroMaterialization") {
      // std.manifestJson(-0.0) should output "-0", not "0"
      // All reference implementations (C++ jsonnet, go-jsonnet, jrsonnet) output "-0"
      eval("""std.manifestJson(-0.0)""") ==> ujson.Str("-0")
      eval("""std.manifestJsonMinified(-0.0)""") ==> ujson.Str("-0")
      eval("""std.manifestJsonEx(-0.0, "  ")""") ==> ujson.Str("-0")
      eval("""std.toString(-0.0)""") ==> ujson.Str("-0")
      eval("""std.manifestYamlDoc(-0.0)""") ==> ujson.Str("-0")
      // -0.0 inside objects
      eval("""std.manifestJson({a: -0.0})""") ==> ujson.Str("{\n    \"a\": -0\n}")
      // -0.0 inside arrays
      eval("""std.manifestJson([-0.0])""") ==> ujson.Str("[\n    -0\n]")
      // +0.0 should still output "0"
      eval("""std.manifestJson(0.0)""") ==> ujson.Str("0")
      eval("""std.toString(0.0)""") ==> ujson.Str("0")
    }

    test("manifestYamlParameterErrors") {
      evalErr(
        """std.manifestYamlDoc({}, indent_array_in_object="yes")"""
      ).contains("indent_array_in_object has to be a boolean, got string")
      evalErr(
        """std.manifestYamlDoc({}, quote_keys=42)"""
      ).contains("quote_keys has to be a boolean, got number")
      evalErr(
        """std.manifestYamlStream([{}], indent_array_in_object="x")"""
      ).contains("indent_array_in_object has to be a boolean, got string")
      evalErr(
        """std.manifestYamlStream([{}], false, c_document_end=[])"""
      ).contains("c_document_end has to be a boolean, got array")
      evalErr(
        """std.manifestYamlStream([{}], false, false, quote_keys=null)"""
      ).contains("quote_keys has to be a boolean, got null")
    }

    test("largeIntegerDoubleMaterialization") {
      // Large whole numbers that don't fit in Long should render as decimal integers,
      // without Java scientific notation or exact binary64 noise.
      val expected1e100 = "1" + "0" * 100
      val expected1e308 = "1" + "0" * 308
      eval("""std.manifestJson(1e100)""") ==> ujson.Str(expected1e100)
      eval("""std.manifestJson(1e308)""") ==> ujson.Str(expected1e308)
      eval("""std.manifestToml({a: 1e100})""") ==> ujson.Str("a = " + expected1e100)
    }

    test("assertBooleanTypeCheck") {
      // non-boolean assert condition should error with type error
      assert(evalErr("""assert "hello"; 42""").contains("Expected boolean, got string"))
      assert(evalErr("""assert 0; 42""").contains("Expected boolean, got number"))
      assert(evalErr("""assert null; 42""").contains("Expected boolean, got null"))
      // boolean conditions should still work
      eval("""assert true; 42""") ==> ujson.Num(42)
      assert(evalErr("""assert false; 42""").contains("Assertion failed"))
    }

    test("inOperatorWordBoundary") {
      // `in` still works as a binary operator
      eval(""" "a" in {a: 1} """) ==> ujson.True
      eval(""" "b" in {a: 1} """) ==> ujson.False
      // identifiers starting with "in" must not be parsed as the `in` operator
      eval("""local index = 42; index""") ==> ujson.Num(42)
      eval("""local information = [1, 2, 3]; information""") ==> ujson.Arr(1, 2, 3)
      eval("""local input = {a: 1}; input""") ==> ujson.Obj("a" -> ujson.Num(1))
    }

    test("stdlibParameterNames") {
      // std.hypot(a, b) per official docs, not std.hypot(x, y)
      eval("""std.hypot(a=3, b=4)""") ==> ujson.Num(5)
      // std.xor(x, y) per official docs, not std.xor(bool1, bool2)
      eval("""std.xor(x=true, y=false)""") ==> ujson.True
      eval("""std.xor(x=true, y=true)""") ==> ujson.False
      // std.xnor(x, y) per official docs, not std.xnor(bool1, bool2)
      eval("""std.xnor(x=true, y=true)""") ==> ujson.True
      eval("""std.xnor(x=true, y=false)""") ==> ujson.False
    }

  }
}
