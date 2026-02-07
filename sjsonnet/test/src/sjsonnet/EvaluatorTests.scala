package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object EvaluatorTests extends TestSuite {
  def allTests(useNewEvaluator: Boolean): Tests = Tests {
    test("arithmetic") {
      eval("1 + 2 + 3", useNewEvaluator = useNewEvaluator) ==> ujson.Num(6)
      eval("1 + 2 * 3", useNewEvaluator = useNewEvaluator) ==> ujson.Num(7)
      eval("-1 + 2 * 3", useNewEvaluator = useNewEvaluator) ==> ujson.Num(5)
      eval("6 - 3 + 2", useNewEvaluator = useNewEvaluator) ==> ujson.Num(5)
    }
    test("objects") {
      eval("{x: 1}.x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
    }
    test("arrays") {
      eval("[1, [2, 3], 4][1][0]", useNewEvaluator = useNewEvaluator) ==> ujson.Num(2)
      eval("([1, 2, 3] + [4, 5, 6])[3]", useNewEvaluator = useNewEvaluator) ==> ujson.Num(4)
      evalErr("[][0]", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: array bounds error: array is empty
        |at [Lookup].(:1:3)""".stripMargin
      eval("std.slice(std.range(1,4), 0, null, 2)", useNewEvaluator = useNewEvaluator) ==> ujson
        .Arr(1, 3)
      eval("std.slice(std.range(1,4), null, null, 2)", useNewEvaluator = useNewEvaluator) ==> ujson
        .Arr(1, 3)
      eval(
        "std.slice(std.range(1,4), null, null, null)",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Arr(1, 2, 3, 4)
      eval("""
             |std.slice("jsonnet", -3, null, null)
             |""".stripMargin) ==> ujson.Str("net")
      eval("std.range(1,4)[0::2]", useNewEvaluator = useNewEvaluator) ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[0:null:2]", useNewEvaluator = useNewEvaluator) ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[null:null:2]", useNewEvaluator = useNewEvaluator) ==> ujson.Arr(1, 3)
      eval("std.range(1,4)[null:null:null]", useNewEvaluator = useNewEvaluator) ==> ujson.Arr(
        1,
        2,
        3,
        4
      )
      eval("std.range(1,4)[::2]", useNewEvaluator = useNewEvaluator) ==> ujson.Arr(1, 3)
    }
    test("functions") {
      eval("(function(x) x)(1)", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("function() 1", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("function(a=1) a", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("(function(x, y = x + 1) y)(x = 10)", useNewEvaluator = useNewEvaluator) ==> ujson.Num(
        11
      )
      eval("local f(x) = function() true; f(42)", useNewEvaluator = useNewEvaluator) ==> ujson.True
      eval(
        "local f(x) = function() true; f(42) == true",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.False
      eval(
        "local f(x) = function() true; f(42)() == true",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.True
      assert(
        evalErr("{foo: function() true}", useNewEvaluator = useNewEvaluator).startsWith(
          "sjsonnet.Error: Couldn't manifest function with params"
        )
      )
      eval("{foo: (function() true)()}", useNewEvaluator = useNewEvaluator) ==> ujson.Obj {
        "foo" -> ujson.True
      }
    }
    test("members") {
      eval("{local x = 1, x: x}['x']", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("{local x(y) = y + '1', x: x('2')}['x']", useNewEvaluator = useNewEvaluator) ==> ujson
        .Str("21")
      eval("{local x(y) = y + '1', x: x(y='2')}['x']", useNewEvaluator = useNewEvaluator) ==> ujson
        .Str("21")
      eval("{local x(y='2') = y + '1', x: x()}['x']", useNewEvaluator = useNewEvaluator) ==> ujson
        .Str("21")
      eval(
        """{[{local x = $.y + "lol", y: x, z: "1"}.z]: 2}["1"]""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(2)
      eval("{local x = 1, y: { z: x }}.y.z", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
    }
    test("extends") {
      eval("(function(a) a.x + a.y)({x: 1}{y: 2})", useNewEvaluator = useNewEvaluator) ==> ujson
        .Num(3)
      eval(
        "(function(a) a.x + a.y)({x: 1}{x: 2, y: 3})",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(5)
      eval("({x: 1}{x+: 2}).x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(3)
      eval("({x+: 1}{x: 2}).x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(2)
      eval("({x+: 1}{x+: 2}).x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(3)
      eval("({x+: 1} + {x+: 2}).x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(3)
      eval(
        "(function(a, b) a + b)({x+: 1}, {x+: 2}).x",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(3)
      eval("""({a: [1]} + {a+: "1"}).a""", useNewEvaluator = useNewEvaluator) ==> ujson.Str("[1]1")
      eval("""({a: 1} + {a+: "1"}).a""", useNewEvaluator = useNewEvaluator) ==> ujson.Str("11")
      eval("""({a: "1"} + {a+: 1}).a""", useNewEvaluator = useNewEvaluator) ==> ujson.Str("11")
    }
    test("ifElse") {
      eval("if true then 1 else 0", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("if 2 > 1 then 1 else 0", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("if false then 1 else 0", useNewEvaluator = useNewEvaluator) ==> ujson.Num(0)
      eval("if 1 > 2 then 1 else 0", useNewEvaluator = useNewEvaluator) ==> ujson.Num(0)
    }
    test("self") {
      eval("{x: 1, y: $.x + 10}.y", useNewEvaluator = useNewEvaluator) ==> ujson.Num(11)
      eval("{x: 1, y: self.x}.y", useNewEvaluator = useNewEvaluator) ==> ujson.Num(1)
      eval("{x: 1, y: {x: 2, z: $.x + 10}}.y.z", useNewEvaluator = useNewEvaluator) ==> ujson.Num(
        11
      )
      eval("{x: 1, y: {x: 2, z: self.x + 10}}.y.z", useNewEvaluator = useNewEvaluator) ==> ujson
        .Num(12)
      eval("{x: 1, y: {x: 0, y: self.x}.y}.y", useNewEvaluator = useNewEvaluator) ==> ujson.Num(0)
      eval("""{x: 1, y: "x" in self}.y""", useNewEvaluator = useNewEvaluator) ==> ujson.True
      eval("""{x: 1, y: "z" in self}.y""", useNewEvaluator = useNewEvaluator) ==> ujson.False
      eval("""{y: "y" in self}.y""", useNewEvaluator = useNewEvaluator) ==> ujson.True
    }
    test("topLevel") {
      eval("local p(n='A') = {w: 'H'+n}; {p: p()}.p.w", useNewEvaluator = useNewEvaluator) ==> ujson
        .Str("HA")
    }
    test("lazy") {
      eval("[{x: $.y, y: $.x}.x, 2][1]", useNewEvaluator = useNewEvaluator) ==> ujson.Num(2)
      eval(
        "{x: $.y, y: $.x, local z(z0) = [3], w: z($.x)}.w[0]",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(3)
      eval(
        "(function(a=[1, b[1]], b=[a[0], 2]) [a, b])()[0][1]",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(2)
    }
    test("comprehensions") {
      eval("[x + 1 for x in [1, 2, 3]][2]", useNewEvaluator = useNewEvaluator) ==> ujson.Num(4)
      eval("[x + 1, for x in [1, 2, 3]][2]", useNewEvaluator = useNewEvaluator) ==> ujson.Num(4)
      eval(
        "[x + y for x in [1, 2, 3] for y in [4, 5, 6] if x + y != 7][3]",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(8)
      eval(
        """{[""+x]: x * x for x in [1, 2, 3]}["3"]""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(9)
      eval(
        """{local y = $["2"], [x]: if x == "1" then y else 0, for x in ["1", "2"]}["1"]""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(0)
      // References between locals in an object comprehension:
      eval(
        """{local a = 1, local b = a + 1, [k]: b + 1 for k in ["x"]}""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Obj("x" -> ujson.Num(3))
      // Locals which reference variables from the comprehension:
      eval(
        """{local x2 = k*2, [std.toString(k)]: x2 for k in [1]}""",
        useNewEvaluator = useNewEvaluator
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
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
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
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
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
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Obj("x" -> ujson.Num(3))
      // Regression test for a bug in handling of non-string field names:
      evalErr("{[k]: k for k in [1]}", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: Field name must be string or null, not number
          |at .(:1:1)""".stripMargin
      // Basic function support:
      eval(
        """
          |local funcs = {
          |  [a](x): x * 2
          |  for a in ["f1", "f2", "f3"]
          |};
          |funcs.f1(10)
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
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
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(20)
    }
    test("super") {
      test("implicit") {

        eval("({ x: 1, y: self.x } + { x: 2 }).y", useNewEvaluator = useNewEvaluator) ==> ujson.Num(
          2
        )
        eval(
          "({ local x = $.y, y: 1, z: x} + { y: 2 }).z",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(2)
        eval(
          "({ local x = self.y, y: 1, z: x} + { y: 2 }).z",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(2)
        eval(
          "local A = {x: 1, local outer = self, y: A{z: outer}}; A.y.z.x",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(1)
        eval(
          "{local x = self, y: 1, z: {a: x, y: 2}}.z.a.y",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(1)
        eval(
          "local A = {x: 1, local outer = self, y: A{x: outer.x}}; A.y.x",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(1)
        eval(
          "local A = {x: 1, local outer = self, y: A{x: outer.x + 1}}; A.y.y.x",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(3)
        eval("""("a" in ({a: 1}{b: 2}))""", useNewEvaluator = useNewEvaluator) ==> ujson.True
      }
      test("explicit") {

        eval("{ x: 1, y: self.x } + { x: 2, y: super.y + 1}", useNewEvaluator = useNewEvaluator) ==>
        ujson.read("""{ "x": 2, "y": 3 }""")

        eval(
          "{ x: 1 } + { x: 2, y: super.x } + { x: 3, z: super.x }",
          useNewEvaluator = useNewEvaluator
        ) ==>
        ujson.read("""{ "x": 3, "y": 1, "z": 2 }""")

        eval(
          """({a: 1} + {b: 2} + {c: ["a" in super, "b" in super]}).c""",
          useNewEvaluator = useNewEvaluator
        ) ==>
        ujson.Arr(true, true)

        eval("""{a: ["a" in super, "b" in super]}.a""", useNewEvaluator = useNewEvaluator) ==> ujson
          .Arr(false, false)

        eval("""(({a: 1}{b: 2}){c: super.a}).c""", useNewEvaluator = useNewEvaluator) ==> ujson.Num(
          1
        )
        eval("""(({a: 1}{b: 2}){c: super.b}).c""", useNewEvaluator = useNewEvaluator) ==> ujson.Num(
          2
        )

        eval(
          """(({a: 1}{b: 2, f: super.a}){c: super.f}).c""",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(1)
        val ex = assertThrows[Exception] {
          eval("""(({a: 1}{b: 2, f: super.b}){c: super.f}).c""", useNewEvaluator = useNewEvaluator)
        }
        assert(ex.getMessage.contains("Field does not exist: b"))

        eval(
          """(({a: 1}{b: 2}) + ({c: super.b}{d: super.a})).c""",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(2)
        eval(
          """(({a: 1}{b: 2}) + ({c: super.b}{d: super.a})).d""",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(1)

        eval(
          """local x = {a: 1}; local y = {b: super.a}; x + y""",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.read("""{"a": 1, "b": 1}""")

        eval(
          """local x = { a: 1, b: { c: 2 }}; x { a: super.a * 10, b:: { c: super.b.c * 10 } }""",
          useNewEvaluator = useNewEvaluator
        ) ==>
        ujson.Obj("a" -> ujson.Num(10))
        evalErr(
          """local x = { a: 1, b: { c: 2 }}; x { a: super.a * 10, b:: { c: super.b.c * 10 } }.b""",
          useNewEvaluator = useNewEvaluator
        ) ==>
        """sjsonnet.Error: Attempt to use `super` when there is no super class
          |at [SelectSuper b].(:1:68)
          |at [Select c].(:1:70)
          |at [BinaryOp *].(:1:73)""".stripMargin
      }
    }
    test("hidden") {
      eval("{i::1 }", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{}""")

      eval("{i:: 1} + {i: 2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{}""")

      eval("{i: 1} + {i:: 2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{}""")

      eval("{i:: 1} + {i:: 2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{}""")

      eval("{i:::1} + {i::2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{}""")

      eval("{i::1} + {i:::2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{"i": 2}""")

      eval("{i:1} + {i:::2}", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{"i": 2}""")

      eval("local M = {x+: self.i, i :: 1}; { x: 1 } + M", useNewEvaluator = useNewEvaluator) ==>
      ujson.read("""{ "x": 2 }""")

      eval("""("%(hello)s" % {hello::"world"})""", useNewEvaluator = useNewEvaluator) ==> ujson.Str(
        "world"
      )

      eval(
        """("%(hello)s" % {hello::"world", bad:: error "lol"})""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Str("world")
    }
    test("evaluator2") {
      eval(
        """{local x = 1, [x]: x, for x in ["foo"]}.foo""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(1)
      eval(
        """{[x]: x, local x = 1, for x in ["foo"]}.foo""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(1)
      eval(
        """local foo = ["foo"]; {local foo = 1, [x]: x, for x in foo}.foo""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Str("foo")
      eval(
        """local foo = ["foo"]; {[x]: x, local foo = 2, for x in foo}.foo""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Str("foo")

      eval(
        """{ [x + ""]: if x == 1 then 1 else x + $["1"] for x in [1, 2, 3] }""",
        useNewEvaluator = useNewEvaluator
      ) ==>
      ujson.read("""{ "1": 1, "2": 3, "3": 4 }""")

      eval(
        """local x = "baz"; { local x = "bar", [x]: x for x in ["foo"] }""",
        useNewEvaluator = useNewEvaluator
      ) ==>
      ujson.read("""{ "foo": "bar" }""")

      eval(
        """{ [x + ""]: x + foo, local foo = 3 for x in [1, 2, 3] }""",
        useNewEvaluator = useNewEvaluator
      ) ==>
      ujson.read("""{ "1": 4, "2": 5, "3": 6 }""")
      eval(
        """{local y = x, ["foo"]: y, for x in ["foo"]}.foo""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Str("foo")
    }
    test("shadowing") {
      eval("local x = 1; local x = 2; x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(2)
      eval("local x = 1; x + local x = 2; x", useNewEvaluator = useNewEvaluator) ==> ujson.Num(3)
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
          |""".stripMargin,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.True
    }
    test("stdLib") {
      eval("std.pow(2, 3)", useNewEvaluator = useNewEvaluator) ==> ujson.Num(8)
      eval("std.pow(x=2, n=3)", useNewEvaluator = useNewEvaluator) ==> ujson.Num(8)
      eval("std.pow(n=3, x=2)", useNewEvaluator = useNewEvaluator) ==> ujson.Num(8)
      eval("({a:: 1} + {a+:::2}).a", useNewEvaluator = useNewEvaluator) ==> ujson.Num(3)
      eval("(std.prune({a:: 1}) + {a+:::2}).a", useNewEvaluator = useNewEvaluator) ==> ujson.Num(2)
      eval(
        "std.toString(std.mapWithIndex(function(idx, elem) elem, [2,1,0]))",
        useNewEvaluator = useNewEvaluator
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
        """.stripMargin,
          useNewEvaluator = useNewEvaluator
        )
      }

      assert(ex.getMessage.contains("Function parameter y not bound in call"))
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
        """.stripMargin,
          useNewEvaluator = useNewEvaluator
        )
      }

      assert(ex.getMessage.contains("Function has no parameter hello"))
    }

    test("unknownVariable") {
      evalErr("x", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.StaticError: Unknown variable: x
        |at [Id x].(:1:1)""".stripMargin
      evalErr("self.x", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.StaticError: Can't use self outside of an object
        |at [Self].(:1:1)""".stripMargin
      evalErr("$.x", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.StaticError: Can't use $ outside of an object
        |at [$].(:1:1)""".stripMargin
      evalErr("super.x", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.StaticError: Can't use super outside of an object
        |at [Super].(:1:1)""".stripMargin
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
        """.stripMargin,
        useNewEvaluator = useNewEvaluator
      )

      res ==> ujson.Str("Hello Bob!")
    }

    test("equalDollar") {
      eval(
        "local f(x) = x; {hello: 123, world: f(x=$.hello)}",
        useNewEvaluator = useNewEvaluator
      ) ==>
      ujson.Obj("hello" -> 123, "world" -> 123)
    }
    test("stdSubstr") {
      eval("std.substr('cookie', 6, 2)", useNewEvaluator = useNewEvaluator) ==> ujson.Str("")
    }
    test("manifestIni") {
      eval(
        """std.manifestIni({
          |  main: { a: "1", b: 2, c: true, d: null, e: [1, {"2": 2}, [3]], f: {"hello": "world"} },
          |  sections: {}
          |})""".stripMargin,
        useNewEvaluator = useNewEvaluator
      ) ==>
      ujson.Str(
        "a = 1\nb = 2\nc = true\nd = null\ne = 1\ne = {\"2\": 2}\ne = [3]\nf = {\"hello\": \"world\"}\n"
      )
    }
    test("format") {
      eval("\"%s\" % \"world\"", useNewEvaluator = useNewEvaluator) ==> ujson.Str("world")
      eval("\"%s\" % [\"world\"]", useNewEvaluator = useNewEvaluator) ==> ujson.Str("world")
      eval("\"%s %s\" % [\"hello\", \"world\"]", useNewEvaluator = useNewEvaluator) ==> ujson.Str(
        "hello world"
      )
      eval("\"%(hello)s\" % {hello: \"world\"}", useNewEvaluator = useNewEvaluator) ==> ujson.Str(
        "world"
      )
      eval("\"%()s %()s!\" % [\"Hello\", \"World\"]", useNewEvaluator = useNewEvaluator) ==> ujson
        .Str("Hello World!")
    }
    test("binaryOps") {
      val ex = assertThrows[Exception](eval("1 && 2", useNewEvaluator = useNewEvaluator))
      assert(ex.getMessage.contains("binary operator && does not operate on numbers."))

      val ex2 = assertThrows[Exception](eval("1 || 2", useNewEvaluator = useNewEvaluator))
      assert(ex2.getMessage.contains("binary operator || does not operate on numbers."))
    }
    test("stdToString") {
      eval("""std.toString({k: "v"})""", useNewEvaluator = useNewEvaluator) ==> ujson.Str(
        """{"k": "v"}"""
      )
    }
    test("floatFormatRegression") {
      eval("'%.4f' % 0.01", useNewEvaluator = useNewEvaluator) ==> ujson.Str("0.0100")
      eval("'%05d' % 2", useNewEvaluator = useNewEvaluator) ==> ujson.Str("00002")
      eval("'%000d' % 2", useNewEvaluator = useNewEvaluator) ==> ujson.Str("2")
      eval("'%000d' % 2.123", useNewEvaluator = useNewEvaluator) ==> ujson.Str("2")
      eval("'%5d' % 2", useNewEvaluator = useNewEvaluator) ==> ujson.Str("    2")
      eval("'%5f' % 2", useNewEvaluator = useNewEvaluator) ==> ujson.Str("2.000000")

      eval("'%10d' % 2.123", useNewEvaluator = useNewEvaluator) ==> ujson.Str("         2")
      eval("'%+5.5f' % 123.456", useNewEvaluator = useNewEvaluator) ==> ujson.Str("+123.45600")
      eval("'%+5.5f' % -123.456", useNewEvaluator = useNewEvaluator) ==> ujson.Str("-123.45600")
      eval("'% 5.5f' % -123.456", useNewEvaluator = useNewEvaluator) ==> ujson.Str("-123.45600")
      eval("'%--+5.5f' % -123.456", useNewEvaluator = useNewEvaluator) ==> ujson.Str("-123.45600")
      eval("'%#-0- + 5.5f' % -123.456", useNewEvaluator = useNewEvaluator) ==> ujson.Str(
        "-123.45600"
      )
    }
    test("strict") {
      eval("({ a: 1 } { b: 2 }).a", strict = false, useNewEvaluator = useNewEvaluator) ==> ujson
        .Num(1)
      evalErr("({ a: 1 } { b: 2 }).a", strict = true, useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.StaticError: Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects
        |at [ObjExtend].(:1:11)""".stripMargin
      eval(
        "local x = { c: 3 }; (x { a: 1 } { b: 2 }).a",
        strict = false,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(1)
      eval(
        "local x = { c: 3 }; (x { a: 1 }).a",
        strict = true,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(1)
      evalErr(
        "local x = { c: 3 }; ({ a: 1 } { b: 2 }).a",
        strict = true,
        useNewEvaluator = useNewEvaluator
      ) ==>
      """sjsonnet.StaticError: Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects
        |at [ObjExtend].(:1:31)""".stripMargin
    }
    test("objectDeclaration") {
      eval("{ ['foo']: x for x in  []}", false, useNewEvaluator = useNewEvaluator) ==> ujson.Obj()
      eval("{ ['foo']: x for x in  [1]}", false, useNewEvaluator = useNewEvaluator) ==> ujson.Obj(
        "foo" -> 1
      )

      eval("{ ['foo']+: x for x in  []}", false, useNewEvaluator = useNewEvaluator) ==> ujson.Obj()
      eval("{ ['foo']+: x for x in  [1]}", false, useNewEvaluator = useNewEvaluator) ==> ujson.Obj(
        "foo" -> 1
      )
      eval(
        "{ ['foo']+: [x] for x in [1]} + { ['foo']+: [x] for x in [2]}",
        false,
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Obj("foo" -> ujson.Arr(1, 2))
    }
    test("givenNoDuplicateFieldsInListComprehension1_expectSuccess") {
      eval("""{ ["bar"]: x for x in [-876.89]}""", useNewEvaluator = useNewEvaluator) ==> ujson.Obj(
        "bar" -> -876.89
      )
    }
    test("givenNoDuplicateFieldsInListComprehension2_expectSuccess") {
      eval("""{ ["bar_" + x]: x for x in [5,12]}""", useNewEvaluator = useNewEvaluator) ==> ujson
        .Obj("bar_5" -> 5, "bar_12" -> 12)
    }
    test("givenDuplicateFieldsInListComprehension_expectFailure") {
      evalErr(
        """{ [x]: x for x in ["A", "A"]}""",
        useNewEvaluator = useNewEvaluator
      ) ==>
      """sjsonnet.Error: Duplicate key A in evaluated object comprehension.
        |at .(:1:1)""".stripMargin
    }
    test("givenDuplicateFieldsInIndirectListComprehension_expectFailure") {
      evalErr(
        """local y = { a: "A" };
          |local z = { a: "A" };
          |{ [x.a]: x for x in [y, z]}""".stripMargin,
        useNewEvaluator = useNewEvaluator
      ) ==>
      """sjsonnet.Error: Duplicate key A in evaluated object comprehension.
        |at .(:3:1)""".stripMargin
    }
    test("functionEqualsNull") {
      eval("""local f(x)=null; f == null""", useNewEvaluator = useNewEvaluator) ==> ujson.False
      eval("""local f=null; f == null""", useNewEvaluator = useNewEvaluator) ==> ujson.True
    }

    test("dynamicDuplicateFields") {
      // Object evaluation should fail if the object contains duplicate field names.
      // This is a regression test for duplicate detection when one or more of the
      // duplicate field names is dynamically computed via a field name expression.

      // Cases where StaticOptimizer replaces the dynamic field names with fixed ones:
      test - (evalErr("""{ ["k"]: 1, ["k"]: 2 }""", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at .(:1:13)""".stripMargin)

      test - (evalErr("""{ k: 1, ["k"]: 2 }""", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at .(:1:9)""".stripMargin)

      test - (evalErr("""{ ["k"]: 1, k: 2 }""", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at .(:1:13)""".stripMargin)

      // Test that lazy evaluation is preserved - duplicate fields should only error when accessed
      test - (eval(
        """{x: { ["k"]: 1, ["k"]: 2 }, y:1 }.y""",
        useNewEvaluator = useNewEvaluator
      ) ==> ujson.Num(1))

      // But accessing the problematic field should still error
      test - (evalErr(
        """{x: { ["k"]: 1, ["k"]: 2 }, y:1 }.x""",
        useNewEvaluator = useNewEvaluator
      ) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at .(:1:17)
          |at [Select x].(:1:34)""".stripMargin)

      // Non-StaticOptimizable case:
      test - (evalErr("""{ k: 1, ["k" + ""]: 2 }""", useNewEvaluator = useNewEvaluator) ==>
      """sjsonnet.Error: Duplicate key k in evaluated object.
          |at .(:1:9)""".stripMargin)
    }

    test("identifierStartsWithKeyword") {
      for (keyword <- Parser.keywords) {
        eval(
          s"""local ${keyword}Foo = 123; ${keyword}Foo""",
          useNewEvaluator = useNewEvaluator
        ) ==> ujson.Num(123)
        eval(s"""{${keyword}Foo: 123}""", useNewEvaluator = useNewEvaluator) ==> ujson.Obj(
          s"${keyword}Foo" -> ujson.Num(123)
        )
      }
    }

    test("errorNonString") {
      assert(
        evalErr("""error {a: "b"}""", useNewEvaluator = useNewEvaluator).contains("""{"a": "b"}""")
      )
      assert(
        evalErr("""assert 1 == 2 : { a: "b"}; 1""", useNewEvaluator = useNewEvaluator).contains(
          """{"a": "b"}"""
        )
      )
    }

    test("assertInheritance") {
      test - assert(
        evalErr("""{ } + {assert false}""", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{assert false} + {}""", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{assert false} + {} + {}""", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{} + {assert false} + {}""", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("""{} + {} + {assert false}""", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      // Both own and inherited assertions are evaluated:
      test - assert(
        evalErr("{assert false} + {assert true}", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      // Accessing an object member should trigger computation of that object's own
      // assertions plus any inherited assertions, even if the accessed member happens
      // to be a constant.
      test - assert(
        evalErr("({assert false} + {x: 2}).x", useNewEvaluator = useNewEvaluator).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr("({assert false} + {f(x): x}).f(1)", useNewEvaluator = useNewEvaluator).contains(
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
            useNewEvaluator = useNewEvaluator
          ).contains("sjsonnet.Error: Assertion failed")
        )
      }
    }
    test("assertInheritanceWithBrokenAssertions") {
      test - assert(
        evalErr(
          """{ } + {assert false}""",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{assert false} + {}""",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{assert false} + {} + {}""",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{} + {assert false} + {}""",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          """{} + {} + {assert false}""",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ).contains(
          "sjsonnet.Error: Assertion failed"
        )
      )
      test - assert(
        evalErr(
          "({assert false} + {f(x): x}).f(1)",
          useNewEvaluator = useNewEvaluator,
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
            useNewEvaluator = useNewEvaluator,
            brokenAssertionLogic = true
          ).contains("sjsonnet.Error: Assertion failed")
        )
      }

      // Behavior is different from now on.
      test {
        eval(
          "{assert false} + {assert true}",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ) ==> ujson.Obj()
        eval(
          "({assert false} + {x: 2}).x",
          useNewEvaluator = useNewEvaluator,
          brokenAssertionLogic = true
        ) ==> ujson.Num(2)
      }
    }

  }
  def tests: Tests = allTests(false).prefix("Evaluator") ++ allTests(true).prefix("NewEvaluator")
}
