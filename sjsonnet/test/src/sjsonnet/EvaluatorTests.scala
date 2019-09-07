package sjsonnet

import utest._
object EvaluatorTests extends TestSuite{
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match{
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }
  def tests = Tests{
    test("arithmetic") {
      eval("1 + 2 + 3") ==> ujson.Num(6)
      eval("1 + 2 * 3") ==> ujson.Num(7)
      eval("-1 + 2 * 3") ==> ujson.Num(5)
      eval("6 - 3 + 2") ==> ujson.Num(5)
    }
    test("objects") {
      eval("{x: 1}.x") ==> ujson.Num(1)
    }
    test("arrays") {
      eval("[1, [2, 3], 4][1][0]") ==> ujson.Num(2)
      eval("([1, 2, 3] + [4, 5, 6])[3]") ==> ujson.Num(4)
    }
    test("functions") {
      eval("(function(x) x)(1)") ==> ujson.Num(1)
      eval("function() 1") ==> ujson.Num(1)
      eval("function(a=1) a") ==> ujson.Num(1)
      eval("(function(x, y = x + 1) y)(x = 10)") ==> ujson.Num(11)
    }
    test("members") {
      eval("{local x = 1, x: x}['x']") ==> ujson.Num(1)
      eval("{local x(y) = y + '1', x: x('2')}['x']") ==> ujson.Str("21")
      eval("{local x(y) = y + '1', x: x(y='2')}['x']") ==> ujson.Str("21")
      eval("{local x(y='2') = y + '1', x: x()}['x']") ==> ujson.Str("21")
      eval("""{[{local x = $.y + "lol", y: x, z: "1"}.z]: 2}["1"]""") ==> ujson.Num(2)
      eval("{local x = 1, y: { z: x }}.y.z") ==> ujson.Num(1)
    }
    test("extends") {
      eval("(function(a) a.x + a.y)({x: 1}{y: 2})") ==> ujson.Num(3)
      eval("(function(a) a.x + a.y)({x: 1}{x: 2, y: 3})") ==> ujson.Num(5)
      eval("({x: 1}{x+: 2}).x") ==> ujson.Num(3)
      eval("({x+: 1}{x: 2}).x") ==> ujson.Num(2)
      eval("({x+: 1}{x+: 2}).x") ==> ujson.Num(3)
      eval("({x+: 1} + {x+: 2}).x") ==> ujson.Num(3)
      eval("(function(a, b) a + b)({x+: 1}, {x+: 2}).x") ==> ujson.Num(3)
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
      eval("{x: 1, y: {x: 2, z: $.x + 10}}.y.z") ==> ujson.Num(11)
      eval("{x: 1, y: {x: 2, z: self.x + 10}}.y.z") ==> ujson.Num(12)
      eval("{x: 1, y: {x: 0, y: self.x}.y}.y") ==> ujson.Num(0)
    }
    test("topLevel") {
      eval("local p(n='A') = {w: 'H'+n}; {p: p()}.p.w") ==> ujson.Str("HA")
    }
    test("lazy") {
      eval("[{x: $.y, y: $.x}.x, 2][1]") ==> ujson.Num(2)
      eval("{x: $.y, y: $.x, local z(z0) = [3], w: z($.x)}.w[0]") ==> ujson.Num(3)
      eval("(function(a=[1, b[1]], b=[a[0], 2]) [a, b])()[0][1]") ==> ujson.Num(2)
    }
    test("comprehensions") {
      eval("[x + 1 for x in [1, 2, 3]][2]") ==> ujson.Num(4)
      eval("[x + 1, for x in [1, 2, 3]][2]") ==> ujson.Num(4)
      eval("[x + y for x in [1, 2, 3] for y in [4, 5, 6] if x + y != 7][3]") ==> ujson.Num(8)
      eval("""{[""+x]: x * x for x in [1, 2, 3]}["3"]""") ==> ujson.Num(9)
      eval("""{local y = $["2"], [x]: if x == "1" then y else 0, for x in ["1", "2"]}["1"]""") ==> ujson.Num(0)
    }
    test("super") {
      test("implicit") {

        eval("({ x: 1, y: self.x } + { x: 2 }).y") ==> ujson.Num(2)
        eval("({ local x = $.y, y: 1, z: x} + { y: 2 }).z") ==> ujson.Num(2)
        eval("({ local x = self.y, y: 1, z: x} + { y: 2 }).z") ==> ujson.Num(2)
        eval("local A = {x: 1, local outer = self, y: A{z: outer}}; A.y.z.x") ==> ujson.Num(1)
        eval("{local x = self, y: 1, z: {a: x, y: 2}}.z.a.y") ==> ujson.Num(1)
        eval("local A = {x: 1, local outer = self, y: A{x: outer.x}}; A.y.x") ==> ujson.Num(1)
        eval("local A = {x: 1, local outer = self, y: A{x: outer.x + 1}}; A.y.y.x") ==> ujson.Num(3)
      }
      test("explicit") {

        eval("{ x: 1, y: self.x } + { x: 2, y: super.y + 1}") ==>
          ujson.read("""{ "x": 2, "y": 3 }""")

        eval("{ x: 1 } + { x: 2, y: super.x } + { x: 3, z: super.x }") ==>
          ujson.read("""{ "x": 3, "y": 1, "z": 2 }""")
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

      eval("""("%(hello)s" % {hello::"world"})""") ==> ujson.Str("world")

      eval("""("%(hello)s" % {hello::"world", bad:: error "lol"})""") ==> ujson.Str("world")
    }
    test("evaluator2") {
      eval("""{local x = 1, [x]: x, for x in ["foo"]}.foo""") ==> ujson.Num(1)
      eval("""{[x]: x, local x = 1, for x in ["foo"]}.foo""") ==> ujson.Num(1)
      eval("""local foo = ["foo"]; {local foo = 1, [x]: x, for x in foo}.foo""") ==> ujson.Str("foo")
      eval("""local foo = ["foo"]; {[x]: x, local foo = 2, for x in foo}.foo""") ==> ujson.Str("foo")

      eval("""{ [x + ""]: if x == 1 then 1 else x + $["1"] for x in [1, 2, 3] }""") ==>
        ujson.read("""{ "1": 1, "2": 3, "3": 4 }""")

      eval("""local x = "baz"; { local x = "bar", [x]: x for x in ["foo"] }""") ==>
        ujson.read("""{ "foo": "bar" }""")

      eval("""{ [x + ""]: x + foo, local foo = 3 for x in [1, 2, 3] }""") ==>
        ujson.read("""{ "1": 4, "2": 5, "3": 6 }""")
      eval("""{local y = x, ["foo"]: y, for x in ["foo"]}.foo""") ==> ujson.Str("foo")
    }
    test("shadowing") {
      eval("local x = 1; local x = 2; x") ==> ujson.Num(2)
      eval("local x = 1; x + local x = 2; x") ==> ujson.Num(3)
      eval("""local str1 = |||
             |        text
             |    |||;
             |
             |local str1 = |||
             |        \n
             |    |||;
             |
             |(str1 == "\\n\n")
             |""".stripMargin) ==> ujson.True
    }
    test("stdLib") {
      eval("std.pow(2, 3)") ==> ujson.Num(8)
      eval("std.pow(x=2, n=3)") ==> ujson.Num(8)
      eval("std.pow(n=3, x=2)") ==> ujson.Num(8)
      eval("({a:: 1} + {a+:::2}).a") ==> ujson.Num(3)
      eval("(std.prune({a:: 1}) + {a+:::2}).a") ==> ujson.Num(2)
      eval("std.toString(std.mapWithIndex(function(idx, elem) elem, [2,1,0]))") ==> ujson.Str("[2, 1, 0]")
    }
    test("unboundParam") {
      val ex = intercept[Exception]{
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

      assert(ex.getMessage.contains("Function parameter y not bound in call"))
    }

    test("invalidParam") {
      val ex = intercept[Exception]{
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

      assert(ex.getMessage.contains("Function has no parameter hello"))
    }

    test("nakedSelf") {
      val ex = intercept[Exception]{eval("self.x")}
      assert(ex.getMessage.contains("Cannot use `self` outside an object"))
    }

    test("nakedDollar") {
      val ex = intercept[Exception]{eval("$.x")}
      assert(ex.getMessage.contains("Cannot use `$` outside an object"))
    }

    test("nakedSuper") {
      val ex = intercept[Exception]{eval("super.x")}
      assert(ex.getMessage.contains("Cannot use `super` outside an object"))
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
      eval("local f(x) = x; {hello: 123, world: f(x=$.hello)}") ==>
        ujson.Obj("hello" -> 123, "world" -> 123)
    }
    test("stdSubstr") {
      eval("std.substr('cookie', 6, 2)") ==> ujson.Str("")
    }
//    test("format") {
//      eval("\"%s\" % \"world\"") ==> Value.Str("world")
//      eval("\"%s\" % [\"world\"]") ==> Value.Str("world")
//      eval("\"%s %s\" % [\"hello\", \"world\"]") ==> Value.Str("hello world")
//      eval("\"%(hello)s\" % {hello: \"world\"}") ==> Value.Str("world")
//    }
  }
}
