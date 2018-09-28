package sjsonnet

import utest._
import ujson.Js
object EvaluatorTests extends TestSuite{
  def eval(s: String) = {
    new Interpreter(new Parser(), Scope.empty).interpret(s) match{
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }
  def tests = Tests{
    'arithmetic - {
      eval("1 + 2 + 3") ==> Js.Num(6)
      eval("1 + 2 * 3") ==> Js.Num(7)
      eval("-1 + 2 * 3") ==> Js.Num(5)
      eval("6 - 3 + 2") ==> Js.Num(5)
    }
    'objects - {
      eval("{x: 1}.x") ==> Js.Num(1)
    }
    'arrays - {
      eval("[1, [2, 3], 4][1][0]") ==> Js.Num(2)
      eval("([1, 2, 3] + [4, 5, 6])[3]") ==> Js.Num(4)
    }
    'functions - {
      eval("(function(x) x)(1)") ==> Js.Num(1)
      eval("function() 1") ==> Js.Num(1)
      eval("function(a=1) a") ==> Js.Num(1)
    }
    'members - {
      eval("{local x = 1, x: x}['x']") ==> Js.Num(1)
      eval("{local x(y) = y + '1', x: x('2')}['x']") ==> Js.Str("21")
      eval("{local x(y) = y + '1', x: x(y='2')}['x']") ==> Js.Str("21")
      eval("{local x(y='2') = y + '1', x: x()}['x']") ==> Js.Str("21")
      eval("""{[{local x = $.y + "lol", y: x, z: "1"}.z]: 2}["1"]""") ==> Js.Num(2)
      eval("{local x = 1, y: { z: x }}.y.z") ==> Js.Num(1)
    }
    'extends - {
      eval("(function(a) a.x + a.y)({x: 1}{y: 2})") ==> Js.Num(3)
      eval("(function(a) a.x + a.y)({x: 1}{x: 2, y: 3})") ==> Js.Num(5)
      eval("({x: 1}{x+: 2}).x") ==> Js.Num(3)
      eval("({x+: 1}{x: 2}).x") ==> Js.Num(2)
      eval("({x+: 1}{x+: 2}).x") ==> Js.Num(3)
      eval("({x+: 1} + {x+: 2}).x") ==> Js.Num(3)
      eval("(function(a, b) a + b)({x+: 1}, {x+: 2}).x") ==> Js.Num(3)
    }
    'ifElse - {
      eval("if true then 1 else 0") ==> Js.Num(1)
      eval("if 2 > 1 then 1 else 0") ==> Js.Num(1)
      eval("if false then 1 else 0") ==> Js.Num(0)
      eval("if 1 > 2 then 1 else 0") ==> Js.Num(0)
    }
    'self - {
      eval("{x: 1, y: $.x + 10}.y") ==> Js.Num(11)
      eval("{x: 1, y: self.x}.y'") ==> Js.Num(1)
      eval("{x: 1, y: {x: 2, z: $.x + 10}}.y.z") ==> Js.Num(11)
      eval("{x: 1, y: {x: 2, z: self.x + 10}}.y.z") ==> Js.Num(12)
      eval("{x: 1, y: {x: 0, y: self.x}.y}.y") ==> Js.Num(0)
    }
    'topLevel - {
      eval("local p(n='A') = {w: 'H'+n}; {p: p()}.p.w") ==> Js.Str("HA")
    }
    'lazy - {
      eval("[{x: $.y, y: $.x}.x, 2][1]") ==> Js.Num(2)
      eval("{x: $.y, y: $.x, local z(z0) = [3], w: z($.x)}.w[0]") ==> Js.Num(3)
      eval("(function(a=[1, b[1]], b=[a[0], 2]) [a, b])()[0][1]") ==> Js.Num(2)
    }
    'comprehensions - {
      eval("[x + 1 for x in [1, 2, 3]][2]") ==> Js.Num(4)
      eval("[x + 1, for x in [1, 2, 3]][2]") ==> Js.Num(4)
      eval("[x + y for x in [1, 2, 3] for y in [4, 5, 6] if x + y != 7][3]") ==> Js.Num(8)
      eval("""{[""+x]: x * x for x in [1, 2, 3]}["3"]""") ==> Js.Num(9)
      eval("""{local y = $["2"], [x]: if x == "1" then y else 0, for x in ["1", "2"]}["1"]""") ==> Js.Num(0)
    }
    'super - {
      'implicit - {

        eval("({ x: 1, y: self.x } + { x: 2 }).y") ==> Js.Num(2)
        eval("({ local x = $.y, y: 1, z: x} + { y: 2 }).z") ==> Js.Num(2)
        eval("({ local x = self.y, y: 1, z: x} + { y: 2 }).z") ==> Js.Num(2)
        eval("local A = {x: 1, local outer = self, y: A{z: outer}}; A.y.z.x") ==> Js.Num(1)
        eval("{local x = self, y: 1, z: {a: x, y: 2}}.z.a.y") ==> Js.Num(1)
        eval("local A = {x: 1, local outer = self, y: A{x: outer.x}}; A.y.x") ==> Js.Num(1)
        eval("local A = {x: 1, local outer = self, y: A{x: outer.x + 1}}; A.y.y.x") ==> Js.Num(3)
      }
      'explicit - {

        eval("{ x: 1, y: self.x } + { x: 2, y: super.y + 1}") ==>
          ujson.read("""{ "x": 2, "y": 3 }""")

        eval("{ x: 1 } + { x: 2, y: super.x } + { x: 3, z: super.x }") ==>
          ujson.read("""{ "x": 3, "y": 1, "z": 2 }""")
      }
    }
    'hidden - {
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
    }
    'evaluator2 - {
      eval("""{local x = 1, [x]: x, for x in ["foo"]}.foo""") ==> Js.Num(1)
      eval("""{[x]: x, local x = 1, for x in ["foo"]}.foo""") ==> Js.Num(1)
      eval("""local foo = ["foo"]; {local foo = 1, [x]: x, for x in foo}.foo""") ==> Js.Str("foo")
      eval("""local foo = ["foo"]; {[x]: x, local foo = 2, for x in foo}.foo""") ==> Js.Str("foo")

      eval("""{ [x + ""]: if x == 1 then 1 else x + $["1"] for x in [1, 2, 3] }""") ==>
        ujson.read("""{ "1": 1, "2": 3, "3": 4 }""")

      eval("""local x = "baz"; { local x = "bar", [x]: x for x in ["foo"] }""") ==>
        ujson.read("""{ "foo": "bar" }""")

      eval("""{ [x + ""]: x + foo, local foo = 3 for x in [1, 2, 3] }""") ==>
        ujson.read("""{ "1": 4, "2": 5, "3": 6 }""")
      eval("""{local y = x, [x]: y, for x in ["foo"]}.foo""") ==> Js.Str("foo")
    }
    'shadowing - {
      eval("local x = 1; local x = 2; x") ==> Js.Num(2)
      eval("local x = 1; x + local x = 2; x") ==> Js.Num(3)
      eval("""local str1 = |||
             |        text
             |    |||;
             |
             |local str1 = |||
             |        \n
             |    |||;
             |
             |(str1 == "\\n\n")
             |""".stripMargin) ==> Js.True
    }
//    'format - {
//      eval("\"%s\" % \"world\"") ==> Value.Str("world")
//      eval("\"%s\" % [\"world\"]") ==> Value.Str("world")
//      eval("\"%s %s\" % [\"hello\", \"world\"]") ==> Value.Str("hello world")
//      eval("\"%(hello)s\" % {hello: \"world\"}") ==> Value.Str("world")
//    }
  }
}
