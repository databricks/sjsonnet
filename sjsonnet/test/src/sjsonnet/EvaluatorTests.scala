package sjsonnet

import utest._

object EvaluatorTests extends TestSuite{
  def eval(s: String) = {
    Evaluator.visitExpr(Parser.expr.parse(s).get.value, new Evaluator.Scope(None, None, Map.empty))
  }
  def tests = Tests{
    'arithmetic - {
      eval("1 + 2 + 3") ==> Value.Num(6)
      eval("1 + 2 * 3") ==> Value.Num(7)
      eval("-1 + 2 * 3") ==> Value.Num(5)
      eval("6 - 3 + 2") ==> Value.Num(5)
    }
    'objects - {
      eval("{x: 1}.x") ==> Value.Num(1)
    }
    'arrays - {
      eval("[1, [2, 3], 4][1][0]") ==> Value.Num(2)
      eval("([1, 2, 3] + [4, 5, 6])[3]") ==> Value.Num(4)
    }
    'functions - {
      eval("(function(x) x)(1)") ==> Value.Num(1)
    }
    'members - {
      eval("{local x = 1, x: x}['x']") ==> Value.Num(1)
      eval("{local x(y) = y + '1', x: x('2')}['x']") ==> Value.Str("21")
      eval("{local x(y) = y + '1', x: x(y='2')}['x']") ==> Value.Str("21")
      eval("{local x(y='2') = y + '1', x: x()}['x']") ==> Value.Str("21")
      eval("""{[{local x = $.y + "lol", y: x, z: "1"}.z]: 2}["1"]""") ==> Value.Num(2)
      eval("{local x = 1, y: { z: x }}.y.z") ==> Value.Num(1)
    }
    'extends - {
      eval("(function(a) a.x + a.y)({x: 1}{y: 2})") ==> Value.Num(3)
      eval("(function(a) a.x + a.y)({x: 1}{x: 2, y: 3})") ==> Value.Num(5)
      eval("({x: 1}{x+: 2}).x") ==> Value.Num(3)
      eval("({x+: 1}{x: 2}).x") ==> Value.Num(2)
      eval("({x+: 1}{x+: 2}).x") ==> Value.Num(3)
      eval("({x+: 1} + {x+: 2}).x") ==> Value.Num(3)
      eval("(function(a, b) a + b)({x+: 1}, {x+: 2}).x") ==> Value.Num(3)
    }
    'ifElse - {
      eval("if true then 1 else 0") ==> Value.Num(1)
      eval("if 2 > 1 then 1 else 0") ==> Value.Num(1)
      eval("if false then 1 else 0") ==> Value.Num(0)
      eval("if 1 > 2 then 1 else 0") ==> Value.Num(0)
    }
    'self - {
      eval("{x: 1, y: $.x + 10}.y") ==> Value.Num(11)
      eval("{x: 1, y: self.x}.y'") ==> Value.Num(1)
      eval("{x: 1, y: {x: 2, z: $.x + 10}}.y.z") ==> Value.Num(11)
      eval("{x: 1, y: {x: 2, z: self.x + 10}}.y.z") ==> Value.Num(12)
      eval("{x: 1, y: {x: 0, y: self.x}.y}.y") ==> Value.Num(0)
    }
    'topLevel - {
      eval("local p(n='A') = {w: 'H'+n}; {p: p()}.p.w") ==> Value.Str("HA")
    }
    'lazy - {
      eval("[{x: $.y, y: $.x}.x, 2][1]") ==> Value.Num(2)
      eval("{x: $.y, y: $.x, local z(z0) = [3], w: z($.x)}.w[0]") ==> Value.Num(3)
      eval("(function(a=[1, b[1]], b=[a[0], 2]) [a, b])()[0][1]") ==> Value.Num(2)
    }
    'comprehensions - {
      eval("[x + 1 for x in [1, 2, 3]][2]") ==> Value.Num(4)
      eval("[x + 1, for x in [1, 2, 3]][2]") ==> Value.Num(4)
      eval("[x + y for x in [1, 2, 3] for y in [4, 5, 6] if x + y != 7][3]") ==> Value.Num(8)
      eval("""{[""+x]: x * x for x in [1, 2, 3]}["3"]""") ==> Value.Num(9)
      eval("""{local y = $["2"], [x]: if x == "1" then y else 0, for x in ["1", "2"]}["1"]""") ==> Value.Num(0)
    }
    'supers - {
      eval("({ x: 1, y: self.x } + { x: 2 }).y") ==> Value.Num(2)
      eval("({ local x = $.y, y: 1, z: x} + { y: 2 }).z") ==> Value.Num(2)
      eval("({ local x = self.y, y: 1, z: x} + { y: 2 }).z") ==> Value.Num(2)
    }

//    'format - {
//      eval("\"%s\" % \"world\"") ==> Value.Str("world")
//      eval("\"%s\" % [\"world\"]") ==> Value.Str("world")
//      eval("\"%s %s\" % [\"hello\", \"world\"]") ==> Value.Str("hello world")
//      eval("\"%(hello)s\" % {hello: \"world\"}") ==> Value.Str("world")
//    }
  }
}

