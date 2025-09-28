package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

class TypeModule extends AbstractFunctionModule {
  def name = "type"

  private object IsString extends Val.Builtin1("isString", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Str])
  }

  private object IsBoolean extends Val.Builtin1("isBoolean", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Bool])
  }

  private object IsNumber extends Val.Builtin1("isNumber", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Num])
  }

  private object IsObject extends Val.Builtin1("isObject", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Obj])
  }

  private object IsArray extends Val.Builtin1("isArray", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Arr])
  }

  private object IsFunction extends Val.Builtin1("isFunction", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, v.force.isInstanceOf[Val.Func])
  }

  private object Type extends Val.Builtin1("type", "x") {
    def evalRhs(x: Lazy, ev: EvalScope, pos: Position): Val = Val.Str(pos, x.force.prettyName)
  }

  private object AssertEqual extends Val.Builtin2("assertEqual", "a", "b") {
    def evalRhs(v1: Lazy, v2: Lazy, ev: EvalScope, pos: Position): Val = {
      val x1 = Materializer(v1.force)(ev)
      val x2 = Materializer(v2.force)(ev)
      if (x1 == x2) Val.True(pos)
      else Error.fail("assertEqual failed: " + x1 + " != " + x2)
    }
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(AssertEqual),
    builtin(IsString),
    builtin(IsBoolean),
    builtin(IsNumber),
    builtin(IsObject),
    builtin(IsArray),
    builtin(IsFunction),
    builtin(Type),
    builtin("equals", "a", "b") { (_, ev, a: Val, b: Val) =>
      ev.equal(a, b)
    },
    builtin("primitiveEquals", "x", "y") { (_, ev, x: Val, y: Val) =>
      val xForce = x.force
      val yForce = y.force
      if (xForce.prettyName != yForce.prettyName) {
        false
      } else {
        (xForce, yForce) match {
          case (_: Val.Num, _: Val.Num) =>
            ev.compare(xForce, yForce) == 0
          case (_: Val.Str, _: Val.Str) =>
            ev.compare(xForce, yForce) == 0
          case (_: Val.Bool, _: Val.Bool) =>
            ev.compare(xForce, yForce) == 0
          case (_: Val.Null, _) =>
            true
          case (_, _: Val.Null) =>
            true
          case _ =>
            Error.fail(
              "primitiveEquals operates on primitive types, got " + xForce.prettyName + " and " + yForce.prettyName
            )
        }
      }
    }
  )
}
