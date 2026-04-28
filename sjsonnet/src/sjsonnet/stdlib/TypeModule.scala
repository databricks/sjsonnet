package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

object TypeModule extends AbstractFunctionModule {
  def name = "type"

  private object IsString extends Val.Builtin1("isString", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Str])
  }

  private object IsBoolean extends Val.Builtin1("isBoolean", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Bool])
  }

  private object IsNumber extends Val.Builtin1("isNumber", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Num])
  }

  private object IsObject extends Val.Builtin1("isObject", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Obj])
  }

  private object IsArray extends Val.Builtin1("isArray", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Arr])
  }

  private object IsFunction extends Val.Builtin1("isFunction", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Func])
  }

  private object IsNull extends Val.Builtin1("isNull", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Null])
  }

  private object Type extends Val.Builtin1("type", "x") {
    def evalRhs(x: Eval, ev: EvalScope, pos: Position): Val = Val.Str(pos, x.value.prettyName)
  }

  private object AssertEqual extends Val.Builtin2("assertEqual", "a", "b") {
    def evalRhs(v1: Eval, v2: Eval, ev: EvalScope, pos: Position): Val = {
      val a = v1.value
      val b = v2.value
      // Use structural equality first (avoids double materialization on success path)
      if (ev.equal(a, b)) Val.True(pos)
      else {
        // Only materialize on failure for the error message
        val x1 = Materializer(a)(ev)
        val x2 = Materializer(b)(ev)
        Error.fail("assertEqual failed: " + x1 + " != " + x2)
      }
    }
  }

  private object Compare extends Val.Builtin2("__compare", "v1", "v2") {
    def evalRhs(v1: Eval, v2: Eval, ev: EvalScope, pos: Position): Val =
      Val.cachedNum(pos, Util.compareJsonnetStd(v1.value, v2.value, ev).toDouble)
  }

  private object CompareArray extends Val.Builtin2("__compare_array", "arr1", "arr2") {
    def evalRhs(arr1: Eval, arr2: Eval, ev: EvalScope, pos: Position): Val =
      Val.cachedNum(
        pos,
        Util.compareJsonnetStdArrays(arr1.value.asArr, arr2.value.asArr, ev).toDouble
      )
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(AssertEqual),
    builtin(IsString),
    builtin(IsBoolean),
    builtin(IsNumber),
    builtin(IsObject),
    builtin(IsArray),
    builtin(IsFunction),
    builtin(IsNull),
    builtin(Type),
    builtin("equals", "a", "b") { (_, ev, a: Val, b: Val) =>
      ev.equal(a, b)
    },
    builtin("primitiveEquals", "x", "y") { (_, ev, x: Val, y: Val) =>
      if (x.prettyName != y.prettyName) {
        false
      } else {
        (x, y) match {
          case (_: Val.Num, _: Val.Num) =>
            ev.compare(x, y) == 0
          case (_: Val.Str, _: Val.Str) =>
            ev.compare(x, y) == 0
          case (_: Val.Bool, _: Val.Bool) =>
            ev.compare(x, y) == 0
          case (_: Val.Null, _) =>
            true
          case (_, _: Val.Null) =>
            true
          case _ =>
            Error.fail(
              "primitiveEquals operates on primitive types, got " + x.prettyName + " and " + y.prettyName
            )
        }
      }
    },
    builtin(Compare),
    builtin(CompareArray),
    builtin("__array_less", "arr1", "arr2") { (_, ev, arr1: Val.Arr, arr2: Val.Arr) =>
      Util.compareJsonnetStdArrays(arr1, arr2, ev) == -1
    },
    builtin("__array_greater", "arr1", "arr2") { (_, ev, arr1: Val.Arr, arr2: Val.Arr) =>
      Util.compareJsonnetStdArrays(arr1, arr2, ev) == 1
    },
    builtin("__array_less_or_equal", "arr1", "arr2") { (_, ev, arr1: Val.Arr, arr2: Val.Arr) =>
      Util.compareJsonnetStdArrays(arr1, arr2, ev) <= 0
    },
    builtin("__array_greater_or_equal", "arr1", "arr2") { (_, ev, arr1: Val.Arr, arr2: Val.Arr) =>
      Util.compareJsonnetStdArrays(arr1, arr2, ev) >= 0
    }
  )
}
