package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isString(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isBoolean(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isNumber(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isObject(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isArray(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.isFunction(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-isNull std.isNull(x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-type std.type(x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-assertEqual std.assertEqual(a, b)]]
 */
object TypeModule extends AbstractFunctionModule {
  def name = "type"

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isString(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type string. The official std.type docs
   * list this helper alongside std.type.
   */
  private object IsString extends Val.Builtin1("isString", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Str])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isBoolean(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type boolean. The official std.type
   * docs list this helper alongside std.type.
   */
  private object IsBoolean extends Val.Builtin1("isBoolean", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Bool])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isNumber(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type number. The official std.type docs
   * list this helper alongside std.type.
   */
  private object IsNumber extends Val.Builtin1("isNumber", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Num])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isObject(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type object. The official std.type docs
   * list this helper alongside std.type.
   */
  private object IsObject extends Val.Builtin1("isObject", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Obj])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isArray(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type array. The official std.type docs
   * list this helper alongside std.type.
   */
  private object IsArray extends Val.Builtin1("isArray", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Arr])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.isFunction(v)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a boolean indicating whether the given value has type function. The official std.type
   * docs list this helper alongside std.type.
   */
  private object IsFunction extends Val.Builtin1("isFunction", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Func])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-isNull std.isNull(x)]].
   *
   * Since: 0.22.0. Group: Types and Reflection.
   *
   * Returns true if the given value is null, false otherwise.
   */
  private object IsNull extends Val.Builtin1("isNull", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(v.value.isInstanceOf[Val.Null])
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-type std.type(x)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Return a string that indicates the type of the value. The possible return values are: "array",
   * "boolean", "function", "null", "number", "object", and "string".
   *
   * The following functions are also available and return a boolean: std.isArray(v),
   * std.isBoolean(v), std.isFunction(v), std.isNumber(v), std.isObject(v), and std.isString(v).
   */
  private object Type extends Val.Builtin1("type", "x") {
    def evalRhs(x: Eval, ev: EvalScope, pos: Position): Val = Val.Str(pos, x.value.prettyName)
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-assertEqual std.assertEqual(a, b)]].
   *
   * Since: 0.10.0. Group: Assertions and Debugging.
   *
   * Ensure that a == b. Returns true or throws an error message.
   */
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
