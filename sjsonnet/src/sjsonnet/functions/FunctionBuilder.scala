package sjsonnet.functions

import sjsonnet._

/**
 * Function building helpers for building functions of the Jsonnet language.
 */
trait FunctionBuilder {

  def builtin(obj: Val.Builtin): (String, Val.Builtin) = (obj.functionName, obj)

  def builtin[R: ReadWriter](name: String)(eval: (Position, EvalScope) => R): (String, Val.Func) = {
    (
      name,
      new Val.Builtin0(name) {
        def evalRhs(ev: EvalScope, outerPos: Position): Val = {
          // println("--- calling builtin: "+name)
          implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev))
        }
      }
    )
  }

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)(
      eval: (Position, EvalScope, T1) => R): (String, Val.Func) = {
    (
      name,
      new Val.Builtin1(name, p1) {
        def evalRhs(arg1: Eval, ev: EvalScope, outerPos: Position): Val = {
          // println("--- calling builtin: "+name)
          val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.value)
          implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1))
        }
      }
    )
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)(
      eval: (Position, EvalScope, T1, T2) => R): (String, Val.Func) = {
    (
      name,
      new Val.Builtin2(name, p1, p2) {
        def evalRhs(arg1: Eval, arg2: Eval, ev: EvalScope, outerPos: Position): Val = {
          // println("--- calling builtin: "+name)
          val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.value)
          val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.value)
          implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2))
        }
      }
    )
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](
      name: String,
      p1: String,
      p2: String,
      p3: String)(eval: (Position, EvalScope, T1, T2, T3) => R): (String, Val.Func) = {
    (
      name,
      new Val.Builtin3(name, p1, p2, p3) {
        def evalRhs(arg1: Eval, arg2: Eval, arg3: Eval, ev: EvalScope, outerPos: Position): Val = {
          // println("--- calling builtin: "+name)
          val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.value)
          val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.value)
          val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.value)
          implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3))
        }
      }
    )
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter, T4: ReadWriter](
      name: String,
      p1: String,
      p2: String,
      p3: String,
      p4: String)(eval: (Position, EvalScope, T1, T2, T3, T4) => R): (String, Val.Func) = {
    (
      name,
      new Val.Builtin4(name, p1, p2, p3, p4) {
        def evalRhs(
            arg1: Eval,
            arg2: Eval,
            arg3: Eval,
            arg4: Eval,
            ev: EvalScope,
            outerPos: Position): Val = {
          // println("--- calling builtin: "+name)
          val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.value)
          val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.value)
          val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.value)
          val v4: T4 = implicitly[ReadWriter[T4]].apply(arg4.value)
          implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3, v4))
        }
      }
    )
  }

  /**
   * Helper function that can define a built-in function with default parameters
   *
   * Arguments of the eval function are (args, ev)
   */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Val.Literal)*)(
      eval: (Array[Val], Position, EvalScope) => R): (String, Val.Func) = {
    name -> new Val.Builtin(name, params.map(_._1).toArray, params.map(_._2).toArray) {
      def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
        implicitly[ReadWriter[R]].write(pos, eval(args.map(_.value), pos, ev))
    }
  }
}
