package sjsonnet.stdlib

import sjsonnet.functions.AbstractFunctionModule
import sjsonnet.{Error, Eval, EvalScope, Platform, Position, Val}

object NativeXz extends AbstractFunctionModule {
  def name = "xz"

  val functions: Seq[(String, Val.Builtin)] = Seq(
    "xz" -> new Val.Builtin2(
      "xz",
      "v",
      "compressionLevel",
      Array(Val.Null(dummyPos), Val.Null(dummyPos))
    ) {
      override def evalRhs(arg1: Eval, arg2: Eval, ev: EvalScope, pos: Position): Val = {
        val compressionLevel: Option[Int] = arg2.value match {
          case Val.Null(_) =>
            // Use default compression level if the user didn't set one
            None
          case Val.Num(_, n) =>
            Some(n.toInt)
          case x =>
            Error.fail("Cannot xz encode with compression level " + x.prettyName)
        }
        arg1.value match {
          case Val.Str(_, value) => Val.Str(pos, Platform.xzString(value, compressionLevel))
          case arr: Val.Arr      =>
            Val.Str(
              pos,
              Platform.xzBytes(
                arr.iterator.map(_.cast[Val.Num].asInt.toByte).toArray,
                compressionLevel
              )
            )
          case x => Error.fail("Cannot xz encode " + x.prettyName)
        }
      }
    }
  )
}
