package sjsonnet.stdlib

import sjsonnet.functions.AbstractFunctionModule
import sjsonnet.{Error, EvalScope, Lazy, Platform, Position, Val}

object NativeXz extends AbstractFunctionModule {
  def name = "xz"

  private val dummyPos: Position = new Position(null, 0)

  val functions: Seq[(String, Val.Builtin)] = Seq(
    "xz" -> new Val.Builtin2(
      "xz",
      "v",
      "compressionLevel",
      Array(Val.Null(dummyPos), Val.Null(dummyPos))
    ) {
      override def evalRhs(arg1: Lazy, arg2: Lazy, ev: EvalScope, pos: Position): Val = {
        val compressionLevel: Option[Int] = arg2.force match {
          case Val.Null(_) =>
            // Use default compression level if the user didn't set one
            None
          case Val.Num(_, n) =>
            Some(n.toInt)
          case x =>
            Error.fail("Cannot xz encode with compression level " + x.prettyName)
        }
        arg1.force match {
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
