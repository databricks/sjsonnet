package sjsonnet.stdlib

import sjsonnet.{Error, EvalScope, Lazy, Platform, Position, Val}

object NativeGzip {
  def functions: Map[String, Val.Builtin] = Map(
    "gzip" -> new Val.Builtin1("gzip", "v") {
      override def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = v.force match {
        case Val.Str(_, value) => Val.Str(pos, Platform.gzipString(value))
        case arr: Val.Arr =>
          Val.Str(pos, Platform.gzipBytes(arr.iterator.map(_.cast[Val.Num].asInt.toByte).toArray))
        case x => Error.fail("Cannot gzip encode " + x.prettyName)
      }
    },
  )
}
