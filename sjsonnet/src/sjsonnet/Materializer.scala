package sjsonnet
import ujson.Js
object Materializer {
  def apply(v: Value): Js = v match{
    case Value.True => Js.True
    case Value.False => Js.False
    case Value.Null => Js.Null
    case Value.Num(n) => Js.Num(n)
    case Value.Str(s) => Js.Str(s)
    case Value.Arr(xs) => Js.Arr(xs.map(x => apply(x.calc)))
    case obj @ Value.Obj(kvs) => Js.Obj.from(kvs.mapValues(x => apply(x._2(obj).calc)))
  }
}
