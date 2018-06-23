package sjsonnet
import ujson.Js
object Materializer {
  def apply(v: Value): Js = v match{
    case Value.True => Js.True
    case Value.False => Js.False
    case Value.Null => Js.Null
    case Value.Num(n) => Js.Num(n)
    case Value.Str(s) => Js.Str(s)
    case Value.Arr(xs) => Js.Arr.from(xs.map(x => apply(x.calc)))
    case obj: Value.Obj =>

      def rec(current: Value.Obj): Seq[String] = {
        current.value0.keys.toSeq ++ current.`super`.toSeq.flatMap(rec)
      }
      val allKeys = rec(obj).distinct
      Js.Obj.from(allKeys.map{k => k -> apply(obj.value(k).calc)})
  }
}
