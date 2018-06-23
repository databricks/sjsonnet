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
      Js.Obj.from(
        for {
          (k, hidden) <- obj.getVisibleKeys().toSeq.sortBy(_._1)
          if !hidden
        }yield k -> apply(obj.value(k).calc)
      )
  }

  def reverse(v: Js.Value): Value = v match{
    case Js.True => Value.True
    case Js.False => Value.False
    case Js.Null => Value.Null
    case Js.Num(n) => Value.Num(n)
    case Js.Str(s) => Value.Str(s)
    case Js.Arr(xs) => Value.Arr(xs.map(x => Ref(reverse(x))))
    case Js.Obj(xs) => Value.Obj(
      xs.map(x => (x._1, (false, ":", (_: Value.Obj, _: Option[Value.Obj]) => Ref(reverse(x._2))))).toMap, None
    )
  }
}
