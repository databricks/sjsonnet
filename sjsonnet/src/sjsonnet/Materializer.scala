package sjsonnet
import sjsonnet.Expr.Member.Visibility
import ujson.Js
object Materializer {
  def apply(v: Val): Js = v match{
    case Val.True => Js.True
    case Val.False => Js.False
    case Val.Null => Js.Null
    case Val.Num(n) => Js.Num(n)
    case Val.Str(s) => Js.Str(s)
    case Val.Arr(xs) => Js.Arr.from(xs.map(x => apply(x.calc)))
    case obj: Val.Obj =>
      Js.Obj.from(
        for {
          (k, hidden) <- obj.getVisibleKeys().toSeq.sortBy(_._1)
          if !hidden
        }yield k -> apply(obj.value(k, ammonite.ops.pwd / "(Unknown)", -1).calc)
      )
  }

  def reverse(v: Js.Value): Val = v match{
    case Js.True => Val.True
    case Js.False => Val.False
    case Js.Null => Val.Null
    case Js.Num(n) => Val.Num(n)
    case Js.Str(s) => Val.Str(s)
    case Js.Arr(xs) => Val.Arr(xs.map(x => Ref(reverse(x))))
    case Js.Obj(xs) => Val.Obj(
      xs.map(x => (x._1, Val.Obj.Member(false, Visibility.Normal, (_: Val.Obj, _: Option[Val.Obj]) => Ref(reverse(x._2))))).toMap, None
    )
  }
}
