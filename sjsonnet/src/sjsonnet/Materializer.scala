package sjsonnet
import java.util.IdentityHashMap

import ammonite.ops.Path
import sjsonnet.Expr.Member.Visibility
import ujson.Js
object Materializer {
  def apply(v: Val,
            extVars: Map[String, ujson.Js],
            wd: Path,
            seen: IdentityHashMap[Val, Unit] = new IdentityHashMap[Val, Unit]): Js = v match{
    case Val.True => Js.True
    case Val.False => Js.False
    case Val.Null => Js.Null
    case Val.Num(n) => Js.Num(n)
    case Val.Str(s) => Js.Str(s)
    case Val.Arr(xs) =>
      if (seen.containsKey(v)) throw new DelegateError("Failed to materialize recursive value")
      seen.put(v, ())
      val res = Js.Arr.from(xs.map(x => apply(x.force, extVars, wd, seen)))
      seen.remove(v, ())
      res
    case obj: Val.Obj =>
      if (seen.containsKey(v)) throw new DelegateError("Failed to materialize recursive value")
      seen.put(v, ())
      def rec(x: Val.Obj): Unit = {
        x.triggerAsserts(obj)
        x.`super` match{
          case Some(s) => rec(s)
          case None => Unit
        }
      }
      rec(obj)

      val res = Js.Obj.from(
        for {
          (k, hidden) <- obj.getVisibleKeys().toSeq.sortBy(_._1)
          if !hidden
        }yield k -> apply(obj.value(k, wd / "(Unknown)", wd, -1, wd, extVars).force, extVars, wd, seen)
      )
      seen.remove(v, ())
      res
    case f: Val.Func => apply(f.apply(Nil, "(memory)", extVars, -1, wd), extVars, wd, seen)
  }

  def reverse(v: Js.Value): Val = v match{
    case Js.True => Val.True
    case Js.False => Val.False
    case Js.Null => Val.Null
    case Js.Num(n) => Val.Num(n)
    case Js.Str(s) => Val.Str(s)
    case Js.Arr(xs) => Val.Arr(xs.map(x => Lazy(reverse(x))))
    case Js.Obj(xs) => Val.Obj(
      xs.map(x => (x._1, Val.Obj.Member(false, Visibility.Normal, (_: Val.Obj, _: Option[Val.Obj], thisFile: String) => Lazy(reverse(x._2))))).toMap,
      _ => (),
      None
    )
  }
}
