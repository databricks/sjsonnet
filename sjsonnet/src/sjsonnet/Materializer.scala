package sjsonnet
import java.lang.ThreadLocal
import java.util.IdentityHashMap

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import ujson.Js
object Materializer {
  def apply(v: Val,
            extVars: Map[String, ujson.Js],
            wd: os.Path): Js = {
    val originalFastPath = useFastPath.get
    useFastPath.set(false)
    try {
      val seen = if (originalFastPath) {
        None
      } else {
        Some(new IdentityHashMap[Val, Unit]())
      }
      apply0(v, extVars, wd, seen)
    } finally {
      useFastPath.set(originalFastPath)
    }
  }

  def apply0(v: Val,
             extVars: Map[String, ujson.Js],
             wd: os.Path,
             seen: Option[IdentityHashMap[Val, Unit]]): Js = v match{
    case Val.True => Js.True
    case Val.False => Js.False
    case Val.Null => Js.Null
    case Val.Num(n) => Js.Num(n)
    case Val.Str(s) => Js.Str(s)
    case arr @ Val.Arr(xs) =>
      seen match {
        case Some(seen) =>
          if (seen.containsKey(arr)) throw new DelegateError("Failed to materialize recursive value")
          seen.put(arr, ())
        case None =>
          if (arr.observed) throw new DelegateError("Failed to materialize recursive value")
          arr.observed = true
      }
      try {
        Js.Arr.from(xs.map(x => apply0(x.force, extVars, wd, seen)))
      } finally {
        seen match {
          case Some(seen) => seen.remove(arr)
          case None => arr.observed = false
        }
      }
    case obj: Val.Obj =>
      seen match {
        case Some(seen) =>
          if (seen.containsKey(obj)) throw new DelegateError("Failed to materialize recursive value")
          seen.put(obj, ())
        case None =>
          if (obj.observed) throw new DelegateError("Failed to materialize recursive value")
          obj.observed = true
      }

      obj.observed = true
      def rec(x: Val.Obj): Unit = {
        x.triggerAsserts(obj)
        x.`super` match{
          case Some(s) => rec(s)
          case None => Unit
        }
      }
      try {
        rec(obj)

        Js.Obj.from(
          for {
            (k, hidden) <- obj.getVisibleKeys().toSeq.sortBy(_._1)
            if !hidden
          }yield k -> apply0(obj.value(k, wd / "(Unknown)", wd, -1, wd, extVars).force, extVars, wd, seen)
        )
      } finally {
        seen match {
          case Some(seen) => seen.remove(obj)
          case None => obj.observed = false
        }
      }
    case f: Val.Func => apply(f.apply(Nil, "(memory)", extVars, -1, wd), extVars, wd)
  }

  def reverse(v: Js.Value): Val = v match{
    case Js.True => Val.True
    case Js.False => Val.False
    case Js.Null => Val.Null
    case Js.Num(n) => Val.Num(n)
    case Js.Str(s) => Val.Str(s)
    case Js.Arr(xs) => Val.Arr(xs.map(x => Lazy(reverse(x))))
    case Js.Obj(xs) => Val.Obj(
      xs.map(x => (x._1, Val.Obj.Member(false, Visibility.Normal, (_: Val.Obj, _: Option[Val.Obj], _) => Lazy(reverse(x._2))))).toMap,
      _ => (),
      None
    )
  }

  def toExpr(v: ujson.Js): Expr = v match{
    case ujson.Js.True => Expr.True(0)
    case ujson.Js.False => Expr.False(0)
    case ujson.Js.Null => Expr.Null(0)
    case ujson.Js.Num(n) => Expr.Num(0, n)
    case ujson.Js.Str(s) => Expr.Str(0, s)
    case ujson.Js.Arr(xs) => Expr.Arr(0, xs.map(toExpr))
    case ujson.Js.Obj(kvs) =>
      Expr.Obj(0,
        ObjBody.MemberList(
          for((k, v) <- kvs.toSeq)
            yield Member.Field(0, FieldName.Fixed(k), false, None, Visibility.Normal, toExpr(v))
        )
      )
  }

  val useFastPath = new ThreadLocal[Boolean]()
  useFastPath.set(true)
}
