package sjsonnet
import java.util.IdentityHashMap

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import ujson.Js
object Materializer {
  def apply(v: Val,
            extVars: Map[String, ujson.Js],
            wd: Path): Js = try {
    v match {
      case Val.True => ujson.True
      case Val.False => ujson.False
      case Val.Null => ujson.Null
      case Val.Num(n) => ujson.Num(n)
      case Val.Str(s) => ujson.Str(s)
      case Val.Arr(xs) => ujson.Arr.from(xs.map(x => apply(x.force, extVars, wd)))

      case obj: Val.Obj =>
        def rec(x: Val.Obj): Unit = {
          x.triggerAsserts(obj)
          x.`super` match {
            case Some(s) => rec(s)
            case None => ()
          }
        }

        rec(obj)

        ujson.Obj.from(
          for {
            (k, hidden) <- obj.getVisibleKeys().toSeq.sortBy(_._1)
            if !hidden
          } yield k -> apply(obj.value(k, wd / "(Unknown)", wd, -1, wd, extVars).force, extVars, wd)
        )

      case f: Val.Func => apply(f.apply(Nil, "(memory)", extVars, -1, wd, wd / "(memory)"), extVars, wd)
    }
  }catch {case e: StackOverflowError =>
    throw DelegateError("Stackoverflow while materializing, possibly due to recursive value")
  }

  def reverse(v: ujson.Value): Val = v match{
    case ujson.True => Val.True
    case ujson.False => Val.False
    case ujson.Null => Val.Null
    case ujson.Num(n) => Val.Num(n)
    case ujson.Str(s) => Val.Str(s)
    case ujson.Arr(xs) => Val.Arr(xs.map(x => Lazy(reverse(x))).toSeq)
    case ujson.Obj(xs) => Val.Obj(
      xs.map(x => (x._1, Val.Obj.Member(false, Visibility.Normal, (_: Val.Obj, _: Option[Val.Obj], _) => Lazy(reverse(x._2))))).toMap,
      _ => (),
      None
    )
  }

  def toExpr(v: ujson.Js): Expr = v match{
    case ujson.True => Expr.True(0)
    case ujson.False => Expr.False(0)
    case ujson.Null => Expr.Null(0)
    case ujson.Num(n) => Expr.Num(0, n)
    case ujson.Str(s) => Expr.Str(0, s)
    case ujson.Arr(xs) => Expr.Arr(0, xs.map(toExpr).toSeq)
    case ujson.Obj(kvs) =>
      Expr.Obj(0,
        ObjBody.MemberList(
          for((k, v) <- kvs.toSeq)
            yield Member.Field(0, FieldName.Fixed(k), false, None, Visibility.Normal, toExpr(v))
        )
      )
  }

}
