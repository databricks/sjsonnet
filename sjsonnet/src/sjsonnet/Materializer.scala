package sjsonnet
import java.util.IdentityHashMap

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import upickle.core.Visitor
object Materializer {
  def apply(v: Val)(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)

  def apply0[T](v: Val, visitor: Visitor[T, T])
               (implicit evaluator: EvalScope): T = try {
    v match {
      case Val.True => visitor.visitTrue(-1)
      case Val.False => visitor.visitFalse(-1)
      case Val.Null => visitor.visitNull(-1)
      case Val.Num(n) => visitor.visitFloat64(n, -1)
      case Val.Str(s) => visitor.visitString(s, -1)
      case Val.Arr(xs) =>
        val arrVisitor = visitor.visitArray(xs.length, -1)
        for(x <- xs) {
          arrVisitor.visitValue(
            apply0(x.force, visitor),
            -1
          )
        }
        arrVisitor.visitEnd(-1)

      case obj: Val.Obj =>
        def rec(x: Val.Obj): Unit = {
          x.triggerAsserts(obj)
          x.`super` match {
            case Some(s) => rec(s)
            case None => ()
          }
        }

        rec(obj)
        val keys = obj.getVisibleKeys().toSeq.sortBy(_._1)
        val objVisitor = visitor.visitObject(keys.size, -1)

        for((k, hidden) <- keys if !hidden){
          objVisitor.visitKey(-1)
          objVisitor.visitKeyValue(k)
          objVisitor.visitValue(
            apply0(
              obj.value(k, -1)(
                new FileScope(null, null, Map()),
                implicitly
              ),
              visitor
            ),
            -1
          )
        }
        objVisitor.visitEnd(-1)

      case f: Val.Func =>
        apply0(
          f.apply(Nil, "(memory)", -1)(
            new FileScope(null, null, Map()),
            implicitly
          ),
          visitor
        )
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
      xs.map(x => (x._1, Val.Obj.Member(false, Visibility.Normal, (_: Val.Obj, _: Option[Val.Obj], _) => reverse(x._2)))).toMap,
      _ => (),
      None
    )
  }

  def toExpr(v: ujson.Value): Expr = v match{
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
