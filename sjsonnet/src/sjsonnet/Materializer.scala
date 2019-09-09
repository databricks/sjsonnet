package sjsonnet

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import upickle.core.Visitor

/**
  * Serializes the given [[Val]] out to the given [[upickle.core.Visitor]],
  * which can transform it into [[ujson.Value]]s or directly serialize it
  * to `String`s
  */
object Materializer {
  def apply(v: Val)(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    apply0(v, new sjsonnet.Renderer()).toString
  }

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
        obj.triggerAllAsserts(obj)

        val keys = obj.getVisibleKeys().toArray.sortBy(_._1)
        val objVisitor = visitor.visitObject(keys.length , -1)

        for(t <- keys) {
          val (k, hidden) = t
          if (!hidden){
            objVisitor.visitKeyValue(objVisitor.visitKey(-1).visitString(k, -1))
            objVisitor.visitValue(
              apply0(
                obj.value(k, -1)(evaluator.emptyMaterializeFileScope, implicitly),
                visitor
              ),
              -1
            )
          }
        }
        objVisitor.visitEnd(-1)

      case f: Val.Func =>
        apply0(
          f.apply(Nil, "(memory)", -1)(evaluator.emptyMaterializeFileScope, implicitly),
          visitor
        )
    }

  }catch {case e: StackOverflowError =>
    throw Error.Delegate("Stackoverflow while materializing, possibly due to recursive value")
  }

  def reverse(v: ujson.Value): Val = v match{
    case ujson.True => Val.True
    case ujson.False => Val.False
    case ujson.Null => Val.Null
    case ujson.Num(n) => Val.Num(n)
    case ujson.Str(s) => Val.Str(s)
    case ujson.Arr(xs) => Val.Arr(xs.map(x => Val.Lazy(reverse(x))).toArray[Val.Lazy])
    case ujson.Obj(xs) =>
      val builder = Map.newBuilder[String, Val.Obj.Member]
      for(x <- xs){
        val v = Val.Obj.Member(false, Visibility.Normal,
          (_: Val.Obj, _: Option[Val.Obj], _, _) => reverse(x._2)
        )
        builder += (x._1 -> v)
      }
      new Val.Obj(builder.result(), _ => (), None)
  }

  def toExpr(v: ujson.Value): Expr = v match{
    case ujson.True => Expr.True(0)
    case ujson.False => Expr.False(0)
    case ujson.Null => Expr.Null(0)
    case ujson.Num(n) => Expr.Num(0, n)
    case ujson.Str(s) => Expr.Str(0, s)
    case ujson.Arr(xs) => Expr.Arr(0, xs.map(toExpr).toArray[Expr])
    case ujson.Obj(kvs) =>
      Expr.Obj(0,
        ObjBody.MemberList(
          for((k, v) <- kvs.toArray)
          yield Member.Field(0, FieldName.Fixed(k), false, None, Visibility.Normal, toExpr(v))
        )
      )
  }

}
