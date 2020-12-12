package sjsonnet
import CurrentPos.currentPos
import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import upickle.core.Visitor

import scala.collection.mutable

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
      case Val.True(pos) => currentPos.set(pos); visitor.visitTrue(-1)
      case Val.False(pos) => currentPos.set(pos); visitor.visitFalse(-1)
      case Val.Null(pos) => currentPos.set(pos); visitor.visitNull(-1)
      case Val.Num(pos, n) => currentPos.set(pos); visitor.visitFloat64(n, -1)
      case Val.Str(pos, s) => currentPos.set(pos); visitor.visitString(s, -1)
      case Val.Arr(pos, xs) =>
        currentPos.set(pos);
        val arrVisitor = visitor.visitArray(xs.length, -1)
        for(x <- xs) {
          arrVisitor.visitValue(
            apply0(x.force, arrVisitor.subVisitor.asInstanceOf[Visitor[T, T]]),
            -1
          )
        }
        arrVisitor.visitEnd(-1)

      case obj: Val.Obj =>
        currentPos.set(obj.pos)
        obj.triggerAllAsserts(obj)

        val keysUnsorted = obj.getVisibleKeys().toArray
        val keys = if (!evaluator.preserveOrder) keysUnsorted.sortBy(_._1) else keysUnsorted
        val objVisitor = visitor.visitObject(keys.length , -1)

        for(t <- keys) {
          val (k, hidden) = t
          if (!hidden){
            val value = obj.value(k, -1)(evaluator.emptyMaterializeFileScope, implicitly)

            currentPos.set(
              value match{
                case v: Val.Obj if v.getVisibleKeys().nonEmpty => value.pos
                case v: Val.Arr if v.value.nonEmpty => value.pos
                case _ => null
              }
            )
            objVisitor.visitKeyValue(objVisitor.visitKey(-1).visitString(k, -1))



            objVisitor.visitValue(
              apply0(value, objVisitor.subVisitor.asInstanceOf[Visitor[T, T]]
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

  def reverse(pos: Position, v: ujson.Value): Val = v match{
    case ujson.True => Val.True(pos)
    case ujson.False => Val.False(pos)
    case ujson.Null => Val.Null(pos)
    case ujson.Num(n) => Val.Num(pos, n)
    case ujson.Str(s) => Val.Str(pos, s)
    case ujson.Arr(xs) => Val.Arr(pos, xs.map(x => Val.Lazy(reverse(pos, x))).toArray[Val.Lazy])
    case ujson.Obj(xs) =>
      val builder = mutable.LinkedHashMap.newBuilder[String, Val.Obj.Member]
      for(x <- xs){
        val v = Val.Obj.Member(false, Visibility.Normal,
          (_: Val.Obj, _: Option[Val.Obj], _, _) => reverse(pos, x._2)
        )
        builder += (x._1 -> v)
      }
      new Val.Obj(pos, builder.result(), _ => (), None)
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
