package sjsonnet
import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Lazy
import upickle.core.Visitor

import scala.collection.mutable

/**
  * Serializes the given [[Val]] out to the given [[upickle.core.Visitor]],
  * which can transform it into [[ujson.Value]]s or directly serialize it
  * to `String`s
  */
object Materializer {
  private val dummyPos: Position = new Position(null, 0)

  def apply(v: Val, storePos: Position => Unit = _ => ())(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    apply0(v, new sjsonnet.Renderer()).toString
  }

  def apply0[T](v: Val, visitor: Visitor[T, T], storePos: Position => Unit = _ => ())
               (implicit evaluator: EvalScope): T = try {
    v match {
      case Val.True(pos) => storePos(pos); visitor.visitTrue(-1)
      case Val.False(pos) => storePos(pos); visitor.visitFalse(-1)
      case Val.Null(pos) => storePos(pos); visitor.visitNull(-1)
      case Val.Num(pos, n) => storePos(pos); visitor.visitFloat64(n, -1)
      case Val.Str(pos, s) => storePos(pos); visitor.visitString(s, -1)
      case Val.Arr(pos, xs) =>
        storePos(pos);
        val arrVisitor = visitor.visitArray(xs.length, -1)
        for(x <- xs) {
          arrVisitor.visitValue(
            apply0(x.force, arrVisitor.subVisitor.asInstanceOf[Visitor[T, T]], storePos),
            -1
          )
        }
        arrVisitor.visitEnd(-1)

      case obj: Val.Obj =>
        storePos(obj.pos)
        obj.triggerAllAsserts(obj)

        val keysUnsorted = obj.getVisibleKeys().toArray
        val keys = if (!evaluator.preserveOrder) keysUnsorted.sortBy(_._1) else keysUnsorted
        val objVisitor = visitor.visitObject(keys.length , -1)

        for(t <- keys) {
          val (k, hidden) = t
          if (!hidden){
            val value = obj.value(k, new Position(evaluator.emptyMaterializeFileScope.currentFile, -1))(evaluator.emptyMaterializeFileScope, implicitly)

            storePos(
              value match{
                case v: Val.Obj if v.getVisibleKeys().nonEmpty => value.pos
                case v: Val.Arr if v.value.nonEmpty => value.pos
                case _ => null
              }
            )
            objVisitor.visitKeyValue(objVisitor.visitKey(-1).visitString(k, -1))



            objVisitor.visitValue(
              apply0(value, objVisitor.subVisitor.asInstanceOf[Visitor[T, T]], storePos),
              -1
            )
          }
        }
        objVisitor.visitEnd(-1)

      case f: Val.Func =>
        apply0(
          f.apply(emptyStringArray, emptyLazyArray, "(memory)", new Position(evaluator.emptyMaterializeFileScope.currentFile, -1))(evaluator.emptyMaterializeFileScope, implicitly),
          visitor,
          storePos
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
          (_: Val.Obj, _: Val.Obj, _, _) => reverse(pos, x._2)
        )
        builder += (x._1 -> v)
      }
      new Val.Obj(pos, builder.result(), _ => (), null)
  }

  def toExpr(v: ujson.Value): Expr = v match{
    case ujson.True => Expr.True(dummyPos)
    case ujson.False => Expr.False(dummyPos)
    case ujson.Null => Expr.Null(dummyPos)
    case ujson.Num(n) => Expr.Num(dummyPos, n)
    case ujson.Str(s) => Expr.Str(dummyPos, s)
    case ujson.Arr(xs) => Expr.Arr(dummyPos, xs.map(toExpr).toArray[Expr])
    case ujson.Obj(kvs) =>
      Expr.Obj(dummyPos,
        ObjBody.MemberList(
          for((k, v) <- kvs.toArray)
          yield Member.Field(dummyPos, FieldName.Fixed(k), false, None, Visibility.Normal, toExpr(v))
        )
      )
  }

  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Lazy](0)

}
