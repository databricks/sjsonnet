package sjsonnet

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import upickle.core.Visitor

/**
  * Serializes the given [[Val]] out to the given [[upickle.core.Visitor]],
  * which can transform it into [[ujson.Value]]s or directly serialize it
  * to `String`s
  */
abstract class Materializer {
  def storePos(pos: Position): Unit
  def storePos(v: Val): Unit

  def apply(v: Val)(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    apply0(v, new sjsonnet.Renderer()).toString
  }

  def apply0[T](v: Val, visitor: Visitor[T, T])
               (implicit evaluator: EvalScope): T = try {
    v match {
      case Val.Str(pos, s) => storePos(pos); visitor.visitString(s, -1)
      case obj: Val.Obj =>
        storePos(obj.pos)
        obj.triggerAllAsserts(obj)
        val objVisitor = visitor.visitObject(obj.visibleKeyNames.length , true, -1)
        val sort = !evaluator.settings.preserveOrder
        var prevKey: String = null
        obj.foreachElement(sort, evaluator.emptyMaterializeFileScopePos) { (k, v) =>
          storePos(v)
          objVisitor.visitKeyValue(objVisitor.visitKey(-1).visitString(k, -1))
          objVisitor.visitValue(
            apply0(v, objVisitor.subVisitor.asInstanceOf[Visitor[T, T]]),
            -1
          )
          if(sort) {
            if(prevKey != null && k.compareTo(prevKey) <= 0)
              Error.fail(s"""Internal error: Unexpected key "$k" after "$prevKey" in sorted object materialization""")
            prevKey = k
          }
        }
        objVisitor.visitEnd(-1)
      case Val.Num(pos, n) => storePos(pos); visitor.visitFloat64(n, -1)
      case xs: Val.Arr =>
        storePos(xs.pos);
        val arrVisitor = visitor.visitArray(xs.length, -1)
        var i = 0
        while(i < xs.length) {
          val sub = arrVisitor.subVisitor.asInstanceOf[Visitor[T, T]]
          arrVisitor.visitValue(apply0(xs.force(i), sub), -1)
          i += 1
        }
        arrVisitor.visitEnd(-1)
      case Val.True(pos) => storePos(pos); visitor.visitTrue(-1)
      case Val.False(pos) => storePos(pos); visitor.visitFalse(-1)
      case Val.Null(pos) => storePos(pos); visitor.visitNull(-1)
      case f: Val.Func =>
        apply0(
          f.apply(Materializer.emptyLazyArray, null, evaluator.emptyMaterializeFileScopePos),
          visitor
        )
    }

  }catch {case e: StackOverflowError =>
    Error.fail("Stackoverflow while materializing, possibly due to recursive value")
  }

  def reverse(pos: Position, v: ujson.Value): Val = v match{
    case ujson.True => Val.True(pos)
    case ujson.False => Val.False(pos)
    case ujson.Null => Val.Null(pos)
    case ujson.Num(n) => Val.Num(pos, n)
    case ujson.Str(s) => Val.Str(pos, s)
    case ujson.Arr(xs) => new Val.Arr(pos, xs.map(x => (() => reverse(pos, x)): Lazy).toArray[Lazy])
    case ujson.Obj(xs) =>
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      for(x <- xs) {
        val v = new Val.Obj.Member(false, Visibility.Normal) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = reverse(pos, x._2)
        }
        builder.put(x._1, v)
      }
      new Val.Obj(pos, builder, false, null, null)
  }

  def toExpr(v: ujson.Value)(implicit ev: EvalScope): Expr = v match{
    case ujson.True => Val.True(ev.emptyMaterializeFileScopePos)
    case ujson.False => Val.False(ev.emptyMaterializeFileScopePos)
    case ujson.Null => Val.Null(ev.emptyMaterializeFileScopePos)
    case ujson.Num(n) => Val.Num(ev.emptyMaterializeFileScopePos, n)
    case ujson.Str(s) => Val.Str(ev.emptyMaterializeFileScopePos, s)
    case ujson.Arr(xs) => Expr.Arr(ev.emptyMaterializeFileScopePos, xs.map(toExpr).toArray[Expr])
    case ujson.Obj(kvs) =>
      ObjBody.MemberList(
        ev.emptyMaterializeFileScopePos,
        null,
        for((k, v) <- kvs.toArray)
          yield Member.Field(ev.emptyMaterializeFileScopePos, FieldName.Fixed(k), false, null, Visibility.Normal, toExpr(v)),
        null
      )
  }

}

object Materializer extends Materializer {
  def storePos(pos: Position): Unit = ()
  def storePos(v: Val): Unit = ()

  final val emptyStringArray = new Array[String](0)
  final val emptyLazyArray = new Array[Lazy](0)
}