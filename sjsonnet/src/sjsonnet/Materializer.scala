package sjsonnet

import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/**
 * Serializes the given [[Val]] out to the given [[upickle.core.Visitor]], which can transform it
 * into [[ujson.Value]]s or directly serialize it to `String`s.
 *
 * TCO boundary: all [[Val]] values entering materialization — whether from object field evaluation
 * (`Val.Obj.value`), array element forcing (`Val.Arr.value`), or top-level evaluation — must not
 * contain unresolved [[TailCall]] sentinels. This invariant is maintained by the evaluator: object
 * field `invoke` calls `visitExpr` (not `visitExprWithTailCallSupport`), and `Val.Func.apply*`
 * resolves TailCalls when called with `TailstrictModeDisabled`. A defensive check in
 * `materializeLeaf` guards against accidental TailCall leakage with a clear internal-error
 * diagnostic.
 */
abstract class Materializer {
  def storePos(pos: Position): Unit
  def storePos(v: Val): Unit

  def apply(v: Val)(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    apply0(v, new sjsonnet.Renderer()).toString
  }

  /**
   * Materialize a leaf value (non-container) to the given visitor. Callers must ensure that
   * container values (Obj/Arr) are never passed to this method — they are handled by the iterative
   * stack-based loop in [[materializeContainer]]. Passing a container will fall through to the
   * catch-all branch and throw an error.
   */
  private def materializeLeaf[T](
      v: Val,
      visitor: Visitor[T, T])(implicit evaluator: EvalScope): T = {
    v match {
      case Val.Str(pos, s)                  => storePos(pos); visitor.visitString(s, -1)
      case Val.Num(pos, _)                  => storePos(pos); visitor.visitFloat64(v.asDouble, -1)
      case Val.True(pos)                    => storePos(pos); visitor.visitTrue(-1)
      case Val.False(pos)                   => storePos(pos); visitor.visitFalse(-1)
      case Val.Null(pos)                    => storePos(pos); visitor.visitNull(-1)
      case mat: Materializer.Materializable => storePos(v.pos); mat.materialize(visitor)
      case s: Val.Func                      =>
        Error.fail(
          "Couldn't manifest function with params [" + s.params.names.mkString(",") + "]",
          v.pos
        )
      case tc: TailCall =>
        Error.fail(
          "Internal error: TailCall sentinel leaked into materialization. " +
          "This indicates a bug in the TCO protocol — a TailCall was not resolved before " +
          "reaching the Materializer.",
          tc.pos
        )
      case vv: Val =>
        Error.fail("Unknown value type " + vv.prettyName, vv.pos)
      case null =>
        Error.fail("Unknown value type " + v)
    }
  }

  /**
   * Hybrid materialization: uses JVM stack recursion for shallow nesting (zero heap allocation,
   * JIT-friendly) and automatically switches to an explicit stack-based iterative loop when the
   * recursion depth exceeds [[Settings.materializeRecursiveDepthLimit]]. This gives optimal
   * performance for the 99.9% common case while still handling arbitrarily deep structures (e.g.
   * those built via TCO) without StackOverflowError.
   */
  def apply0[T](v: Val, visitor: Visitor[T, T])(implicit evaluator: EvalScope): T = v match {
    case obj: Val.Obj => materializeRecursive(obj, visitor, 0)
    case xs: Val.Arr  => materializeRecursive(xs, visitor, 0)
    case _            => materializeLeaf(v, visitor)
  }

  // Recursive materialization for shallow nesting. Each container consumes one JVM stack frame.
  // When depth reaches settings.materializeRecursiveDepthLimit, switches to the iterative materializeContainer to
  // avoid StackOverflowError. The method is kept small to encourage JIT inlining.
  private def materializeRecursive[T](v: Val, visitor: Visitor[T, T], depth: Int)(implicit
      evaluator: EvalScope): T = {
    val sort = !evaluator.settings.preserveOrder
    val brokenAssertionLogic = evaluator.settings.brokenAssertionLogic
    val emptyPos = evaluator.emptyMaterializeFileScopePos
    v match {
      case obj: Val.Obj =>
        storePos(obj.pos)
        obj.triggerAllAsserts(brokenAssertionLogic)
        val keys =
          if (sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
          else obj.visibleKeyNames
        val ov = visitor.visitObject(keys.length, jsonableKeys = true, -1)
        var i = 0
        var prevKey: String = null
        while (i < keys.length) {
          val key = keys(i)
          val childVal = obj.value(key, emptyPos)
          storePos(childVal)
          if (sort) {
            if (prevKey != null && Util.compareStringsByCodepoint(key, prevKey) <= 0)
              Error.fail(
                s"""Internal error: Unexpected key "$key" after "$prevKey" in sorted object materialization""",
                childVal.pos
              )
            prevKey = key
          }
          ov.visitKeyValue(ov.visitKey(-1).visitString(key, -1))
          val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
          ov.visitValue(materializeRecursiveChild(childVal, sub, depth), -1)
          i += 1
        }
        ov.visitEnd(-1)
      case xs: Val.Arr =>
        storePos(xs.pos)
        val av = visitor.visitArray(xs.length, -1)
        var i = 0
        while (i < xs.length) {
          val childVal = xs.value(i)
          val sub = av.subVisitor.asInstanceOf[Visitor[T, T]]
          av.visitValue(materializeRecursiveChild(childVal, sub, depth), -1)
          i += 1
        }
        av.visitEnd(-1)
      case _ =>
        materializeLeaf(v, visitor)
    }
  }

  // Materialize a child value during recursive mode. Leaf values are handled directly;
  // container children either recurse (if depth < limit) or switch to iterative mode.
  private def materializeRecursiveChild[T](childVal: Val, childVisitor: Visitor[T, T], depth: Int)(
      implicit evaluator: EvalScope): T = {
    if (!childVal.isInstanceOf[Val.Obj] && !childVal.isInstanceOf[Val.Arr]) {
      materializeLeaf(childVal, childVisitor)
    } else {
      val nextDepth = depth + 1
      if (nextDepth < evaluator.settings.materializeRecursiveDepthLimit)
        materializeRecursive(childVal, childVisitor, nextDepth)
      else
        materializeContainer(childVal, childVisitor)
    }
  }

  // Iterative materialization for deep nesting. Used as a fallback when recursive depth exceeds
  // settings.materializeRecursiveDepthLimit. Uses an explicit ArrayDeque stack to avoid StackOverflowError.
  private def materializeContainer[T](v: Val, visitor: Visitor[T, T])(implicit
      evaluator: EvalScope): T = {
    try {
      val maxDepth = evaluator.settings.maxMaterializeDepth
      val sort = !evaluator.settings.preserveOrder
      val brokenAssertionLogic = evaluator.settings.brokenAssertionLogic
      val emptyPos = evaluator.emptyMaterializeFileScopePos
      val stack = new java.util.ArrayDeque[Materializer.MaterializeFrame](
        evaluator.settings.materializeRecursiveDepthLimit << 2
      )

      // Push the initial container frame
      v match {
        case obj: Val.Obj => pushObjFrame(obj, visitor, stack, maxDepth, sort, brokenAssertionLogic)
        case xs: Val.Arr  => pushArrFrame(xs, visitor, stack, maxDepth)
        case _            => () // unreachable
      }

      while (true) {
        stack.peekFirst() match {
          case frame: Materializer.MaterializeObjFrame[T @unchecked] =>
            val keys = frame.keys
            val ov = frame.objVisitor
            if (frame.index < keys.length) {
              val key = keys(frame.index)
              val childVal = frame.obj.value(key, emptyPos)
              storePos(childVal)

              if (frame.sort) {
                if (
                  frame.prevKey != null && Util.compareStringsByCodepoint(key, frame.prevKey) <= 0
                )
                  Error.fail(
                    s"""Internal error: Unexpected key "$key" after "${frame.prevKey}" in sorted object materialization""",
                    childVal.pos
                  )
                frame.prevKey = key
              }

              ov.visitKeyValue(ov.visitKey(-1).visitString(key, -1))
              frame.index += 1

              val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
              materializeChild(childVal, sub, ov, stack, maxDepth, sort, brokenAssertionLogic)
            } else {
              val result = ov.visitEnd(-1)
              stack.removeFirst()
              if (stack.isEmpty) return result
              feedResult(stack.peekFirst(), result)
            }

          case frame: Materializer.MaterializeArrFrame[T @unchecked] =>
            val arr = frame.arr
            val av = frame.arrVisitor
            if (frame.index < arr.length) {
              val childVal = arr.value(frame.index)
              frame.index += 1

              val sub = av.subVisitor.asInstanceOf[Visitor[T, T]]
              materializeChild(childVal, sub, av, stack, maxDepth, sort, brokenAssertionLogic)
            } else {
              val result = av.visitEnd(-1)
              stack.removeFirst()
              if (stack.isEmpty) return result
              feedResult(stack.peekFirst(), result)
            }
        }
      }

      null.asInstanceOf[T] // unreachable — while(true) exits via return
    } catch {
      case _: StackOverflowError =>
        Error.fail("Stackoverflow while materializing, possibly due to recursive value", v.pos)
      case _: OutOfMemoryError =>
        Error.fail("Out of memory while materializing, possibly due to recursive value", v.pos)
    }
  }

  // Materialize a child value in iterative mode: leaf fast-path avoids a full pattern match for
  // the common case (strings, numbers, booleans, null). Only containers push a new frame.
  private def materializeChild[T](
      childVal: Val,
      childVisitor: Visitor[T, T],
      parentVisitor: upickle.core.ObjArrVisitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      maxDepth: Int,
      sort: Boolean,
      brokenAssertionLogic: Boolean)(implicit evaluator: EvalScope): Unit = {
    if (!childVal.isInstanceOf[Val.Obj] && !childVal.isInstanceOf[Val.Arr]) {
      parentVisitor.visitValue(materializeLeaf(childVal, childVisitor), -1)
    } else
      childVal match {
        case obj: Val.Obj =>
          pushObjFrame(obj, childVisitor, stack, maxDepth, sort, brokenAssertionLogic)
        case xs: Val.Arr =>
          pushArrFrame(xs, childVisitor, stack, maxDepth)
        case _ => () // unreachable — guarded by isInstanceOf checks above
      }
  }

  private def pushObjFrame[T](
      obj: Val.Obj,
      visitor: Visitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      maxDepth: Int,
      sort: Boolean,
      brokenAssertionLogic: Boolean)(implicit evaluator: EvalScope): Unit = {
    checkDepth(obj.pos, stack.size, maxDepth)
    storePos(obj.pos)
    obj.triggerAllAsserts(brokenAssertionLogic)
    val keyNames =
      if (sort) obj.visibleKeyNames.sorted(Util.CodepointStringOrdering)
      else obj.visibleKeyNames
    val objVisitor = visitor.visitObject(keyNames.length, jsonableKeys = true, -1)
    stack.push(new Materializer.MaterializeObjFrame[T](objVisitor, keyNames, obj, sort, 0, null))
  }

  private def pushArrFrame[T](
      xs: Val.Arr,
      visitor: Visitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      maxDepth: Int)(implicit evaluator: EvalScope): Unit = {
    checkDepth(xs.pos, stack.size, maxDepth)
    storePos(xs.pos)
    val arrVisitor = visitor.visitArray(xs.length, -1)
    stack.push(new Materializer.MaterializeArrFrame[T](arrVisitor, xs, 0))
  }

  // Feed a completed child result into the parent frame's visitor.
  private def feedResult[T](parentFrame: Materializer.MaterializeFrame, result: T): Unit =
    parentFrame match {
      case f: Materializer.MaterializeObjFrame[T @unchecked] =>
        f.objVisitor.visitValue(result, -1)
      case f: Materializer.MaterializeArrFrame[T @unchecked] =>
        f.arrVisitor.visitValue(result, -1)
    }

  private def checkDepth(pos: Position, stackSize: Int, maxDepth: Int)(implicit
      ev: EvalErrorScope): Unit =
    if (stackSize >= maxDepth)
      Error.fail(
        "Stackoverflow while materializing, possibly due to recursive value",
        pos
      )

  def reverse(pos: Position, v: ujson.Value): Val = v match {
    case ujson.True    => Val.True(pos)
    case ujson.False   => Val.False(pos)
    case ujson.Null    => Val.Null(pos)
    case ujson.Num(n)  => Val.Num(pos, n)
    case ujson.Str(s)  => Val.Str(pos, s)
    case ujson.Arr(xs) =>
      val len = xs.length
      val res = new Array[Eval](len)
      var i = 0
      while (i < len) {
        val x = xs(i)
        res(i) = new Lazy(() => reverse(pos, x))
        i += 1
      }
      Val.Arr(pos, res)
    case ujson.Obj(xs) =>
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      for (x <- xs) {
        val v = new Val.Obj.Member(false, Visibility.Normal, deprecatedSkipAsserts = true) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
            reverse(pos, x._2)
        }
        builder.put(x._1, v)
      }
      new Val.Obj(pos, builder, false, null, null)
  }

  def toExpr(v: ujson.Value)(implicit ev: EvalScope): Expr = v match {
    case ujson.True    => Val.True(ev.emptyMaterializeFileScopePos)
    case ujson.False   => Val.False(ev.emptyMaterializeFileScopePos)
    case ujson.Null    => Val.Null(ev.emptyMaterializeFileScopePos)
    case ujson.Num(n)  => Val.Num(ev.emptyMaterializeFileScopePos, n)
    case ujson.Str(s)  => Val.Str(ev.emptyMaterializeFileScopePos, s)
    case ujson.Arr(xs) =>
      val len = xs.length
      val res = new Array[Expr](len)
      var i = 0
      while (i < len) {
        res(i) = toExpr(xs(i))
        i += 1
      }
      Expr.Arr(ev.emptyMaterializeFileScopePos, res)
    case ujson.Obj(kvs) =>
      val members = new Array[Member.Field](kvs.size)
      var i = 0
      for ((k, v) <- kvs) {
        members(i) = Member.Field(
          ev.emptyMaterializeFileScopePos,
          FieldName.Fixed(k),
          plus = false,
          null,
          Visibility.Normal,
          toExpr(v)
        )
        i += 1
      }
      ObjBody.MemberList(
        ev.emptyMaterializeFileScopePos,
        null,
        members,
        null
      )
  }

}

object Materializer extends Materializer {
  def storePos(pos: Position): Unit = ()
  def storePos(v: Val): Unit = ()

  final val emptyStringArray = new Array[String](0)
  final val emptyLazyArray = new Array[Eval](0)

  /** Common parent for stack frames used in iterative materialization. */
  private[sjsonnet] sealed trait MaterializeFrame

  /** Stack frame for in-progress object materialization. */
  private[sjsonnet] final class MaterializeObjFrame[T](
      val objVisitor: ObjVisitor[T, T],
      val keys: Array[String],
      val obj: Val.Obj,
      val sort: Boolean,
      var index: Int,
      var prevKey: String)
      extends MaterializeFrame

  /** Stack frame for in-progress array materialization. */
  private[sjsonnet] final class MaterializeArrFrame[T](
      val arrVisitor: ArrVisitor[T, T],
      val arr: Val.Arr,
      var index: Int)
      extends MaterializeFrame

  /**
   * Trait for providing custom materialization logic to the Materializer.
   * @since 1.0.0
   */
  trait Materializable {
    def materialize[T](visitor: Visitor[T, T])(implicit evaluator: EvalScope): T
  }
}
