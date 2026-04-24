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
 * resolves TailCalls when called with `TailstrictModeDisabled`. A defensive check in the match arms
 * guards against accidental TailCall leakage with a clear internal-error diagnostic.
 *
 * Match ordering: all dispatch points ([[apply0]], [[materializeRecursiveChild]],
 * [[materializeChild]]) use a unified dispatch that routes via `valTag` (a byte field on each
 * [[Val]] subclass) through a JVM tableswitch for O(1) type resolution. The tag values 0-7
 * correspond to Str, Num, True, False, Null, Arr, Obj, Func respectively. Rare types
 * (Materializable, TailCall) fall through to a pattern match in the default branch.
 */
abstract class Materializer {
  def storePos(pos: Position): Unit
  def storePos(v: Val): Unit

  def apply(v: Val)(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    // Fast path for leaf types: avoid Renderer + StringWriter + CharBuilder allocation
    v match {
      case Val.True(_)   => "true"
      case Val.False(_)  => "false"
      case Val.Null(_)   => "null"
      case Val.Num(_, _) =>
        val d = v.asDouble
        val l = d.toLong
        if (l.toDouble == d) java.lang.Long.toString(l)
        else RenderUtils.renderDouble(d)
      case _ => apply0(v, new sjsonnet.Renderer()).toString
    }
  }

  /**
   * Hybrid materialization: uses JVM stack recursion for shallow nesting (zero heap allocation,
   * JIT-friendly) and automatically switches to an explicit stack-based iterative loop when the
   * recursion depth exceeds [[Settings.materializeRecursiveDepthLimit]].
   */
  def apply0[T](v: Val, visitor: Visitor[T, T])(implicit evaluator: EvalScope): T = try {
    v match {
      case Val.Str(pos, s) => storePos(pos); visitor.visitString(s, -1)
      case obj: Val.Obj    =>
        materializeRecursiveObj(obj, visitor, 0, Materializer.MaterializeContext(evaluator))
      case Val.Num(pos, _) => storePos(pos); visitor.visitFloat64(v.asDouble, -1)
      case xs: Val.Arr     =>
        materializeRecursiveArr(xs, visitor, 0, Materializer.MaterializeContext(evaluator))
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
  } catch {
    case _: StackOverflowError =>
      Error.fail("Stackoverflow while materializing, possibly due to recursive value", v.pos)
    case _: OutOfMemoryError =>
      Error.fail("Out of memory while materializing, possibly due to recursive value", v.pos)
  }

  private def materializeRecursiveObj[T](
      obj: Val.Obj,
      visitor: Visitor[T, T],
      depth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    if (!ctx.enterObject(obj))
      Error.fail("Stackoverflow while materializing, possibly due to recursive value", obj.pos)
    try {
      storePos(obj.pos)
      obj.triggerAllAsserts(ctx.brokenAssertionLogic)
      if (obj.canDirectIterate) {
        // Fast path for inline objects (1-8 fields, no super chain, no excludedKeys).
        // Bypasses visibleKeyNames allocation, value() HashMap lookup per key,
        // and sortedVisibleKeyNames lazy val. Instead iterates raw arrays directly.
        if (ctx.sort) materializeSortedInlineObj(obj, visitor, depth, ctx)
        else materializeInlineObj(obj, visitor, depth, ctx)
      } else {
        val keys =
          if (ctx.sort) obj.sortedVisibleKeyNames
          else obj.visibleKeyNames
        val ov = visitor.visitObject(keys.length, jsonableKeys = true, -1)
        var i = 0
        while (i < keys.length) {
          val key = keys(i)
          val childVal = obj.value(key, ctx.emptyPos)
          storePos(childVal)
          ov.visitKeyValue(ov.visitKey(-1).visitString(key, -1))
          val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
          ov.visitValue(materializeRecursiveChild(childVal, sub, depth, ctx), -1)
          i += 1
        }
        ov.visitEnd(-1)
      }
    } finally {
      ctx.exitObject(obj)
    }
  }

  /**
   * Direct iteration for inline objects without super chain. Bypasses value() lookup (cache checks,
   * valueRaw dispatch, key scan), invoking members directly by array index.
   */
  private def materializeInlineObj[T](
      obj: Val.Obj,
      visitor: Visitor[T, T],
      depth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers
      val rawN = rawKeys.length
      var visCount = 0
      var i = 0
      while (i < rawN) {
        if (rawMembers(i).visibility != Visibility.Hidden) visCount += 1
        i += 1
      }
      val ov = visitor.visitObject(visCount, jsonableKeys = true, -1)
      i = 0
      while (i < rawN) {
        val m = rawMembers(i)
        if (m.visibility != Visibility.Hidden) {
          val childVal = m.invoke(obj, null, fs, evaluator)
          if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(i), childVal)
          storePos(childVal)
          ov.visitKeyValue(ov.visitKey(-1).visitString(rawKeys(i), -1))
          val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
          ov.visitValue(materializeRecursiveChild(childVal, sub, depth, ctx), -1)
        }
        i += 1
      }
      ov.visitEnd(-1)
    } else {
      // Single-field object
      val sfm = obj.singleMem
      if (sfm.visibility != Visibility.Hidden) {
        val ov = visitor.visitObject(1, jsonableKeys = true, -1)
        val childVal = sfm.invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(obj.singleKey, childVal)
        storePos(childVal)
        ov.visitKeyValue(ov.visitKey(-1).visitString(obj.singleKey, -1))
        val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
        ov.visitValue(materializeRecursiveChild(childVal, sub, depth, ctx), -1)
        ov.visitEnd(-1)
      } else {
        visitor.visitObject(0, jsonableKeys = true, -1).visitEnd(-1)
      }
    }
  }

  /**
   * Sorted direct iteration for inline objects. Uses cached sorted field order when available
   * (shared across all objects from the same MemberList), falling back to per-object computation.
   * Avoids: sortedVisibleKeyNames lazy val, value() linear scan, validation check.
   */
  private def materializeSortedInlineObj[T](
      obj: Val.Obj,
      visitor: Visitor[T, T],
      depth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    val fs = ctx.emptyPos.fileScope
    val rawKeys = obj.inlineKeys
    if (rawKeys != null) {
      val rawMembers = obj.inlineMembers
      // Use cached sorted order if available, otherwise compute
      val order = {
        val cached = obj._sortedInlineOrder
        if (cached != null) cached
        else Materializer.computeSortedInlineOrder(rawKeys, rawMembers)
      }
      val visCount = order.length
      // Iterate in sorted order with direct member invocation
      val ov = visitor.visitObject(visCount, jsonableKeys = true, -1)
      var i = 0
      while (i < visCount) {
        val idx = order(i)
        val childVal = rawMembers(idx).invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(rawKeys(idx), childVal)
        storePos(childVal)
        ov.visitKeyValue(ov.visitKey(-1).visitString(rawKeys(idx), -1))
        val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
        ov.visitValue(materializeRecursiveChild(childVal, sub, depth, ctx), -1)
        i += 1
      }
      ov.visitEnd(-1)
    } else {
      // Single-field object: sorting is trivial (same as unsorted)
      val sfm = obj.singleMem
      if (sfm.visibility != Visibility.Hidden) {
        val ov = visitor.visitObject(1, jsonableKeys = true, -1)
        val childVal = sfm.invoke(obj, null, fs, evaluator)
        if (!obj._skipFieldCache) obj.cacheFieldValue(obj.singleKey, childVal)
        storePos(childVal)
        ov.visitKeyValue(ov.visitKey(-1).visitString(obj.singleKey, -1))
        val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
        ov.visitValue(materializeRecursiveChild(childVal, sub, depth, ctx), -1)
        ov.visitEnd(-1)
      } else {
        visitor.visitObject(0, jsonableKeys = true, -1).visitEnd(-1)
      }
    }
  }

  @inline private def materializeRecursiveArr[T](
      xs: Val.Arr,
      visitor: Visitor[T, T],
      depth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    storePos(xs.pos)
    // Fast path for byte-backed arrays: skip per-element value() + type dispatch
    xs match {
      case ba: Val.ByteArr =>
        val bytes = ba.rawBytes
        val len = bytes.length
        val av = visitor.visitArray(len, -1)
        var i = 0
        while (i < len) {
          av.visitValue(
            av.subVisitor
              .asInstanceOf[Visitor[T, T]]
              .visitFloat64((bytes(i) & 0xff).toDouble, -1),
            -1
          )
          i += 1
        }
        return av.visitEnd(-1)
      case _ =>
    }
    val len = xs.length
    val av = visitor.visitArray(len, -1)
    var i = 0
    while (i < len) {
      val childVal = xs.value(i)
      av.visitValue(
        materializeRecursiveChild(childVal, av.subVisitor.asInstanceOf[Visitor[T, T]], depth, ctx),
        -1
      )
      i += 1
    }
    av.visitEnd(-1)
  }

  private def materializeRecursiveChild[T](
      childVal: Val,
      childVisitor: Visitor[T, T],
      depth: Int,
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    if (childVal == null) Error.fail("Unknown value type " + childVal)
    // Use tableswitch dispatch via @switch on valTag for O(1) type routing.
    // This replaces up to 7 sequential instanceof checks with a single
    // byte comparison + jump table, benefiting materialization-heavy workloads.
    val vt: Int = childVal.valTag.toInt
    (vt: @scala.annotation.switch) match {
      case 0 => // TAG_STR
        val s = childVal.asInstanceOf[Val.Str]
        storePos(s.pos); childVisitor.visitString(s.str, -1)
      case 1 => // TAG_NUM
        storePos(childVal.pos); childVisitor.visitFloat64(childVal.asDouble, -1)
      case 2 => // TAG_TRUE
        storePos(childVal.pos); childVisitor.visitTrue(-1)
      case 3 => // TAG_FALSE
        storePos(childVal.pos); childVisitor.visitFalse(-1)
      case 4 => // TAG_NULL
        storePos(childVal.pos); childVisitor.visitNull(-1)
      case 5 => // TAG_ARR
        val xs = childVal.asInstanceOf[Val.Arr]
        val nextDepth = depth + 1
        if (nextDepth < ctx.recursiveDepthLimit)
          materializeRecursiveArr(xs, childVisitor, nextDepth, ctx)
        else
          materializeStackless(childVal, childVisitor, ctx)
      case 6 => // TAG_OBJ
        val obj = childVal.asInstanceOf[Val.Obj]
        val nextDepth = depth + 1
        if (nextDepth < ctx.recursiveDepthLimit)
          materializeRecursiveObj(obj, childVisitor, nextDepth, ctx)
        else
          materializeStackless(childVal, childVisitor, ctx)
      case 7 => // TAG_FUNC
        val s = childVal.asInstanceOf[Val.Func]
        Error.fail(
          "Couldn't manifest function with params [" + s.params.names.mkString(",") + "]",
          childVal.pos
        )
      case _ =>
        childVal match {
          case mat: Materializer.Materializable =>
            storePos(childVal.pos); mat.materialize(childVisitor)
          case tc: TailCall =>
            Error.fail(
              "Internal error: TailCall sentinel leaked into materialization. " +
              "This indicates a bug in the TCO protocol — a TailCall was not resolved before " +
              "reaching the Materializer.",
              tc.pos
            )
          case vv: Val =>
            Error.fail("Unknown value type " + vv.prettyName, vv.pos)
        }
    }
  }

  // Iterative materialization for deep nesting. Used as a fallback when recursive depth exceeds
  // the recursive depth limit. Uses an explicit ArrayDeque stack to avoid StackOverflowError.
  private def materializeStackless[T](
      v: Val,
      visitor: Visitor[T, T],
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): T = {
    val stack = new java.util.ArrayDeque[Materializer.MaterializeFrame](
      Math.max(16, Math.min(ctx.recursiveDepthLimit * 4, 8192))
    )

    // Push the initial container frame
    v match {
      case obj: Val.Obj => pushObjFrame(obj, visitor, stack, ctx)
      case xs: Val.Arr  => pushArrFrame(xs, visitor, stack, ctx)
      case _            => () // unreachable
    }

    while (true) {
      stack.peekFirst() match {
        case frame: Materializer.MaterializeObjFrame[T @unchecked] =>
          val keys = frame.keys
          val ov = frame.objVisitor
          if (frame.index < keys.length) {
            val key = keys(frame.index)
            val childVal = frame.obj.value(key, ctx.emptyPos)
            storePos(childVal)

            if (frame.sort) {
              if (frame.prevKey != null && Util.compareStringsByCodepoint(key, frame.prevKey) <= 0)
                Error.fail(
                  s"""Internal error: Unexpected key "$key" after "${frame.prevKey}" in sorted object materialization""",
                  childVal.pos
                )
              frame.prevKey = key
            }

            ov.visitKeyValue(ov.visitKey(-1).visitString(key, -1))
            frame.index += 1

            val sub = ov.subVisitor.asInstanceOf[Visitor[T, T]]
            materializeChild(childVal, sub, ov, stack, ctx)
          } else {
            ctx.exitObject(frame.obj)
            val result = ov.visitEnd(-1)
            stack.removeFirst()
            if (stack.isEmpty) return result
            feedResult(stack.peekFirst(), result)
          }

        case frame: Materializer.MaterializeArrFrame[T @unchecked] =>
          val arr = frame.arr
          val av = frame.arrVisitor
          // Fast path for byte-backed arrays: emit all elements directly
          if (frame.index == 0) {
            arr match {
              case ba: Val.ByteArr =>
                val bytes = ba.rawBytes
                val len = bytes.length
                var i = 0
                while (i < len) {
                  val sub = av.subVisitor.asInstanceOf[Visitor[T, T]]
                  av.visitValue(sub.visitFloat64((bytes(i) & 0xff).toDouble, -1), -1)
                  i += 1
                }
                frame.index = len // mark as done
              case _ =>
            }
          }
          if (frame.index < arr.length) {
            val childVal = arr.value(frame.index)
            frame.index += 1

            val sub = av.subVisitor.asInstanceOf[Visitor[T, T]]
            materializeChild(childVal, sub, av, stack, ctx)
          } else {
            val result = av.visitEnd(-1)
            stack.removeFirst()
            if (stack.isEmpty) return result
            feedResult(stack.peekFirst(), result)
          }
      }
    }

    null.asInstanceOf[T] // unreachable — while(true) exits via return
  }

  // Materialize a child value in iterative mode. Single match dispatches leaf values directly
  // and pushes a new stack frame for containers. Maintains the same match ordering as the
  // recursive path: Val.Str first for optimal performance on the most common leaf type.
  @inline private def materializeChild[T](
      childVal: Val,
      childVisitor: Visitor[T, T],
      parentVisitor: upickle.core.ObjArrVisitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    childVal match {
      case Val.Str(pos, s) =>
        storePos(pos); parentVisitor.visitValue(childVisitor.visitString(s, -1), -1)
      case obj: Val.Obj =>
        pushObjFrame(obj, childVisitor, stack, ctx)
      case Val.Num(pos, _) =>
        storePos(pos);
        parentVisitor.visitValue(childVisitor.visitFloat64(childVal.asDouble, -1), -1)
      case xs: Val.Arr =>
        pushArrFrame(xs, childVisitor, stack, ctx)
      case Val.True(pos) =>
        storePos(pos); parentVisitor.visitValue(childVisitor.visitTrue(-1), -1)
      case Val.False(pos) =>
        storePos(pos); parentVisitor.visitValue(childVisitor.visitFalse(-1), -1)
      case Val.Null(pos) =>
        storePos(pos); parentVisitor.visitValue(childVisitor.visitNull(-1), -1)
      case mat: Materializer.Materializable =>
        storePos(childVal.pos); parentVisitor.visitValue(mat.materialize(childVisitor), -1)
      case s: Val.Func =>
        Error.fail(
          "Couldn't manifest function with params [" + s.params.names.mkString(",") + "]",
          childVal.pos
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
        Error.fail("Unknown value type " + childVal)
    }
  }

  @inline private def pushObjFrame[T](
      obj: Val.Obj,
      visitor: Visitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    checkDepth(obj.pos, stack.size, ctx.maxDepth)
    if (!ctx.enterObject(obj))
      Error.fail("Stackoverflow while materializing, possibly due to recursive value", obj.pos)
    storePos(obj.pos)
    obj.triggerAllAsserts(ctx.brokenAssertionLogic)
    val keyNames =
      if (ctx.sort) obj.sortedVisibleKeyNames
      else obj.visibleKeyNames
    val objVisitor = visitor.visitObject(keyNames.length, jsonableKeys = true, -1)
    stack.push(
      new Materializer.MaterializeObjFrame[T](objVisitor, keyNames, obj, ctx.sort, 0, null)
    )
  }

  @inline private def pushArrFrame[T](
      xs: Val.Arr,
      visitor: Visitor[T, T],
      stack: java.util.ArrayDeque[Materializer.MaterializeFrame],
      ctx: Materializer.MaterializeContext)(implicit evaluator: EvalScope): Unit = {
    checkDepth(xs.pos, stack.size, ctx.maxDepth)
    storePos(xs.pos)
    val arrVisitor = visitor.visitArray(xs.length, -1)
    stack.push(new Materializer.MaterializeArrFrame[T](arrVisitor, xs, 0))
  }

  // Feed a completed child result into the parent frame's visitor.
  @inline private def feedResult[T](parentFrame: Materializer.MaterializeFrame, result: T): Unit =
    parentFrame match {
      case f: Materializer.MaterializeObjFrame[T @unchecked] =>
        f.objVisitor.visitValue(result, -1)
      case f: Materializer.MaterializeArrFrame[T @unchecked] =>
        f.arrVisitor.visitValue(result, -1)
    }

  @inline private def checkDepth(pos: Position, stackSize: Int, maxDepth: Int)(implicit
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
        res(i) = new LazyFunc(() => reverse(pos, x))
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

  /**
   * Compute sorted field order for inline objects. Returns array of indices into the keys/members
   * arrays, sorted by key name and excluding hidden fields. Used for both per-object computation
   * and MemberList-level caching.
   */
  private[sjsonnet] def computeSortedInlineOrder(
      keys: Array[String],
      members: Array[Val.Obj.Member]
  ): Array[Int] = {
    val n = keys.length
    var visCount = 0
    var i = 0
    while (i < n) {
      if (members(i).visibility != Visibility.Hidden) visCount += 1
      i += 1
    }
    val order = new Array[Int](visCount)
    i = 0
    var k = 0
    while (i < n) {
      if (members(i).visibility != Visibility.Hidden) {
        order(k) = i
        k += 1
      }
      i += 1
    }
    // Insertion sort by key name (optimal for 2-8 elements)
    i = 1
    while (i < visCount) {
      val pivotIdx = order(i)
      val pivotKey = keys(pivotIdx)
      var j = i - 1
      while (j >= 0 && Util.compareStringsByCodepoint(keys(order(j)), pivotKey) > 0) {
        order(j + 1) = order(j)
        j -= 1
      }
      order(j + 1) = pivotIdx
      i += 1
    }
    order
  }

  /**
   * Checks whether a [[Expr.ObjBody.MemberList]]'s expressions reference `self`, `super`, or `$` at
   * the current object scope level. When `true` (no self-ref), field caching can be safely skipped
   * during materialization since no field evaluation will trigger `obj.value()` on the current
   * object.
   *
   * Uses two-mode scanning:
   *   - At current scope (`inNestedObj=false`): `Self`, `Super`, `$`, `SelectSuper`, `InSuper`,
   *     `LookupSuper` → has self-ref
   *   - Inside nested object bodies (`inNestedObj=true`): only `$` → has self-ref (because `$`
   *     always references the root object, which might be the current object; `self/super` inside
   *     nested objects refer to the inner object, not the outer)
   *
   * The result is cached on [[Expr.ObjBody.MemberList._noSelfRef]] (volatile, benign-race safe)
   * since the same MemberList AST node is shared across all objects created from the same
   * expression.
   */
  private[sjsonnet] def computeNoSelfRef(e: Expr.ObjBody.MemberList): Boolean = {
    val cached = e._noSelfRef
    if (cached != null) return cached.booleanValue()
    val result = !hasSelfRefInMemberList(e, inNestedObj = false)
    e._noSelfRef = java.lang.Boolean.valueOf(result)
    result
  }

  /** Scan all expressions in a MemberList for self/super/$ references. */
  private def hasSelfRefInMemberList(
      ml: Expr.ObjBody.MemberList,
      inNestedObj: Boolean
  ): Boolean = {
    // Check fields (rhs, dynamic field names, method-style args)
    if (ml.fields != null) {
      var i = 0
      while (i < ml.fields.length) {
        val f = ml.fields(i)
        f.fieldName match {
          case Expr.FieldName.Dyn(expr) =>
            if (hasSelfRefExpr(expr, inNestedObj)) return true
          case _ =>
        }
        if (f.args != null && hasSelfRefParams(f.args, inNestedObj)) return true
        if (hasSelfRefExpr(f.rhs, inNestedObj)) return true
        i += 1
      }
    }
    // Check binds (object locals): rhs and function default args
    if (ml.binds != null) {
      var i = 0
      while (i < ml.binds.length) {
        val b = ml.binds(i)
        if (hasSelfRefExpr(b.rhs, inNestedObj)) return true
        if (b.args != null && hasSelfRefParams(b.args, inNestedObj)) return true
        i += 1
      }
    }
    // Check asserts: value and optional message
    if (ml.asserts != null) {
      var i = 0
      while (i < ml.asserts.length) {
        val a = ml.asserts(i)
        if (hasSelfRefExpr(a.value, inNestedObj)) return true
        if (a.msg != null && hasSelfRefExpr(a.msg, inNestedObj)) return true
        i += 1
      }
    }
    false
  }

  /** Scan function parameter default expressions for self/super/$ references. */
  private def hasSelfRefParams(p: Expr.Params, inNestedObj: Boolean): Boolean = {
    if (p.defaultExprs != null) {
      var i = 0
      while (i < p.defaultExprs.length) {
        val de = p.defaultExprs(i)
        if (de != null && hasSelfRefExpr(de, inNestedObj)) return true
        i += 1
      }
    }
    false
  }

  /**
   * Recursive self-reference check for a single expression.
   *
   * @param inNestedObj
   *   when true, Self/Super nodes are ignored (they refer to the inner object); only `$` is
   *   detected (it always refers to the root object)
   */
  private def hasSelfRefExpr(e: Expr, inNestedObj: Boolean): Boolean = {
    if (e == null) return false
    e match {
      // $ always propagates — it references the root object, which might be the current object
      case _: Expr.$ => true
      // self/super only matter at current scope level; inner objects rebind them
      case _: Expr.Self        => !inNestedObj
      case _: Expr.Super       => !inNestedObj
      case _: Expr.SelectSuper => !inNestedObj
      case e: Expr.InSuper     =>
        if (!inNestedObj) true else hasSelfRefExpr(e.value, true)
      case e: Expr.LookupSuper =>
        if (!inNestedObj) true else hasSelfRefExpr(e.index, true)

      // Leaf nodes: no self-reference
      case _: Val.Literal    => false
      case _: Expr.ValidId   => false
      case _: Expr.Id        => false
      case _: Expr.Import    => false
      case _: Expr.ImportStr => false
      case _: Expr.ImportBin => false

      // Composite nodes: recurse into children
      case Expr.UnaryOp(_, _, v)     => hasSelfRefExpr(v, inNestedObj)
      case Expr.BinaryOp(_, l, _, r) =>
        hasSelfRefExpr(l, inNestedObj) || hasSelfRefExpr(r, inNestedObj)
      case Expr.And(_, l, r)    => hasSelfRefExpr(l, inNestedObj) || hasSelfRefExpr(r, inNestedObj)
      case Expr.Or(_, l, r)     => hasSelfRefExpr(l, inNestedObj) || hasSelfRefExpr(r, inNestedObj)
      case Expr.Select(_, v, _) => hasSelfRefExpr(v, inNestedObj)
      case Expr.Lookup(_, v, idx) =>
        hasSelfRefExpr(v, inNestedObj) || hasSelfRefExpr(idx, inNestedObj)
      case Expr.IfElse(_, c, t, el) =>
        hasSelfRefExpr(c, inNestedObj) || hasSelfRefExpr(t, inNestedObj) || hasSelfRefExpr(
          el,
          inNestedObj
        )
      case Expr.Error(_, v) => hasSelfRefExpr(v, inNestedObj)

      // Apply variants
      case Expr.Apply(_, v, args, _, _, _) =>
        hasSelfRefExpr(v, inNestedObj) || args.exists(a => hasSelfRefExpr(a, inNestedObj))
      case Expr.Apply0(_, v, _, _)     => hasSelfRefExpr(v, inNestedObj)
      case Expr.Apply1(_, v, a1, _, _) =>
        hasSelfRefExpr(v, inNestedObj) || hasSelfRefExpr(a1, inNestedObj)
      case Expr.Apply2(_, v, a1, a2, _, _) =>
        hasSelfRefExpr(v, inNestedObj) || hasSelfRefExpr(a1, inNestedObj) || hasSelfRefExpr(
          a2,
          inNestedObj
        )
      case Expr.Apply3(_, v, a1, a2, a3, _, _) =>
        hasSelfRefExpr(v, inNestedObj) || hasSelfRefExpr(a1, inNestedObj) ||
        hasSelfRefExpr(a2, inNestedObj) || hasSelfRefExpr(a3, inNestedObj)

      // ApplyBuiltin variants
      case _: Expr.ApplyBuiltin0               => false
      case Expr.ApplyBuiltin1(_, _, a1, _)     => hasSelfRefExpr(a1, inNestedObj)
      case Expr.ApplyBuiltin2(_, _, a1, a2, _) =>
        hasSelfRefExpr(a1, inNestedObj) || hasSelfRefExpr(a2, inNestedObj)
      case Expr.ApplyBuiltin3(_, _, a1, a2, a3, _) =>
        hasSelfRefExpr(a1, inNestedObj) || hasSelfRefExpr(a2, inNestedObj) || hasSelfRefExpr(
          a3,
          inNestedObj
        )
      case Expr.ApplyBuiltin4(_, _, a1, a2, a3, a4, _) =>
        hasSelfRefExpr(a1, inNestedObj) || hasSelfRefExpr(a2, inNestedObj) ||
        hasSelfRefExpr(a3, inNestedObj) || hasSelfRefExpr(a4, inNestedObj)
      case Expr.ApplyBuiltin(_, _, argExprs, _) =>
        argExprs.exists(a => hasSelfRefExpr(a, inNestedObj))

      // Assert expression (inline assert ... ; expr)
      case Expr.AssertExpr(_, asserted, returned) =>
        hasSelfRefExpr(asserted.value, inNestedObj) ||
        (asserted.msg != null && hasSelfRefExpr(asserted.msg, inNestedObj)) ||
        hasSelfRefExpr(returned, inNestedObj)

      // Local expression (local x = ...; expr)
      case Expr.LocalExpr(_, bindings, returned) =>
        bindings.exists(b =>
          hasSelfRefExpr(b.rhs, inNestedObj) ||
          (b.args != null && hasSelfRefParams(b.args, inNestedObj))
        ) || hasSelfRefExpr(returned, inNestedObj)

      // Array literal
      case Expr.Arr(_, values) => values.exists(v => hasSelfRefExpr(v, inNestedObj))

      // Slice
      case Expr.Slice(_, v, start, end, stride) =>
        hasSelfRefExpr(v, inNestedObj) ||
        start.exists(s => hasSelfRefExpr(s, inNestedObj)) ||
        end.exists(e => hasSelfRefExpr(e, inNestedObj)) ||
        stride.exists(s => hasSelfRefExpr(s, inNestedObj))

      // Function: scan body and parameter defaults (closures can capture self)
      case Expr.Function(_, params, body) =>
        hasSelfRefExpr(body, inNestedObj) ||
        (params != null && hasSelfRefParams(params, inNestedObj))

      // Array comprehension
      case Expr.Comp(_, v, first, rest) =>
        hasSelfRefExpr(v, inNestedObj) || hasSelfRefExpr(first.cond, inNestedObj) ||
        rest.exists {
          case Expr.ForSpec(_, _, cond) => hasSelfRefExpr(cond, inNestedObj)
          case Expr.IfSpec(_, cond)     => hasSelfRefExpr(cond, inNestedObj)
        }

      // Object extension: base is in current scope, ext creates new self scope
      case Expr.ObjExtend(_, base, ext) =>
        hasSelfRefExpr(base, inNestedObj) || hasSelfRefExpr(ext, true)

      // Nested object bodies: switch to inNestedObj=true (self/super refer to inner object)
      case ml: Expr.ObjBody.MemberList => hasSelfRefInMemberList(ml, inNestedObj = true)
      case oc: Expr.ObjBody.ObjComp    =>
        hasSelfRefExpr(oc.key, true) || hasSelfRefExpr(oc.value, true) ||
        (oc.preLocals != null && oc.preLocals.exists(b =>
          hasSelfRefExpr(b.rhs, true) || (b.args != null && hasSelfRefParams(b.args, true))
        )) ||
        (oc.postLocals != null && oc.postLocals.exists(b =>
          hasSelfRefExpr(b.rhs, true) || (b.args != null && hasSelfRefParams(b.args, true))
        )) ||
        hasSelfRefExpr(oc.first.cond, true) ||
        oc.rest.exists {
          case Expr.ForSpec(_, _, cond) => hasSelfRefExpr(cond, true)
          case Expr.IfSpec(_, cond)     => hasSelfRefExpr(cond, true)
        }

      // Val.Func (appears in AST after StaticOptimizer inlines constant functions).
      // Val.Builtin* extends Val.Func — builtins have bodyExpr=null, so this returns false for them.
      case f: Val.Func =>
        (f.bodyExpr != null && hasSelfRefExpr(f.bodyExpr, inNestedObj)) ||
        (f.params != null && hasSelfRefParams(f.params, inNestedObj))

      // Conservative default: assume self-reference for unknown node types
      case _ => true
    }
  }

  /**
   * Immutable snapshot of all settings needed during a single materialization pass. Created once
   * per top-level call and threaded through recursive/iterative helpers, avoiding repeated field
   * lookups on the [[Settings]] object on every frame.
   */
  private[sjsonnet] final class MaterializeContext(
      val sort: Boolean,
      val brokenAssertionLogic: Boolean,
      val emptyPos: Position,
      val recursiveDepthLimit: Int,
      val maxDepth: Int) {

    // Tracks objects currently being materialized on the current path (entry → exit).
    // Used to detect cycles like `{ x: self }` early, before any significant output
    // is written. Uses identity equality so structurally equal but distinct objects
    // can still be materialized independently.
    val visitedObjects: java.util.IdentityHashMap[Val.Obj, java.lang.Boolean] =
      new java.util.IdentityHashMap[Val.Obj, java.lang.Boolean]()

    /** Returns true if `obj` was NOT already being materialized (safe to proceed). */
    @inline def enterObject(obj: Val.Obj): Boolean =
      visitedObjects.put(obj, java.lang.Boolean.TRUE) eq null

    @inline def exitObject(obj: Val.Obj): Unit =
      visitedObjects.remove(obj)
  }

  private[sjsonnet] object MaterializeContext {
    def apply(ev: EvalScope): MaterializeContext = new MaterializeContext(
      sort = !ev.settings.preserveOrder,
      brokenAssertionLogic = ev.settings.brokenAssertionLogic,
      emptyPos = ev.emptyMaterializeFileScopePos,
      recursiveDepthLimit = ev.settings.materializeRecursiveDepthLimit,
      maxDepth = ev.settings.maxMaterializeDepth
    )
  }

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
