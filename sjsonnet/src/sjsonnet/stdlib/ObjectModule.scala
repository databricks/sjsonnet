package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.AbstractFunctionModule

object ObjectModule extends AbstractFunctionModule {
  def name = "object"

  private object ObjectHas extends Val.Builtin2("objectHas", "o", "f") {
    def evalRhs(o: Eval, f: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(o.value.asObj.containsVisibleKey(f.value.asString))
    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) =
      args match {
        case Array(o, s: Val.Str) => (new SpecF(s.str), Array(o))
        case _                    => null
      }
    private class SpecF(f: String) extends Val.Builtin1("objectHas", "o") {
      def evalRhs(o: Eval, ev: EvalScope, pos: Position): Val =
        Val.bool(o.value.asObj.containsVisibleKey(f))
    }
  }

  private object ObjectHasAll extends Val.Builtin2("objectHasAll", "o", "f") {
    def evalRhs(o: Eval, f: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(o.value.asObj.containsKey(f.value.asString))
    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) =
      args match {
        case Array(o, s: Val.Str) => (new SpecF(s.str), Array(o))
        case _                    => null
      }
    class SpecF(f: String) extends Val.Builtin1("objectHasAll", "o") {
      def evalRhs(o: Eval, ev: EvalScope, pos: Position): Val =
        Val.bool(o.value.asObj.containsKey(f))
    }
  }

  private object ObjectFields extends Val.Builtin1("objectFields", "o") {
    def evalRhs(o: Eval, ev: EvalScope, pos: Position): Val = {
      val keys = getVisibleKeys(ev, o.value.asObj)
      val result = new Array[Eval](keys.length)
      var i = 0
      while (i < keys.length) {
        result(i) = Val.Str(pos, keys(i))
        i += 1
      }
      Val.Arr(pos, result)
    }
  }

  private object ObjectFieldsAll extends Val.Builtin1("objectFieldsAll", "o") {
    def evalRhs(o: Eval, ev: EvalScope, pos: Position): Val = {
      val keys = getAllKeys(ev, o.value.asObj)
      val result = new Array[Eval](keys.length)
      var i = 0
      while (i < keys.length) {
        result(i) = Val.Str(pos, keys(i))
        i += 1
      }
      Val.Arr(pos, result)
    }
  }

  private object ObjectFieldsEx extends Val.Builtin2("objectFieldsEx", "o", "inc_hidden") {
    def evalRhs(o: Eval, incHidden: Eval, ev: EvalScope, pos: Position): Val = {
      val keys =
        if (incHidden.value.asBoolean) getAllKeys(ev, o.value.asObj)
        else getVisibleKeys(ev, o.value.asObj)
      val result = new Array[Eval](keys.length)
      var i = 0
      while (i < keys.length) {
        result(i) = Val.Str(pos, keys(i))
        i += 1
      }
      Val.Arr(pos, result)
    }
  }

  private object ObjectValues extends Val.Builtin1("objectValues", "o") {
    def evalRhs(_o: Eval, ev: EvalScope, pos: Position): Val = {
      val o = _o.value.asObj
      val keys = getVisibleKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object ObjectValuesAll extends Val.Builtin1("objectValuesAll", "o") {
    def evalRhs(_o: Eval, ev: EvalScope, pos: Position): Val = {
      val o = _o.value.asObj
      val keys = getAllKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object Get
      extends Val.Builtin(
        "get",
        Array("o", "f", "default", "inc_hidden"),
        Array(null, null, Val.Null(dummyPos), Val.True(dummyPos))
      ) {
    override def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val = {
      val obj = args(0).value.asObj
      val k = args(1).value.asString
      val incHidden = args(3).value.asBoolean
      if (incHidden && obj.containsKey(k)) {
        obj.value(k, pos.noOffset, obj)(ev)
      } else if (!incHidden && obj.containsVisibleKey(k)) {
        obj.value(k, pos.noOffset, obj)(ev)
      } else {
        args(2).value
      }
    }
  }

  private object MapWithKey extends Val.Builtin2("mapWithKey", "func", "obj") {
    def evalRhs(_func: Eval, _obj: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      val obj = _obj.value.asObj
      val allKeys = obj.allKeyNames
      val n = allKeys.length
      if (n == 1) {
        new Val.Obj(
          pos,
          null,
          false,
          null,
          null,
          singleFieldKey = allKeys(0),
          singleFieldMember = new MapWithKeyMember(func, obj, allKeys(0), pos)
        )
      } else if (n <= 8) {
        val members = new Array[Val.Obj.Member](n)
        var i = 0
        while (i < n) {
          members(i) = new MapWithKeyMember(func, obj, allKeys(i), pos)
          i += 1
        }
        new Val.Obj(
          pos,
          null,
          false,
          null,
          null,
          inlineFieldKeys = allKeys,
          inlineFieldMembers = members
        )
      } else {
        val m = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](n)
        var i = 0
        while (i < n) {
          m.put(allKeys(i), new MapWithKeyMember(func, obj, allKeys(i), pos))
          i += 1
        }
        new Val.Obj(pos, m, false, null, null)
      }
    }
  }

  private final class MapWithKeyMember(
      private val func: Val.Func,
      private val sourceObj: Val.Obj,
      private val key: String,
      private val pos: Position)
      extends Val.Obj.Member(false, Visibility.Normal, deprecatedSkipAsserts = true) {
    def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
      func.apply2(
        Val.Str(pos, key),
        new LazyFunc(() => sourceObj.value(key, pos.noOffset)(ev)),
        pos.noOffset
      )(ev, TailstrictModeDisabled)
  }

  def getVisibleKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.visibleKeyNames)

  def getAllKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.allKeyNames)

  @inline private def maybeSortKeys(ev: EvalScope, keys: Array[String]): Array[String] =
    if (ev.settings.preserveOrder) keys else keys.sorted(Util.CodepointStringOrdering)

  def getObjValuesFromKeys(
      pos: Position,
      ev: EvalScope,
      v1: Val.Obj,
      keys: Array[String]): Val.Arr = {
    val result = new Array[Eval](keys.length)
    var i = 0
    while (i < keys.length) {
      val k = keys(i)
      result(i) = new LazyFunc(() => v1.value(k, pos.noOffset)(ev))
      i += 1
    }
    Val.Arr(pos, result)
  }

  val functionNames: Array[String] = Array(
    "objectHas",
    "objectHasAll",
    "objectHasEx",
    "objectFields",
    "objectFieldsAll",
    "objectFieldsEx",
    "objectValues",
    "objectValuesAll",
    "get",
    "mapWithKey",
    "objectKeysValues",
    "objectKeysValuesAll",
    "objectRemoveKey",
    "mergePatch",
    "prune"
  )

  lazy val functions: Seq[(String, Val.Func)] = Seq(
    builtin(ObjectHas),
    builtin(ObjectHasAll),
    builtin("objectHasEx", "o", "k", "inc_hidden") {
      (_, _, o: Val, k: String, incHidden: Boolean) =>
        if (incHidden) {
          o.asObj.containsKey(k)
        } else {
          o.asObj.containsVisibleKey(k)
        }
    },
    builtin(ObjectFields),
    builtin(ObjectFieldsAll),
    builtin(ObjectFieldsEx),
    builtin(ObjectValues),
    builtin(ObjectValuesAll),
    builtin(Get),
    builtin(MapWithKey),
    builtin("objectKeysValues", "o") { (pos, ev, o: Val.Obj) =>
      val keys = getVisibleKeys(ev, o)
      val noOffsetPos = pos.fileScope.noOffsetPos
      val result = new Array[Eval](keys.length)
      var i = 0
      while (i < keys.length) {
        val k = keys(i)
        result(i) = Val.Obj.mk(
          noOffsetPos,
          "key" -> new Val.Obj.ConstMember(false, Visibility.Normal, Val.Str(noOffsetPos, k)),
          "value" -> new Val.Obj.ConstMember(
            false,
            Visibility.Normal,
            o.value(k, noOffsetPos)(ev)
          )
        )
        i += 1
      }
      Val.Arr(pos, result)
    },
    builtin("objectKeysValuesAll", "o") { (pos, ev, o: Val.Obj) =>
      val keys = getAllKeys(ev, o)
      val noOffsetPos = pos.fileScope.noOffsetPos
      val result = new Array[Eval](keys.length)
      var i = 0
      while (i < keys.length) {
        val k = keys(i)
        result(i) = Val.Obj.mk(
          noOffsetPos,
          "key" -> new Val.Obj.ConstMember(false, Visibility.Normal, Val.Str(noOffsetPos, k)),
          "value" -> new Val.Obj.ConstMember(
            false,
            Visibility.Normal,
            o.value(k, noOffsetPos)(ev)
          )
        )
        i += 1
      }
      Val.Arr(pos, result)
    },
    builtin("objectRemoveKey", "obj", "key") { (pos, ev, o: Val.Obj, key: String) =>
      o.removeKeys(pos, key)
    },
    builtin("mergePatch", "target", "patch") { (pos, ev, target: Val, patch: Val) =>
      val mergePosition = pos
      def createLazyMember(v: => Val) =
        new Val.Obj.Member(false, Visibility.Normal, deprecatedSkipAsserts = true) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = v
        }
      def recPair(l: Val, r: Val): Val = (l, r) match {
        case (l: Val.Obj, r: Val.Obj) =>
          val keys: Array[String] = distinctKeys(l.visibleKeyNames, r.visibleKeyNames)
          val kvs: Array[(String, Val.Obj.Member)] =
            new Array[(String, Val.Obj.Member)](keys.length)
          var kvsIdx = 0
          var i = 0
          while (i < keys.length) {
            val key = keys(i)
            val rValue = if (r.containsVisibleKey(key)) r.valueRaw(key, r, pos)(ev) else null
            if (!rValue.isInstanceOf[Val.Null]) { // if we are not removing the key
              if (l.containsVisibleKey(key)) {
                if (rValue == null) {
                  // Preserve the LHS/target value:
                  kvs(kvsIdx) = (key, createLazyMember(l.valueRaw(key, l, pos)(ev)))
                } else {
                  // Below, lValue is lazy so that we can short circuit and skip its
                  // evaluation when rValue is not an object:
                  lazy val lValue = l.valueRaw(key, l, pos)(ev)
                  if (rValue.isInstanceOf[Val.Obj] && lValue.isInstanceOf[Val.Obj]) {
                    // Recursively merge objects:
                    kvs(kvsIdx) = (key, createLazyMember(recPair(lValue, rValue)))
                  } else {
                    // Use the RHS/patch value and recursively remove Null or hidden fields:
                    kvs(kvsIdx) = (key, createLazyMember(recSingle(rValue)))
                  }
                }
              } else {
                // Use the RHS/patch value and recursively remove Null or hidden fields:
                kvs(kvsIdx) = (key, createLazyMember(recSingle(rValue)))
              }
              kvsIdx += 1
            }
            i += 1
          }

          val trimmedKvs = if (kvsIdx == i) kvs else kvs.slice(0, kvsIdx)
          Val.Obj.mk(mergePosition, trimmedKvs)

        case (_, _) => recSingle(r)
      }
      def recSingle(v: Val): Val = v match {
        case obj: Val.Obj =>
          val keys: Array[String] = obj.visibleKeyNames
          val kvs: Array[(String, Val.Obj.Member)] =
            new Array[(String, Val.Obj.Member)](keys.length)
          var kvsIdx = 0
          var i = 0
          while (i < keys.length) {
            val key = keys(i)
            val value = obj.value(key, pos, obj)(ev)
            if (!value.isInstanceOf[Val.Null]) {
              kvs(kvsIdx) = (key, createLazyMember(recSingle(value)))
              kvsIdx += 1
            }
            i += 1
          }
          val trimmedKvs = if (kvsIdx == i) kvs else kvs.slice(0, kvsIdx)
          Val.Obj.mk(obj.pos, trimmedKvs)

        case _ => v
      }
      def distinctKeys(lKeys: Array[String], rKeys: Array[String]): Array[String] = {
        // Fast path for small RHS size (the common case when merging a small
        // patch into a large target object), avoiding the cost of constructing
        // and probing a hash set: instead, perform a nested loop where the LHS
        // is scanned and matching RHS entries are marked as null to be skipped.
        // Via local microbenchmarks simulating a "worst-case" (RHS keys all new),
        // the threshold of `8` was empirically determined to be a good tradeoff
        // between allocation + hashing costs vs. nested loop array scans.
        if (rKeys.length <= 8) {
          val rKeysCopy = new Array[String](rKeys.length)
          rKeys.copyToArray(rKeysCopy)
          var i = 0
          var numNewRKeys = rKeysCopy.length
          while (i < lKeys.length) {
            val lKey = lKeys(i)
            var j = 0
            while (j < rKeysCopy.length) {
              // This LHS key is in the RHS, so mark it to be skipped in output:
              if (lKey == rKeysCopy(j)) {
                rKeysCopy(j) = null
                numNewRKeys -= 1
              }
              j += 1
            }
            i += 1
          }
          // Combine lKeys with non-null elements of rKeysCopy:
          if (numNewRKeys == 0) {
            lKeys
          } else {
            val outArray = new Array[String](lKeys.length + numNewRKeys)
            System.arraycopy(lKeys, 0, outArray, 0, lKeys.length)
            var outIdx = lKeys.length
            var j = 0
            while (j < rKeysCopy.length) {
              if (rKeysCopy(j) != null) {
                outArray(outIdx) = rKeysCopy(j)
                outIdx += 1
              }
              j += 1
            }
            outArray
          }
        } else {
          // Fallback: Use LinkedHashSet for large RHS arrays (preserves order, avoids quadratic):
          val allKeys = new java.util.LinkedHashSet[String](lKeys.length + rKeys.length)
          var li = 0
          while (li < lKeys.length) { allKeys.add(lKeys(li)); li += 1 }
          var ri = 0
          while (ri < rKeys.length) { allKeys.add(rKeys(ri)); ri += 1 }
          allKeys.toArray(new Array[String](allKeys.size))
        }
      }
      recPair(target.value, patch.value)
    },
    builtin("prune", "a") { (pos, ev, s: Val) =>
      def filter(x: Val) = x match {
        case c: Val.Arr if c.length == 0                 => false
        case c: Val.Obj if c.visibleKeyNames.length == 0 => false
        case Val.Null(_)                                 => false
        case _                                           => true
      }
      def rec(x: Eval): Val = x.value match {
        case o: Val.Obj =>
          val bindings: Array[(String, Val.Obj.Member)] = for {
            k <- o.visibleKeyNames
            v = rec(o.value(k, pos.fileScope.noOffsetPos)(ev))
            if filter(v)
          } yield (k, new Val.Obj.ConstMember(false, Visibility.Normal, v))
          Val.Obj.mk(pos, bindings)
        case a: Val.Arr =>
          Val.Arr(pos, a.asLazyArray.map(rec).filter(filter))
        case x => x
      }
      rec(s)
    }
  )
}
