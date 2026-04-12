package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable
import scala.collection.Searching.*

object SetModule extends AbstractFunctionModule {
  def name = "set"

  private object Set_ extends Val.Builtin2("set", "arr", "keyF", Array(null, Val.False(dummyPos))) {
    def evalRhs(arr: Eval, keyF: Eval, ev: EvalScope, pos: Position): Val = {
      uniqArr(pos, ev, sortArr(pos, ev, arr.value, keyF.value), keyF.value)
    }
  }

  private def applyKeyFunc(elem: Val, keyF: Val, pos: Position, ev: EvalScope): Val = {
    keyF match {
      case keyFFunc: Val.Func =>
        keyFFunc.apply1(elem, pos.noOffset)(ev, TailstrictModeDisabled).value
      case _ => elem
    }
  }

  private def toArrOrString(arg: Val, pos: Position, ev: EvalScope) = {
    arg match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.str).asLazyArray
      case _            => Error.fail(f"Argument must be either arrays or strings")
    }
  }

  private def validateSet(ev: EvalScope, pos: Position, keyF: Val, arr: Val): Unit = {
    if (ev.settings.throwErrorForInvalidSets) {
      val sorted = uniqArr(pos.noOffset, ev, sortArr(pos.noOffset, ev, arr, keyF), keyF)
      if (!ev.equal(arr, sorted)) {
        Error.fail("Set operation on " + arr.value.prettyName + " was called with a non-set")
      }
    }
  }

  private def existsInSet(
      ev: EvalScope,
      pos: Position,
      keyF: Val,
      arr: mutable.IndexedSeq[? <: Eval],
      toFind: Val): Boolean = {
    val appliedX = applyKeyFunc(toFind, keyF, pos, ev)
    arr
      .search(appliedX)((toFind: Eval, value: Eval) => {
        val appliedValue = applyKeyFunc(value.value, keyF, pos, ev)
        ev.compare(toFind.value, appliedValue)
      })
      .isInstanceOf[Found]
  }

  private def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val): Val = {
    val arrValue = toArrOrString(arr, pos, ev)
    if (arrValue.length <= 1) {
      return arr
    }

    val out = new mutable.ArrayBuilder.ofRef[Eval]
    // Set a reasonable size hint - in the worst case (no duplicates), we'll need arrValue.length elements
    out.sizeHint(arrValue.length)

    var lastAddedKey: Val = null
    var i = 0
    while (i < arrValue.length) {
      val v = arrValue(i)
      val vKey =
        if (keyF.isInstanceOf[Val.False]) v.value
        else keyF.asInstanceOf[Val.Func].apply1(v, pos.noOffset)(ev, TailstrictModeDisabled)
      if (lastAddedKey == null || !ev.equal(vKey, lastAddedKey)) {
        out.+=(v)
        lastAddedKey = vKey
      }
      i += 1
    }

    Val.Arr(pos, out.result())
  }

  private def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    val vs = toArrOrString(arr, pos, ev)
    if (vs.length <= 1) {
      arr
    } else {
      val keyFFunc =
        if (keyF == null || keyF.isInstanceOf[Val.False]) null else keyF.asInstanceOf[Val.Func]
      Val.Arr(
        pos,
        if (keyFFunc != null) {
          // Reuse a single-element argument buffer across all key function calls
          // to avoid allocating a new Array(1) per element.
          val keys = new Array[Val](vs.length)
          val argBuf = new Array[Val](1)
          var i = 0
          while (i < vs.length) {
            argBuf(0) = vs(i).value
            keys(i) = keyFFunc(argBuf, null, pos.noOffset)(ev, TailstrictModeDisabled).value
            i += 1
          }
          val keyType = keys(0).getClass
          if (classOf[Val.Bool].isAssignableFrom(keyType)) {
            Error.fail("Cannot sort with key values that are booleans")
          }
          if (!keys.forall(_.getClass == keyType)) {
            Error.fail("Cannot sort with key values that are not all the same type")
          }

          val indices = Array.range(0, vs.length)

          val sortedIndices = if (keyType == classOf[Val.Str]) {
            indices.sortBy(i => keys(i).cast[Val.Str].asString)(Util.CodepointStringOrdering)
          } else if (keyType == classOf[Val.Num]) {
            // Extract doubles into primitive array for unboxed comparison,
            // avoiding repeated Val.Num cast + Double boxing per comparison.
            val dkeys = new Array[Double](keys.length)
            var di = 0
            while (di < dkeys.length) {
              dkeys(di) = keys(di).asInstanceOf[Val.Num].asDouble; di += 1
            }
            indices.sortWith((a, b) => dkeys(a) < dkeys(b))
          } else if (keyType == classOf[Val.Arr]) {
            indices.sortBy(i => keys(i).cast[Val.Arr])(ev.compare(_, _))
          } else {
            Error.fail("Cannot sort with key values that are " + keys(0).prettyName + "s")
          }

          // Use while-loop instead of .map() to avoid closure + iterator allocation
          val result = new Array[Eval](sortedIndices.length)
          var j = 0
          while (j < sortedIndices.length) {
            result(j) = vs(sortedIndices(j))
            j += 1
          }
          result
        } else {
          // Force all lazy elements to strict values using while-loop
          val strict = new Array[Val](vs.length)
          var i = 0
          while (i < vs.length) {
            strict(i) = vs(i).value
            i += 1
          }
          val keyType = strict(0).getClass
          if (classOf[Val.Bool].isAssignableFrom(keyType)) {
            Error.fail("Cannot sort with values that are booleans")
          }
          if (!strict.forall(_.getClass == keyType))
            Error.fail("Cannot sort with values that are not all the same type")

          // Sort in-place to avoid intermediate array allocations
          if (keyType == classOf[Val.Str]) {
            java.util.Arrays.sort(
              strict.asInstanceOf[Array[AnyRef]],
              (a: AnyRef, b: AnyRef) =>
                Util.compareStringsByCodepoint(
                  a.asInstanceOf[Val.Str].asString,
                  b.asInstanceOf[Val.Str].asString
                )
            )
          } else if (keyType == classOf[Val.Num]) {
            // Primitive double sort: extract doubles, sort primitively (DualPivotQuicksort),
            // then reconstruct Val.Num array. Avoids Comparator virtual dispatch + boxing.
            val n = strict.length
            val doubles = new Array[Double](n)
            var di = 0
            while (di < n) {
              doubles(di) = strict(di).asInstanceOf[Val.Num].asDouble; di += 1
            }
            java.util.Arrays.sort(doubles)
            di = 0
            while (di < n) {
              strict(di) = Val.cachedNum(pos, doubles(di)); di += 1
            }
          } else if (keyType == classOf[Val.Arr]) {
            java.util.Arrays.sort(
              strict.asInstanceOf[Array[AnyRef]],
              (a: AnyRef, b: AnyRef) => ev.compare(a.asInstanceOf[Val.Arr], b.asInstanceOf[Val.Arr])
            )
          } else if (keyType == classOf[Val.Obj]) {
            Error.fail("Unable to sort array of objects without key function")
          } else {
            Error.fail("Cannot sort array of " + strict(0).prettyName)
          }
          strict
        }
      )
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    val chars = new Array[Eval](str.codePointCount(0, str.length))
    var charIndex = 0
    var i = 0
    while (i < str.length) {
      val codePoint = str.codePointAt(i)
      chars(charIndex) = Val.Str(pos, Character.toString(codePoint))
      i += Character.charCount(codePoint)
      charIndex += 1
    }
    Val.Arr(pos, chars)
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(Set_),
    builtin("slice", "indexable", "index", "end", "step") {
      (pos, ev, indexable: Val, index: Option[Int], _end: Option[Int], _step: Option[Int]) =>
        Util.slice(pos, ev, indexable, index, _end, _step)
    },
    builtinWithDefaults("uniq", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      uniqArr(pos, ev, args(0), args(1))
    },
    builtinWithDefaults("sort", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      sortArr(pos, ev, args(0), args(1))
    },
    builtinWithDefaults("setUnion", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(0))
        validateSet(ev, pos, keyF, args(1))

        val a = toArrOrString(args(0), pos, ev)
        val b = toArrOrString(args(1), pos, ev)

        if (a.isEmpty) {
          args(1)
        } else if (b.isEmpty) {
          args(0)
        } else {
          val out = new mutable.ArrayBuilder.ofRef[Eval]
          out.sizeHint(a.length + b.length)

          var idxA = 0
          var idxB = 0

          while (idxA < a.length && idxB < b.length) {
            val elemA = a(idxA).value
            val elemB = b(idxB).value

            val keyA = applyKeyFunc(elemA, keyF, pos, ev)
            val keyB = applyKeyFunc(elemB, keyF, pos, ev)

            val cmp = ev.compare(keyA, keyB)
            if (cmp < 0) {
              // keyA < keyB, take from a
              out.+=(a(idxA))
              idxA += 1
            } else if (cmp > 0) {
              // keyA > keyB, take from b
              out.+=(b(idxB))
              idxB += 1
            } else {
              // keyA == keyB, take one and skip duplicate
              out.+=(a(idxA))
              idxA += 1
              idxB += 1
            }
          }

          // Add remaining elements from a or b
          while (idxA < a.length) {
            out.+=(a(idxA))
            idxA += 1
          }
          while (idxB < b.length) {
            out.+=(b(idxB))
            idxB += 1
          }

          Val.Arr(pos, out.result())
        }
    },
    builtinWithDefaults("setInter", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(0))
        validateSet(ev, pos, keyF, args(1))

        val a = toArrOrString(args(0), pos, ev)
        val b = toArrOrString(args(1), pos, ev)

        val out = new mutable.ArrayBuilder.ofRef[Eval]
        // Set a reasonable size hint - intersection will be at most the size of the smaller set
        out.sizeHint(math.min(a.length, b.length))

        var idxA = 0
        var idxB = 0

        while (idxA < a.length && idxB < b.length) {
          val elemA = a(idxA).value
          val elemB = b(idxB).value

          val keyA = applyKeyFunc(elemA, keyF, pos, ev)
          val keyB = applyKeyFunc(elemB, keyF, pos, ev)

          val cmp = ev.compare(keyA, keyB)
          if (cmp < 0) {
            // keyA < keyB, elemA not in intersection
            idxA += 1
          } else if (cmp > 0) {
            // keyA > keyB, elemB not in intersection
            idxB += 1
          } else {
            // keyA == keyB, found intersection element
            out.+=(a(idxA))
            idxA += 1
            idxB += 1
          }
        }

        Val.Arr(pos, out.result())
    },
    builtinWithDefaults("setDiff", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(0))
        validateSet(ev, pos, keyF, args(1))

        val a = toArrOrString(args(0), pos, ev)
        val b = toArrOrString(args(1), pos, ev)
        val out = new mutable.ArrayBuilder.ofRef[Eval]
        // Set a reasonable size hint - difference will be at most the size of the first set
        out.sizeHint(a.length)

        var idxA = 0
        var idxB = 0

        while (idxA < a.length) {
          val elemA = a(idxA).value
          val keyA = applyKeyFunc(elemA, keyF, pos, ev)

          // Advance idxB to find first element >= keyA
          var foundEqual = false
          var continue = true
          while (idxB < b.length && continue) {
            val elemB = b(idxB).value
            val keyB = applyKeyFunc(elemB, keyF, pos, ev)

            val cmp = ev.compare(keyA, keyB)
            if (cmp <= 0) {
              // keyA <= keyB, found position
              foundEqual = (cmp == 0)
              if (foundEqual) idxB += 1 // Move past the match
              continue = false
            } else {
              // keyA > keyB, keep advancing in b
              idxB += 1
            }
          }

          // Add elemA if we didn't find it in b
          if (!foundEqual) {
            out.+=(a(idxA))
          }

          idxA += 1
        }

        Val.Arr(pos, out.result())
    },
    builtinWithDefaults("setMember", "x" -> null, "arr" -> null, "keyF" -> Val.False(dummyPos)) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(1))
        val arr = args(1).asArr.asLazyArray
        existsInSet(ev, pos, keyF, arr, args(0))
    }
  )
}
