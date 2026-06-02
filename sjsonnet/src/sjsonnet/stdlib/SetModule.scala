package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable
import scala.collection.Searching.*

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-set std.set(arr, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-slice std.slice(indexable, index, end, step)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-uniq std.uniq(arr, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sort std.sort(arr, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-setUnion std.setUnion(a, b, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-setInter std.setInter(a, b, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-setDiff std.setDiff(a, b, keyF=id)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-setMember std.setMember(x, arr, keyF=id)]]
 */
object SetModule extends AbstractFunctionModule {
  def name = "set"

  private val DefaultKeyF = Val.Null(dummyPos)
  private[this] final val SortKindOther = 0
  private[this] final val SortKindString = 1
  private[this] final val SortKindNumber = 2
  private[this] final val SortKindArray = 3
  private[this] final val SortKindBoolean = 4
  private[this] final val SortKindObject = 5
  private[this] final val SortKindNull = 6
  private[this] final val SortKindFunction = 7

  @inline private[this] def sortKind(v: Val): Int = v match {
    case _: Val.Str  => SortKindString
    case _: Val.Num  => SortKindNumber
    case _: Val.Arr  => SortKindArray
    case _: Val.Bool => SortKindBoolean
    case _: Val.Obj  => SortKindObject
    case _: Val.Null => SortKindNull
    case _: Val.Func => SortKindFunction
    case _           => SortKindOther
  }

  @inline private[this] def allSameSortKind(values: Array[Val], kind: Int): Boolean = {
    var i = 1
    while (i < values.length) {
      if (sortKind(values(i)) != kind) return false
      i += 1
    }
    true
  }

  @inline private def isDefaultKeyF(v: Val): Boolean = v.asInstanceOf[AnyRef] eq DefaultKeyF

  @inline private def isIdentityKeyF(v: Val): Boolean =
    v == null || isDefaultKeyF(v) || (v.isInstanceOf[Val.Func] && v.asFunc.isIdentityFunction)

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-set std.set(arr, keyF=id)]].
   *
   * Since: 0.10.0. Group: Sets.
   *
   * Shortcut for std.uniq(std.sort(arr)).
   */
  private object Set_ extends Val.Builtin2("set", "arr", "keyF", Array(null, DefaultKeyF)) {
    def evalRhs(arr: Eval, keyF: Eval, ev: EvalScope, pos: Position): Val = {
      uniqArr(pos, ev, sortArr(pos, ev, arr.value, keyF.value), keyF.value)
    }
  }

  private def applyKeyFunc(elem: Val, keyF: Val, pos: Position, ev: EvalScope): Val = {
    if (isIdentityKeyF(keyF)) elem
    else keyF.asFunc.apply1(elem, pos.noOffset)(ev, TailstrictModeDisabled).value
  }

  private def toArrOrString(arg: Val, pos: Position, ev: EvalScope) = {
    arg match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.str).asLazyArray
      case _            => Error.fail(f"Argument must be either arrays or strings")
    }
  }

  @inline private def trimSetOutput(out: Array[Eval], len: Int): Array[Eval] = {
    if (len == out.length) out
    else java.util.Arrays.copyOf(out, len)
  }

  @inline private def compareDefaultSetKeys(ev: EvalScope, a: Val, b: Val): Int = {
    a match {
      case aNum: Val.Num =>
        b match {
          case bNum: Val.Num => java.lang.Double.compare(aNum.asDouble, bNum.asDouble)
          case _             => ev.compare(a, b)
        }
      case aStr: Val.Str =>
        b match {
          case bStr: Val.Str => Util.compareStringsByCodepoint(aStr.str, bStr.str)
          case _             => ev.compare(a, b)
        }
      case _ => ev.compare(a, b)
    }
  }

  // WHY: default-key set operations are hot in numeric/string workloads. Keep the current
  // forced key at each cursor and compare primitive-like values directly; this avoids repeated
  // Eval.value calls and generic ev.compare dispatch without changing keyF semantics.
  private def setUnionDefaultKeyF(
      pos: Position,
      ev: EvalScope,
      a: Array[? <: Eval],
      b: Array[? <: Eval]): Val.Arr = {
    val out = new Array[Eval](a.length + b.length)

    var outLen = 0
    var idxA = 0
    var idxB = 0
    var elemA = a(idxA).value
    var elemB = b(idxB).value

    while (idxA < a.length && idxB < b.length) {
      val cmp = compareDefaultSetKeys(ev, elemA, elemB)
      if (cmp < 0) {
        out(outLen) = a(idxA)
        outLen += 1
        idxA += 1
        if (idxA < a.length) elemA = a(idxA).value
      } else if (cmp > 0) {
        out(outLen) = b(idxB)
        outLen += 1
        idxB += 1
        if (idxB < b.length) elemB = b(idxB).value
      } else {
        out(outLen) = a(idxA)
        outLen += 1
        idxA += 1
        idxB += 1
        if (idxA < a.length && idxB < b.length) {
          elemA = a(idxA).value
          elemB = b(idxB).value
        }
      }
    }

    while (idxA < a.length) {
      out(outLen) = a(idxA)
      outLen += 1
      idxA += 1
    }
    while (idxB < b.length) {
      out(outLen) = b(idxB)
      outLen += 1
      idxB += 1
    }

    Val.Arr(pos, trimSetOutput(out, outLen))
  }

  private def setInterDefaultKeyF(
      pos: Position,
      ev: EvalScope,
      a: Array[? <: Eval],
      b: Array[? <: Eval]): Val.Arr = {
    if (a.isEmpty || b.isEmpty) return Val.Arr(pos, Array.empty[Eval])

    val out = new Array[Eval](math.min(a.length, b.length))

    var outLen = 0
    var idxA = 0
    var idxB = 0
    var elemA = a(idxA).value
    var elemB = b(idxB).value

    while (idxA < a.length && idxB < b.length) {
      val cmp = compareDefaultSetKeys(ev, elemA, elemB)
      if (cmp < 0) {
        idxA += 1
        if (idxA < a.length) elemA = a(idxA).value
      } else if (cmp > 0) {
        idxB += 1
        if (idxB < b.length) elemB = b(idxB).value
      } else {
        out(outLen) = a(idxA)
        outLen += 1
        idxA += 1
        idxB += 1
        if (idxA < a.length && idxB < b.length) {
          elemA = a(idxA).value
          elemB = b(idxB).value
        }
      }
    }

    Val.Arr(pos, trimSetOutput(out, outLen))
  }

  private def setDiffDefaultKeyF(
      pos: Position,
      ev: EvalScope,
      a: Array[? <: Eval],
      b: Array[? <: Eval]): Val.Arr = {
    if (a.isEmpty) return Val.Arr(pos, Array.empty[Eval])

    val out = new Array[Eval](a.length)

    var outLen = 0
    var idxA = 0
    var idxB = 0
    var elemB: Val = null

    while (idxA < a.length) {
      val elemA = a(idxA).value
      var foundEqual = false
      var continue = true

      while (idxB < b.length && continue) {
        if (elemB == null) elemB = b(idxB).value
        val cmp = compareDefaultSetKeys(ev, elemA, elemB)
        if (cmp <= 0) {
          foundEqual = cmp == 0
          if (foundEqual) {
            idxB += 1
            elemB = null
          }
          continue = false
        } else {
          idxB += 1
          elemB = null
        }
      }

      if (!foundEqual) {
        out(outLen) = a(idxA)
        outLen += 1
      }

      idxA += 1
    }

    Val.Arr(pos, trimSetOutput(out, outLen))
  }

  private def validateSet(ev: EvalScope, pos: Position, keyF: Val, arr: Val): Unit = {
    if (ev.settings.throwErrorForInvalidSets) {
      val sorted = uniqArr(pos.noOffset, ev, sortArr(pos.noOffset, ev, arr, keyF), keyF)
      val isSet = arr match {
        case Val.Str(_, str) =>
          sorted match {
            case Val.Str(_, sortedStr) => str == sortedStr
            case sortedArr: Val.Arr    =>
              val sb = new java.lang.StringBuilder(str.length)
              var i = 0
              while (i < sortedArr.length) {
                sb.append(sortedArr.value(i).asString)
                i += 1
              }
              str == sb.toString
            case _ => false
          }
        case _ =>
          ev.equal(arr, sorted)
      }
      if (!isSet) {
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
    if (arr.isEmpty) return false
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
        if (isIdentityKeyF(keyF)) v.value
        else keyF.asFunc.apply1(v, pos.noOffset)(ev, TailstrictModeDisabled)
      if (lastAddedKey == null || !ev.equal(vKey, lastAddedKey)) {
        out.+=(v)
        lastAddedKey = vKey
      }
      i += 1
    }

    Val.Arr(pos, out.result())
  }

  private def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val): Val = {
    // Fast path: range arrays are already sorted ascending by construction.
    // Avoids O(n) materialization + O(n log n) sort for already-sorted data.
    if (isIdentityKeyF(keyF) || keyF.isInstanceOf[Val.False]) {
      arr match {
        case a: Val.Arr =>
          val sorted = a.asSortedIfKnown(pos)
          if (sorted != null) return sorted
        case _ =>
      }
    }
    if (keyF != null && keyF.isInstanceOf[Val.Func] && keyF.asFunc.isUnaryMinusParamFunction) {
      arr match {
        case a: Val.Arr =>
          val sorted = a.asSortedIfKnown(pos)
          if (sorted != null) return sorted.reversed(pos)
        case _ =>
      }
    }
    sortArrSlow(pos, ev, arr, keyF)
  }

  private def sortArrSlow(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    val vs = toArrOrString(arr, pos, ev)
    if (vs.length <= 1) {
      arr
    } else {
      val keyFFunc =
        if (isIdentityKeyF(keyF)) null else keyF.asFunc
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
          val keyType = sortKind(keys(0))
          if (keyType == SortKindBoolean) {
            Error.fail("Cannot sort with key values that are booleans")
          }
          if (!allSameSortKind(keys, keyType)) {
            Error.fail("Cannot sort with key values that are not all the same type")
          }

          val indices = Array.range(0, vs.length)

          val sortedIndices = if (keyType == SortKindString) {
            indices.sortBy(i => keys(i).cast[Val.Str].asString)(Util.CodepointStringOrdering)
          } else if (keyType == SortKindNumber) {
            // Extract doubles into primitive array for unboxed comparison,
            // avoiding repeated Val.Num cast + Double boxing per comparison.
            val dkeys = new Array[Double](keys.length)
            var di = 0
            while (di < dkeys.length) {
              dkeys(di) = keys(di).asInstanceOf[Val.Num].asDouble; di += 1
            }
            indices.sortWith((a, b) => dkeys(a) < dkeys(b))
          } else if (keyType == SortKindArray) {
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
          val keyType = sortKind(strict(0))
          if (keyType == SortKindBoolean) {
            Error.fail("Cannot sort with values that are booleans")
          }
          if (!allSameSortKind(strict, keyType))
            Error.fail("Cannot sort with values that are not all the same type")

          // Sort in-place to avoid intermediate array allocations
          if (keyType == SortKindString) {
            java.util.Arrays.sort(
              strict.asInstanceOf[Array[AnyRef]],
              (a: AnyRef, b: AnyRef) =>
                Util.compareStringsByCodepoint(
                  a.asInstanceOf[Val.Str].asString,
                  b.asInstanceOf[Val.Str].asString
                )
            )
          } else if (keyType == SortKindNumber) {
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
          } else if (keyType == SortKindArray) {
            java.util.Arrays.sort(
              strict.asInstanceOf[Array[AnyRef]],
              (a: AnyRef, b: AnyRef) => ev.compare(a.asInstanceOf[Val.Arr], b.asInstanceOf[Val.Arr])
            )
          } else if (keyType == SortKindObject) {
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
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-slice std.slice(indexable, index, end, step)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * Selects the elements of an array or a string from index to end with step and returns an array
     * or a string respectively.
     *
     * Note that it's recommended to use dedicated slicing syntax both for arrays and strings (e.g.
     * arr[0:4:1] instead of std.slice(arr, 0, 4, 1)).
     */
    builtin("slice", "indexable", "index", "end", "step") {
      (
          pos,
          ev,
          indexable: Val,
          index: Option[Double],
          _end: Option[Double],
          _step: Option[Double]) =>
        Util.slice(pos, ev, indexable, index, _end, _step)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-uniq std.uniq(arr, keyF=id)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * Removes successive duplicates. When given a sorted array, removes all duplicates.
     *
     * Optional argument keyF is a single argument function used to extract comparison key from each
     * array element. Default value is identity function keyF=function(x) x.
     */
    builtinWithDefaults("uniq", "arr" -> null, "keyF" -> DefaultKeyF) { (args, pos, ev) =>
      uniqArr(pos, ev, args(0), args(1))
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sort std.sort(arr, keyF=id)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * Sorts the array using the <= operator.
     *
     * Optional argument keyF is a single argument function used to extract comparison key from each
     * array element. Default value is identity function keyF=function(x) x.
     */
    builtinWithDefaults("sort", "arr" -> null, "keyF" -> DefaultKeyF) { (args, pos, ev) =>
      sortArr(pos, ev, args(0), args(1))
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-setUnion std.setUnion(a, b, keyF=id)]].
     *
     * Since: 0.10.0. Group: Sets.
     *
     * Set union operation (values in any of a or b). Note that + on sets will simply concatenate
     * the arrays, possibly forming an array that is not a set (due to not being ordered without
     * duplicates).
     */
    builtinWithDefaults("setUnion", "a" -> null, "b" -> null, "keyF" -> DefaultKeyF) {
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
        } else if (isDefaultKeyF(keyF)) {
          setUnionDefaultKeyF(pos, ev, a, b)
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
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-setInter std.setInter(a, b, keyF=id)]].
     *
     * Since: 0.10.0. Group: Sets.
     *
     * Set intersection operation (values in both a and b).
     */
    builtinWithDefaults("setInter", "a" -> null, "b" -> null, "keyF" -> DefaultKeyF) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(0))
        validateSet(ev, pos, keyF, args(1))

        val a = toArrOrString(args(0), pos, ev)
        val b = toArrOrString(args(1), pos, ev)

        if (isDefaultKeyF(keyF)) {
          setInterDefaultKeyF(pos, ev, a, b)
        } else {
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
        }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-setDiff std.setDiff(a, b, keyF=id)]].
     *
     * Since: 0.10.0. Group: Sets.
     *
     * Set difference operation (values in a but not b).
     */
    builtinWithDefaults("setDiff", "a" -> null, "b" -> null, "keyF" -> DefaultKeyF) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(0))
        validateSet(ev, pos, keyF, args(1))

        val a = toArrOrString(args(0), pos, ev)
        val b = toArrOrString(args(1), pos, ev)
        if (isDefaultKeyF(keyF)) {
          setDiffDefaultKeyF(pos, ev, a, b)
        } else {
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
        }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-setMember std.setMember(x, arr, keyF=id)]].
     *
     * Since: 0.10.0. Group: Sets.
     *
     * Returns true if x is a member of array, otherwise false.
     */
    builtinWithDefaults("setMember", "x" -> null, "arr" -> null, "keyF" -> DefaultKeyF) {
      (args, pos, ev) =>
        val keyF = args(2)
        validateSet(ev, pos, keyF, args(1))
        val arr = toArrOrString(args(1), pos, ev)
        existsInSet(ev, pos, keyF, arr, args(0))
    }
  )
}
