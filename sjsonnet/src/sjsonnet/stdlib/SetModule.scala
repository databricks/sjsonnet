package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable
import scala.collection.Searching.*

object SetModule extends AbstractFunctionModule {
  def name = "set"

  private val dummyPos: Position = new Position(null, 0)

  private object Set_ extends Val.Builtin2("set", "arr", "keyF", Array(null, Val.False(dummyPos))) {
    def evalRhs(arr: Lazy, keyF: Lazy, ev: EvalScope, pos: Position): Val = {
      uniqArr(pos, ev, sortArr(pos, ev, arr.force, keyF.force), keyF.force)
    }
  }

  private def applyKeyFunc(elem: Val, keyF: Val, pos: Position, ev: EvalScope): Val = {
    keyF match {
      case keyFFunc: Val.Func =>
        keyFFunc.apply1(elem, pos.noOffset)(ev, TailstrictModeDisabled).force
      case _ => elem
    }
  }

  private def toArrOrString(arg: Val, pos: Position, ev: EvalScope) = {
    arg match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.value).asLazyArray
      case _            => Error.fail(f"Argument must be either arrays or strings")
    }
  }

  private def validateSet(ev: EvalScope, pos: Position, keyF: Val, arr: Val): Unit = {
    if (ev.settings.throwErrorForInvalidSets) {
      val sorted = uniqArr(pos.noOffset, ev, sortArr(pos.noOffset, ev, arr, keyF), keyF)
      if (!ev.equal(arr, sorted)) {
        Error.fail("Set operation on " + arr.force.prettyName + " was called with a non-set")
      }
    }
  }

  private def existsInSet(
      ev: EvalScope,
      pos: Position,
      keyF: Val,
      arr: mutable.IndexedSeq[? <: Lazy],
      toFind: Val): Boolean = {
    val appliedX = applyKeyFunc(toFind, keyF, pos, ev)
    arr
      .search(appliedX)((toFind: Lazy, value: Lazy) => {
        val appliedValue = applyKeyFunc(value.force, keyF, pos, ev)
        ev.compare(toFind.force, appliedValue)
      })
      .isInstanceOf[Found]
  }

  private def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val): Val = {
    val arrValue = toArrOrString(arr, pos, ev)
    if (arrValue.length <= 1) {
      return arr
    }

    val out = new mutable.ArrayBuilder.ofRef[Lazy]
    // Set a reasonable size hint - in the worst case (no duplicates), we'll need arrValue.length elements
    out.sizeHint(arrValue.length)
    for (v <- arrValue) {
      val outResult = out.result()
      if (outResult.length == 0) {
        out.+=(v)
      } else if (keyF.isInstanceOf[Val.False]) {
        if (!ev.equal(outResult.last.force, v.force)) {
          out.+=(v)
        }
      } else if (!keyF.isInstanceOf[Val.False]) {
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val o1Key = keyFFunc.apply1(v, pos.noOffset)(ev, TailstrictModeDisabled)
        val o2Key = keyFFunc.apply1(outResult.last, pos.noOffset)(ev, TailstrictModeDisabled)
        if (!ev.equal(o1Key, o2Key)) {
          out.+=(v)
        }
      }
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
          val keys: Array[Val] = vs.map(v =>
            keyFFunc(Array(v.force), null, pos.noOffset)(ev, TailstrictModeDisabled).force
          )
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
            indices.sortBy(i => keys(i).cast[Val.Num].asDouble)
          } else if (keyType == classOf[Val.Arr]) {
            indices.sortBy(i => keys(i).cast[Val.Arr])(ev.compare(_, _))
          } else {
            Error.fail("Cannot sort with key values that are " + keys(0).prettyName + "s")
          }

          sortedIndices.map(i => vs(i))
        } else {
          val keyType = vs(0).force.getClass
          if (classOf[Val.Bool].isAssignableFrom(keyType)) {
            Error.fail("Cannot sort with values that are booleans")
          }
          if (!vs.forall(_.force.getClass == keyType))
            Error.fail("Cannot sort with values that are not all the same type")

          if (keyType == classOf[Val.Str]) {
            vs.map(_.force.cast[Val.Str]).sortBy(_.asString)(Util.CodepointStringOrdering)
          } else if (keyType == classOf[Val.Num]) {
            vs.map(_.force.cast[Val.Num]).sortBy(_.asDouble)
          } else if (keyType == classOf[Val.Arr]) {
            vs.map(_.force.cast[Val.Arr]).sortBy(identity)(ev.compare(_, _))
          } else if (keyType == classOf[Val.Obj]) {
            Error.fail("Unable to sort array of objects without key function")
          } else {
            Error.fail("Cannot sort array of " + vs(0).force.prettyName)
          }
        }
      )
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    val chars = new Array[Lazy](str.codePointCount(0, str.length))
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
          val out = new mutable.ArrayBuilder.ofRef[Lazy]
          out.sizeHint(a.length + b.length)

          var idxA = 0
          var idxB = 0

          while (idxA < a.length && idxB < b.length) {
            val elemA = a(idxA).force
            val elemB = b(idxB).force

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

        val out = new mutable.ArrayBuilder.ofRef[Lazy]
        // Set a reasonable size hint - intersection will be at most the size of the smaller set
        out.sizeHint(math.min(a.length, b.length))

        var idxA = 0
        var idxB = 0

        while (idxA < a.length && idxB < b.length) {
          val elemA = a(idxA).force
          val elemB = b(idxB).force

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
        val out = new mutable.ArrayBuilder.ofRef[Lazy]
        // Set a reasonable size hint - difference will be at most the size of the first set
        out.sizeHint(a.length)

        var idxA = 0
        var idxB = 0

        while (idxA < a.length) {
          val elemA = a(idxA).force
          val keyA = applyKeyFunc(elemA, keyF, pos, ev)

          // Advance idxB to find first element >= keyA
          var foundEqual = false
          var continue = true
          while (idxB < b.length && continue) {
            val elemB = b(idxB).force
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
