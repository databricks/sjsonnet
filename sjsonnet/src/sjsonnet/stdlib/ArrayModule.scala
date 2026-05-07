package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-minArray std.minArray(arr, keyF, onEmpty)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-maxArray std.maxArray(arr, keyF, onEmpty)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-all std.all(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-any std.any(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-count std.count(arr, x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-filter std.filter(func, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-map std.map(func, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-mapWithIndex std.mapWithIndex(func, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-find std.find(value, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-flattenArrays std.flattenArrays(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-flattenDeepArray std.flattenDeepArray(value)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-reverse std.reverse(arrs)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-member std.member(arr, x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-range std.range(from, to)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-foldl std.foldl(func, arr, init)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-foldr std.foldr(func, arr, init)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-flatMap std.flatMap(func, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-filterMap std.filterMap(filter_func, map_func, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-repeat std.repeat(what, count)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-makeArray std.makeArray(sz, func)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-contains std.contains(arr, elem)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-remove std.remove(arr, elem)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-removeAt std.removeAt(arr, idx)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-sum std.sum(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-avg std.avg(arr)]]
 */
object ArrayModule extends AbstractFunctionModule {
  def name = "array"

  private val DefaultKeyF = Val.Null(dummyPos)
  private val DefaultOnEmpty = Val.Null(dummyPos)

  @inline private def isDefaultKeyF(v: Val): Boolean = v.asInstanceOf[AnyRef] eq DefaultKeyF
  @inline private def isDefaultOnEmpty(v: Val): Boolean =
    v.asInstanceOf[AnyRef] eq DefaultOnEmpty

  private def applyArrayKey(keyF: Val, elem: Val, pos: Position, ev: EvalScope): Val =
    if (isDefaultKeyF(keyF)) elem
    else keyF.asFunc.apply1(elem, pos.noOffset)(ev, TailstrictModeDisabled)

  private def rangeSum(arr: Val.RangeArr): Double = {
    val len = arr.length
    if (len == 0) 0.0
    else (arr.value(0).asDouble + arr.value(len - 1).asDouble) * len / 2.0
  }

  private def byteArraySum(arr: Val.ByteArr): Double = {
    val bytes = arr.rawBytes
    var sum = 0.0
    var i = 0
    while (i < bytes.length) {
      sum += (bytes(i) & 0xff)
      i += 1
    }
    sum
  }

  private def removeAtView(arr: Val.Arr, removeIdx: Int): Val.Arr = {
    val len = arr.length
    if (len == 1) Val.Arr(arr.pos, Val.Arr.EMPTY_EVAL_ARRAY)
    else if (removeIdx == 0) arr.sliced(arr.pos, 1, len, 1)
    else if (removeIdx == len - 1) arr.sliced(arr.pos, 0, removeIdx, 1)
    else {
      val left = arr.sliced(arr.pos, 0, removeIdx, 1)
      val right = arr.sliced(arr.pos, removeIdx + 1, len, 1)
      left.concat(arr.pos, right)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-minArray std.minArray(arr, keyF, onEmpty)]].
   *
   * Since: 0.21.0. Group: Arrays.
   *
   * Return the minimum of all elements in arr. If keyF is provided, it is called on each element of
   * the array and should return a comparator value, and in this case minArray will return an
   * element with the minimum comparator value. If onEmpty is provided, and arr is empty, then
   * minArray will return the provided onEmpty value. If onEmpty is not provided, then an empty arr
   * will raise an error.
   */
  private object MinArray
      extends Val.Builtin(
        "minArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, DefaultKeyF, DefaultOnEmpty)
      ) {
    override def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).value.asArr
      val keyF = args(1).value
      val onEmpty = args(2).value
      if (arr.length == 0) {
        if (isDefaultOnEmpty(onEmpty)) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty
        }
      } else {
        var bestElem = arr.value(0)
        var bestKey = applyArrayKey(keyF, bestElem, pos, ev)
        Util.compareJsonnetStd(bestKey, bestKey, ev)
        var i = 1
        while (i < arr.length) {
          val elem = arr.value(i)
          val v = applyArrayKey(keyF, elem, pos, ev)
          if (Util.compareJsonnetStd(v, bestKey, ev) < 0) {
            bestKey = v
            bestElem = elem
          }
          i += 1
        }
        bestElem
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-maxArray std.maxArray(arr, keyF, onEmpty)]].
   *
   * Since: 0.21.0. Group: Arrays.
   *
   * Return the maximum of all elements in arr. If keyF is provided, it is called on each element of
   * the array and should return a comparator value, and in this case maxArray will return an
   * element with the maximum comparator value. If onEmpty is provided, and arr is empty, then
   * maxArray will return the provided onEmpty value. If onEmpty is not provided, then an empty arr
   * will raise an error.
   */
  private object MaxArray
      extends Val.Builtin(
        "maxArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, DefaultKeyF, DefaultOnEmpty)
      ) {
    override def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).value.asArr
      val keyF = args(1).value
      val onEmpty = args(2).value
      if (arr.length == 0) {
        if (isDefaultOnEmpty(onEmpty)) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty
        }
      } else {
        var bestElem = arr.value(0)
        var bestKey = applyArrayKey(keyF, bestElem, pos, ev)
        Util.compareJsonnetStd(bestKey, bestKey, ev)
        var i = 1
        while (i < arr.length) {
          val elem = arr.value(i)
          val v = applyArrayKey(keyF, elem, pos, ev)
          if (Util.compareJsonnetStd(v, bestKey, ev) > 0) {
            bestKey = v
            bestElem = elem
          }
          i += 1
        }
        bestElem
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-all std.all(arr)]].
   *
   * Since: 0.19.0. Group: Arrays.
   *
   * Return true if all elements of arr is true, false otherwise. all([]) evaluates to true.
   *
   * It's an error if 1) arr is not an array, or 2) arr contains non-boolean values.
   */
  private object All extends Val.Builtin1("all", "arr") {
    def evalRhs(arr: Eval, ev: EvalScope, pos: Position): Val = {
      val a = arr.value.asArr
      var i = 0
      while (i < a.length) {
        if (!a.value(i).asBoolean) return Val.staticFalse
        i += 1
      }
      Val.staticTrue
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-any std.any(arr)]].
   *
   * Since: 0.19.0. Group: Arrays.
   *
   * Return true if any element of arr is true, false otherwise. any([]) evaluates to false.
   *
   * It's an error if 1) arr is not an array, or 2) arr contains non-boolean values.
   */
  private object Any extends Val.Builtin1("any", "arr") {
    def evalRhs(arr: Eval, ev: EvalScope, pos: Position): Val = {
      val a = arr.value.asArr
      var i = 0
      while (i < a.length) {
        if (a.value(i).asBoolean) return Val.staticTrue
        i += 1
      }
      Val.staticFalse
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-count std.count(arr, x)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Return the number of times that x occurs in arr.
   */
  private object Count extends Val.Builtin2("count", "arr", "x") {
    def evalRhs(arr: Eval, x: Eval, ev: EvalScope, pos: Position): Val = {
      var count = 0
      arr.value.asArr.foreach(v => if (ev.equal(v.value, x.value)) count += 1)
      Val.cachedNum(pos, count.toDouble)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-filter std.filter(func, arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Return a new array containing all the elements of arr for which the func function returns true.
   */
  private object Filter extends Val.Builtin2("filter", "func", "arr") {
    def evalRhs(_func: Eval, arr: Eval, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val source = arr.value.asArr
      val len = source.length
      var i = 0
      val func = _func.value.asFunc
      if (func.isInstanceOf[Val.Builtin] || func.params.names.length != 1) {
        while (i < len) {
          val elem = source.eval(i)
          if (!func.apply1(elem, p)(ev, TailstrictModeDisabled).asBoolean) {
            var b = new Array[Eval](len - 1)
            var k = 0
            while (k < i) {
              b(k) = source.eval(k)
              k += 1
            }
            var j = i + 1
            while (j < len) {
              val elem = source.eval(j)
              if (func.apply1(elem, p)(ev, TailstrictModeDisabled).asBoolean) {
                b(i) = elem
                i += 1
              }
              j += 1
            }
            if (i != b.length) b = java.util.Arrays.copyOf(b, i)
            return Val.Arr(pos, b)
          }
          i += 1
        }
      } else {
        // Single-param non-builtin can benefit from scope reuse: We compute a strict boolean from
        // the function, there's no risk of the scope leaking (and being invalid at a later point)
        val funDefFileScope: FileScope = func.pos match {
          case null => p.fileScope
          case pp   => pp.fileScope
        }
        val newScope: ValScope = func.defSiteValScope.extendBy(1)
        val scopeIdx = newScope.length - 1
        while (i < len) {
          val elem = source.eval(i)
          newScope.bindings(scopeIdx) = elem
          if (!func.evalRhsResolved(newScope, ev, funDefFileScope, p).asBoolean) {
            var b = new Array[Eval](len - 1)
            var k = 0
            while (k < i) {
              b(k) = source.eval(k)
              k += 1
            }
            var j = i + 1
            while (j < len) {
              val elem = source.eval(j)
              newScope.bindings(scopeIdx) = elem
              if (func.evalRhsResolved(newScope, ev, funDefFileScope, p).asBoolean) {
                b(i) = elem
                i += 1
              }
              j += 1
            }
            if (i != b.length) b = java.util.Arrays.copyOf(b, i)
            return Val.Arr(pos, b)
          }
          i += 1
        }
      }
      source
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-map std.map(func, arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Apply the given function to every element of the array to form a new array.
   */
  private object Map_ extends Val.Builtin2("map", "func", "arr") {
    def evalRhs(_func: Eval, arr: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      val arg = arr.value
      arg match {
        case Val.Str(_, str) => evalStr(func, str, ev, pos.noOffset)
        case a: Val.Arr      => Val.Arr.mapped(pos, a, func, pos.noOffset, ev)
        case _               => Val.Arr.mapped(pos, arg.asArr, func, pos.noOffset, ev)
      }
    }

    private def evalStr(_func: Val.Func, arg: String, ev: EvalScope, pos: Position): Val.Arr = {
      Val.Arr.mapped(pos, stringChars(pos, arg), _func, pos.noOffset, ev)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-mapWithIndex std.mapWithIndex(func, arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Similar to map above, but it also passes to the function the element's index in the array. The
   * function func is expected to take the index as the first parameter and the element as the
   * second.
   */
  private object MapWithIndex extends Val.Builtin2("mapWithIndex", "func", "arr") {
    def evalRhs(_func: Eval, _arr: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      val arr = _arr.value match {
        case Val.Str(_, str) => stringChars(pos, str)
        case v               => v.asArr
      }
      Val.Arr.mappedWithIndex(pos, arr, func, pos, pos.noOffset, ev)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-find std.find(value, arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Returns an array that contains the indexes of all occurrences of value in arr.
   */
  private object Find extends Val.Builtin2("find", "value", "arr") {
    def evalRhs(value: Eval, _arr: Eval, ev: EvalScope, pos: Position): Val = {
      val arr = _arr.value.asArr
      val b = new mutable.ArrayBuilder.ofRef[Eval]
      b.sizeHint(arr.length) // Size hint based on array length (worst case)
      var i = 0
      while (i < arr.length) {
        if (ev.equal(arr.value(i), value.value)) {
          val finalI = i
          b.+=(Val.cachedNum(pos, finalI))
        }
        i += 1
      }
      Val.Arr(pos, b.result())
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-flattenArrays std.flattenArrays(arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Concatenate an array of arrays into a single array.
   */
  private object FlattenArrays extends Val.Builtin1("flattenArrays", "arrs") {
    def evalRhs(arrs: Eval, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuilder.ofRef[Eval]
      val arr = arrs.value.asArr
      out.sizeHint(arr.length * 4) // Rough size hint
      for (x <- arr) {
        x.value match {
          case v: Val.Arr =>
            var i = 0
            while (i < v.length) {
              out += v.eval(i)
              i += 1
            }
          case x =>
            Error.fail("binary operator + requires matching types, got array and " + x.prettyName)
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-flattenDeepArray std.flattenDeepArray(value)]].
   *
   * Since: 0.21.0. Group: Arrays.
   *
   * Concatenate an array containing values and arrays into a single flattened array.
   */
  private object FlattenDeepArrays extends Val.Builtin1("flattenDeepArray", "value") {
    def evalRhs(value: Eval, ev: EvalScope, pos: Position): Val = {
      val value0 = value.value
      if (!value0.isInstanceOf[Val.Arr]) {
        return Val.Arr(pos, Array[Eval](value0))
      }
      val arr = value0.asInstanceOf[Val.Arr]
      val out = new mutable.ArrayBuilder.ofRef[Eval]
      out.sizeHint(arr.length)
      val q = new java.util.ArrayDeque[Eval](arr.length)
      var initialIdx = 0
      while (initialIdx < arr.length) {
        q.add(arr.eval(initialIdx))
        initialIdx += 1
      }
      while (!q.isEmpty) {
        q.removeFirst().value match {
          case v: Val.Arr =>
            var i = v.length - 1
            while (i >= 0) { q.push(v.eval(i)); i -= 1 }
          case x => out += x
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-reverse std.reverse(arrs)]].
   *
   * Since: 0.13.0. Group: Arrays.
   *
   * Reverses an array.
   */
  private object Reverse extends Val.Builtin1("reverse", "arrs") {
    def evalRhs(arrs: Eval, ev: EvalScope, pos: Position): Val = {
      // Most arrays reverse as a zero-copy view that shares the same backing array. Array-level
      // lazy views may first materialize thunks so the original and reversed arrays preserve
      // shared callback evaluation semantics.
      arrs.value.asArr.reversed(pos)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-member std.member(arr, x)]].
   *
   * Since: 0.15.0. Group: Arrays.
   *
   * Returns whether x occurs in arr. Argument arr may be an array or a string.
   */
  private object Member extends Val.Builtin2("member", "arr", "x") {
    def evalRhs(arr: Eval, x: Eval, ev: EvalScope, pos: Position): Val = {
      Val.bool(
        pos,
        arr.value match {
          case str: Val.Str =>
            val secondArg = x.value match {
              case Val.Str(_, value) => value
              case n                 =>
                Error.fail("std.member second argument must be a string, got " + n.prettyName)
            }
            str.str.contains(secondArg)
          case a: Val.Arr =>
            var i = 0
            var found = false
            while (i < a.length && !found) {
              if (ev.equal(a.value(i), x.value)) found = true
              i += 1
            }
            found
          case arr =>
            Error.fail(
              "std.member first argument must be an array or a string, got " + arr.prettyName
            )
        }
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-range std.range(from, to)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Return an array of ascending numbers between the two limits, inclusively.
   */
  private object Range extends Val.Builtin2("range", "from", "to") {
    def evalRhs(from: Eval, to: Eval, ev: EvalScope, pos: Position): Val = {
      val fromInt = from.value.asInt
      val toInt = to.value.asInt
      // Use Long arithmetic to detect overflow before allocating
      val sizeLong = toInt.toLong - fromInt.toLong + 1L
      val size =
        if (sizeLong <= 0) 0
        else if (sizeLong > Int.MaxValue)
          Error.fail("std.range result too large: " + sizeLong + " elements")
        else sizeLong.toInt
      // Lazy range: O(1) creation, elements computed on demand.
      // Particularly beneficial for patterns like `std.range(1, 1000000) + [x]`
      // where element-wise comparison never needs the full backing array.
      if (size == 0) Val.Arr(pos, Val.Arr.EMPTY_EVAL_ARRAY)
      else Val.Arr.range(pos, fromInt, size)
    }
  }

  /**
   * Detect string-concat patterns in foldl function bodies and use StringBuilder for O(n) total.
   * Supported patterns:
   *   - `function(acc, elem) acc + elem`
   *   - `function(acc, elem) acc + SEP + elem` (separator)
   *   - `function(acc, elem) if acc == "" then elem else acc + SEP + elem` (conditional separator)
   * Returns null if the pattern doesn't match, letting the caller fall through to the general path.
   */
  private def tryStringBuilderFoldl(
      func: Val.Func,
      arr: Val.Arr,
      initStr: String,
      ev: EvalScope,
      pos: Position
  ): Val = {
    val body = func.bodyExpr
    if (body == null) return null
    val base = func.defSiteValScope.bindings.length
    body match {
      case e: Expr.BinaryOp if e.op == Expr.BinaryOp.OP_+ =>
        tryStringBuilderFromBinaryOp(e, base, arr, initStr, null, ev, pos)
      case ifElse: Expr.IfElse =>
        tryStringBuilderFromIfElse(ifElse, base, arr, initStr, ev, pos)
      case _ => null
    }
  }

  /**
   * Match BinaryOp patterns:
   *   - `acc + elem` (simple concat)
   *   - `acc + SEP + elem` (separator concat)
   * If `skipSepForFirst` is non-null, the separator is omitted for the first element.
   */
  private def tryStringBuilderFromBinaryOp(
      e: Expr.BinaryOp,
      base: Int,
      arr: Val.Arr,
      initStr: String,
      skipSepForFirst: String, // non-null means skip sep when acc equals this
      ev: EvalScope,
      pos: Position
  ): Val = {
    (e.lhs, e.rhs) match {
      // Pattern: acc + elem
      case (l: Expr.ValidId, r: Expr.ValidId) if l.nameIdx == base && r.nameIdx == base + 1 =>
        val direct = arr.directBackingArray
        val len = if (direct == null) arr.length else direct.length
        val sb = new java.lang.StringBuilder(initStr.length + len * 8)
        sb.append(initStr)
        var i = 0
        while (i < len) {
          val elem = if (direct == null) arr.value(i) else direct(i).value
          elem match {
            case s: Val.Str => sb.append(s.str)
            case v          => sb.append(Materializer.stringify(v)(ev))
          }
          i += 1
        }
        Val.Str(pos, sb.toString)

      // Pattern: (acc + SEP) + elem  →  acc + SEP + elem
      case (inner: Expr.BinaryOp, r: Expr.ValidId)
          if inner.op == Expr.BinaryOp.OP_+ && r.nameIdx == base + 1 =>
        (inner.lhs, inner.rhs) match {
          case (l: Expr.ValidId, sep: Val.Str) if l.nameIdx == base =>
            val sepStr = sep.str
            val direct = arr.directBackingArray
            val len = if (direct == null) arr.length else direct.length
            val sb =
              new java.lang.StringBuilder(initStr.length + len * (sepStr.length + 8))
            sb.append(initStr)
            var i = 0
            while (i < len) {
              if (skipSepForFirst == null || i > 0 || initStr != skipSepForFirst)
                sb.append(sepStr)
              val elem = if (direct == null) arr.value(i) else direct(i).value
              elem match {
                case s: Val.Str => sb.append(s.str)
                case v          => sb.append(Materializer.stringify(v)(ev))
              }
              i += 1
            }
            Val.Str(pos, sb.toString)
          case _ => null
        }

      case _ => null
    }
  }

  /**
   * Match conditional separator pattern: `if acc == "" then elem else acc + SEP + elem`
   */
  private def tryStringBuilderFromIfElse(
      ifElse: Expr.IfElse,
      base: Int,
      arr: Val.Arr,
      initStr: String,
      ev: EvalScope,
      pos: Position
  ): Val = {
    ifElse.cond match {
      case eq: Expr.BinaryOp if eq.op == Expr.BinaryOp.OP_== =>
        (eq.lhs, eq.rhs) match {
          // if acc == "" then elem else <body>
          case (accId: Expr.ValidId, emptyStr: Val.Str)
              if accId.nameIdx == base && emptyStr.str.isEmpty =>
            ifElse.`then` match {
              case elemId: Expr.ValidId if elemId.nameIdx == base + 1 =>
                ifElse.`else` match {
                  case sepBody: Expr.BinaryOp if sepBody.op == Expr.BinaryOp.OP_+ =>
                    tryStringBuilderFromBinaryOp(sepBody, base, arr, initStr, "", ev, pos)
                  case _ => null
                }
              case _ => null
            }
          // if "" == acc then elem else <body>
          case (emptyStr: Val.Str, accId: Expr.ValidId)
              if accId.nameIdx == base && emptyStr.str.isEmpty =>
            ifElse.`then` match {
              case elemId: Expr.ValidId if elemId.nameIdx == base + 1 =>
                ifElse.`else` match {
                  case sepBody: Expr.BinaryOp if sepBody.op == Expr.BinaryOp.OP_+ =>
                    tryStringBuilderFromBinaryOp(sepBody, base, arr, initStr, "", ev, pos)
                  case _ => null
                }
              case _ => null
            }
          case _ => null
        }
      case _ => null
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-foldl std.foldl(func, arr, init)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Classic foldl function. Calls the function for each array element, passing the result from the
   * previous call (or init for the first call), and the array element. Traverses the array from
   * left to right.
   *
   * For example: foldl(f, [1,2,3], 0) is equivalent to f(f(f(0, 1), 2), 3).
   */
  private object Foldl extends Val.Builtin3("foldl", "func", "arr", "init") {
    def evalRhs(_func: Eval, arr: Eval, init: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      arr.value match {
        case arr: Val.Arr =>
          val initVal = init.value
          // Fast path: string concatenation via StringBuilder O(n) instead of O(n²)
          initVal match {
            case s: Val.Str =>
              val result = tryStringBuilderFoldl(func, arr, s.str, ev, pos)
              if (result != null) return result
            case _ =>
          }
          var current = initVal
          val direct = arr.directBackingArray
          val len = if (direct == null) arr.length else direct.length
          val noOff = pos.noOffset
          var i = 0
          while (i < len) {
            val c = current
            val elem = if (direct == null) arr.eval(i) else direct(i)
            current = func.apply2(c, elem, noOff)(ev, TailstrictModeDisabled)
            i += 1
          }
          current

        case s: Val.Str =>
          var current = init.value
          val str = s.str
          val noOff = pos.noOffset
          var i = 0
          while (i < str.length) {
            val c = current
            val codePoint = str.codePointAt(i)
            current = func.apply2(c, Val.Str(pos, Character.toString(codePoint)), noOff)(
              ev,
              TailstrictModeDisabled
            )
            i += Character.charCount(codePoint)
          }
          current

        case arr => Error.fail("Cannot call foldl on " + arr.prettyName)
      }

    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-foldr std.foldr(func, arr, init)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Classic foldr function. Calls the function for each array element, passing the array element
   * and the result from the previous call (or init for the first call). Traverses the array from
   * right to left.
   *
   * For example: foldr(f, [1,2,3], 0) is equivalent to f(1, f(2, f(3, 0))).
   */
  private object Foldr extends Val.Builtin3("foldr", "func", "arr", "init") {
    def evalRhs(_func: Eval, arr: Eval, init: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      arr.value match {
        case arr: Val.Arr =>
          var current = init.value
          val direct = arr.directBackingArray
          var i = (if (direct == null) arr.length else direct.length) - 1
          while (i >= 0) {
            val c = current
            val elem = if (direct == null) arr.eval(i) else direct(i)
            current = func.apply2(elem, c, pos.noOffset)(ev, TailstrictModeDisabled)
            i -= 1
          }
          current
        case s: Val.Str =>
          var current = init.value
          val str = s.str
          var i = str.length
          while (i > 0) {
            val codePoint = str.codePointBefore(i)
            i -= Character.charCount(codePoint)
            val c = current
            current = func.apply2(Val.Str(pos, Character.toString(codePoint)), c, pos.noOffset)(
              ev,
              TailstrictModeDisabled
            )
          }
          current
        case arr => Error.fail("Cannot call foldr on " + arr.prettyName)
      }
    }
  }

  private def stringChars(pos: Position, str: String): Val.Arr = {
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
    builtin(MinArray),
    builtin(MaxArray),
    builtin(All),
    builtin(Any),
    builtin(Count),
    builtin(Filter),
    builtin(Map_),
    builtin(MapWithIndex),
    builtin(Find),
    builtin(FlattenArrays),
    builtin(FlattenDeepArrays),
    builtin(Reverse),
    builtin(Member),
    builtin(Range),
    builtin(Foldl),
    builtin(Foldr),
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-flatMap std.flatMap(func, arr)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * Apply the given function to every element of arr to form a new array then flatten the result.
     * The argument arr must be an array or a string. If arr is an array, function func must return
     * an array. If arr is a string, function func must return an string.
     *
     * The std.flatMap function can be thought of as a generalized std.map, with each element mapped
     * to 0, 1 or more elements.
     */
    builtin("flatMap", "func", "arr") { (pos, ev, func: Val.Func, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val noOff = pos.noOffset
          // Two-pass: first collect sub-arrays and count total size,
          // then copy into a single pre-sized array
          val subArrays = new Array[Val.Arr](a.length)
          var totalLen = 0
          var i = 0
          while (i < a.length) {
            val fres = func.apply1(a.eval(i), noOff)(ev, TailstrictModeDisabled)
            fres match {
              case va: Val.Arr =>
                subArrays(i) = va
                totalLen += va.length
              case unknown =>
                Error.fail(
                  "std.flatMap on arrays, provided function must return an array, got " + unknown.prettyName
                )
            }
            i += 1
          }
          val result = new Array[Eval](totalLen)
          var offset = 0
          i = 0
          while (i < subArrays.length) {
            val sub = subArrays(i)
            if (sub != null) {
              var j = 0
              while (j < sub.length) {
                result(offset + j) = sub.eval(j)
                j += 1
              }
              offset += sub.length
            }
            i += 1
          }
          Val.Arr(pos, result)

        case s: Val.Str =>
          val builder = new java.lang.StringBuilder()
          var i = 0
          while (i < s.str.length) {
            val codePoint = s.str.codePointAt(i)
            val codepointStr = Character.toString(codePoint)
            val fres =
              func.apply1(Val.Str(pos, codepointStr), pos.noOffset)(ev, TailstrictModeDisabled)
            // Official std.flatMap on strings concatenates callback string results only.
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.str
                case x             =>
                  Error.fail(
                    "std.flatMap on strings, provided function must return a string, got " + fres
                      .asInstanceOf[Val]
                      .value
                      .prettyName
                  )
              }
            )
            i += Character.charCount(codePoint)
          }
          Val.Str(pos, builder.toString)
        case unknown =>
          Error.fail("std.flatMap second param must be array / string, got " + unknown.prettyName)
      }
      res
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-filterMap std.filterMap(filter_func, map_func, arr)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * It first filters, then maps the given array, using the two functions provided.
     */
    builtin("filterMap", "filter_func", "map_func", "arr") {
      (pos, ev, filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
        val noOff = pos.noOffset
        // Equivalent to std.map(map_func, std.filter(filter_func, arr)): callback args stay lazy.
        val b = new mutable.ArrayBuilder.ofRef[Eval]
        b.sizeHint(arr.length) // Worst case: all elements pass filter
        var i = 0
        while (i < arr.length) {
          val elem = arr.eval(i)
          if (filter_func.apply1(elem, noOff)(ev, TailstrictModeDisabled).asBoolean) {
            b += elem
          }
          i += 1
        }
        Val.Arr.mapped(pos, Val.Arr(pos, b.result()), map_func, noOff, ev)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-repeat std.repeat(what, count)]].
     *
     * Since: 0.15.0. Group: Arrays.
     *
     * Repeats an array or a string what a number of times specified by an integer count.
     */
    builtin("repeat", "what", "count") { (pos, ev, what: Val, count: Int) =>
      if (count < 0) {
        Error.fail("makeArray requires size >= 0, got " + count)
      }
      val res: Val = what match {
        case Val.Str(_, str) =>
          Val.Str(pos, Platform.repeatString(str, count))
        case a: Val.Arr =>
          if (a.length.toLong * count.toLong > Int.MaxValue)
            Error.fail("array too large", pos)(ev)
          Val.Arr.repeated(pos, a, count)
        case x => Error.fail("std.repeat first argument must be an array or a string")
      }
      res
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-makeArray std.makeArray(sz, func)]].
     *
     * Since: 0.10.0. Group: Arrays.
     *
     * Create a new array of sz elements by calling func(i) to initialize each element. Func is
     * expected to be a function that takes a single parameter, the index of the element it should
     * initialize.
     */
    builtin("makeArray", "sz", "func") { (pos, ev, size: Val, func: Val.Func) =>
      val sz = size.cast[Val.Num].asPositiveInt
      val body = func.bodyExpr
      if (func.params.names.length == 1 && body != null && body.isInstanceOf[Val.Literal]) {
        // Function body is a constant (e.g. `function(_) 'x'`).
        // Keep the eager shared-value array: it is smaller and faster than a lazy view here.
        val a = new Array[Eval](sz)
        java.util.Arrays.fill(a.asInstanceOf[Array[AnyRef]], body.asInstanceOf[Val])
        Val.Arr(pos, a)
      } else {
        Val.Arr.makeArray(pos, sz, func, pos, pos.noOffset, ev)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-contains std.contains(arr, elem)]].
     *
     * Since: 0.21.0. Group: Arrays.
     *
     * Return true if given elem is present in arr, false otherwise.
     */
    builtin("contains", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      var i = 0
      var found = false
      while (i < arr.length && !found) {
        if (ev.equal(arr.value(i), elem)) found = true
        i += 1
      }
      found
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-remove std.remove(arr, elem)]].
     *
     * Since: 0.21.0. Group: Arrays.
     *
     * Remove first occurrence of elem from arr.
     */
    builtin("remove", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      var idx = -1
      var i = 0
      while (i < arr.length && idx == -1) {
        if (ev.equal(arr.value(i), elem)) idx = i
        i += 1
      }
      if (idx == -1) {
        arr
      } else {
        removeAtView(arr, idx)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-removeAt std.removeAt(arr, idx)]].
     *
     * Since: 0.21.0. Group: Arrays.
     *
     * Remove element at idx index from arr.
     */
    builtin("removeAt", "arr", "idx") { (_, _, arr: Val.Arr, idx: Val) =>
      val removeIdx = idx match {
        case n: Val.Num =>
          val d = n.asDouble
          if (d.isWhole && d >= 0 && d < arr.length) d.toInt else -1
        case _ => -1
      }
      if (removeIdx == -1) arr
      else removeAtView(arr, removeIdx)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-sum std.sum(arr)]].
     *
     * Since: 0.20.0. Group: Arrays.
     *
     * Return sum of all element in arr.
     */
    builtin("sum", "arr") { (_, _, arr: Val.Arr) =>
      arr match {
        case r: Val.RangeArr => rangeSum(r)
        case b: Val.ByteArr  => byteArraySum(b)
        case _               =>
          var sum = 0.0
          var i = 0
          while (i < arr.length) {
            arr.value(i) match {
              case n: Val.Num => sum += n.asDouble
              case x          => Error.fail("std.sum expected number, got " + x.prettyName)
            }
            i += 1
          }
          sum
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-avg std.avg(arr)]].
     *
     * Since: 0.21.0. Group: Arrays.
     *
     * Return average of all element in arr.
     */
    builtin("avg", "arr") { (_, _, arr: Val.Arr) =>
      if (arr.length == 0) {
        Error.fail("Cannot calculate average of an empty array")
      }
      arr match {
        case r: Val.RangeArr =>
          (r.value(0).asDouble + r.value(r.length - 1).asDouble) / 2.0
        case b: Val.ByteArr =>
          byteArraySum(b) / b.length
        case _ =>
          var sum = 0.0
          var i = 0
          while (i < arr.length) {
            arr.value(i) match {
              case n: Val.Num => sum += n.asDouble
              case x          => Error.fail("std.avg expected number, got " + x.prettyName)
            }
            i += 1
          }
          sum / arr.length
      }
    }
  )
}
