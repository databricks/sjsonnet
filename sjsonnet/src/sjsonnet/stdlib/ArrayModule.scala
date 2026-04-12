package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable

object ArrayModule extends AbstractFunctionModule {
  def name = "array"

  private object MinArray
      extends Val.Builtin(
        "minArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, Val.False(dummyPos), Val.False(dummyPos))
      ) {
    override def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).value.asArr
      val keyF = args(1).value
      val onEmpty = args(2)
      if (arr.length == 0) {
        if (onEmpty.value.isInstanceOf[Val.False]) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty.value
        }
      } else if (keyF.isInstanceOf[Val.False]) {
        arr.asStrictArray.min(ev)
      } else {
        val strict = arr.asStrictArray
        val func = keyF.asInstanceOf[Val.Func]
        var bestIdx = 0
        var bestVal = func.apply1(strict(0), pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
        var i = 1
        while (i < strict.length) {
          val v = func.apply1(strict(i), pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
          if (ev.compare(v, bestVal) < 0) {
            bestVal = v
            bestIdx = i
          }
          i += 1
        }
        strict(bestIdx)
      }
    }
  }

  private object MaxArray
      extends Val.Builtin(
        "maxArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, Val.False(dummyPos), Val.False(dummyPos))
      ) {
    override def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).value.asArr
      val keyF = args(1).value
      val onEmpty = args(2)
      if (arr.length == 0) {
        if (onEmpty.value.isInstanceOf[Val.False]) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty.value
        }
      } else if (keyF.isInstanceOf[Val.False]) {
        arr.asStrictArray.max(ev)
      } else {
        val strict = arr.asStrictArray
        val func = keyF.asInstanceOf[Val.Func]
        var bestIdx = 0
        var bestVal = func.apply1(strict(0), pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
        var i = 1
        while (i < strict.length) {
          val v = func.apply1(strict(i), pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
          if (ev.compare(v, bestVal) > 0) {
            bestVal = v
            bestIdx = i
          }
          i += 1
        }
        strict(bestIdx)
      }
    }
  }

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

  private object Count extends Val.Builtin2("count", "arr", "x") {
    def evalRhs(arr: Eval, x: Eval, ev: EvalScope, pos: Position): Val = {
      var count = 0
      arr.value.asArr.foreach(v => if (ev.equal(v.value, x.value)) count += 1)
      Val.cachedNum(pos, count.toDouble)
    }
  }

  private object Filter extends Val.Builtin2("filter", "func", "arr") {
    def evalRhs(_func: Eval, arr: Eval, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.value.asArr.asLazyArray
      var i = 0
      val func = _func.value.asFunc
      if (func.isInstanceOf[Val.Builtin] || func.params.names.length != 1) {
        while (i < a.length) {
          if (!func.apply1(a(i), p)(ev, TailstrictModeDisabled).asBoolean) {
            var b = new Array[Eval](a.length - 1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i + 1
            while (j < a.length) {
              if (func.apply1(a(j), p)(ev, TailstrictModeDisabled).asBoolean) {
                b(i) = a(j)
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
        while (i < a.length) {
          newScope.bindings(scopeIdx) = a(i)
          if (!func.evalRhsResolved(newScope, ev, funDefFileScope, p).asBoolean) {
            var b = new Array[Eval](a.length - 1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i + 1
            while (j < a.length) {
              newScope.bindings(scopeIdx) = a(j)
              if (func.evalRhsResolved(newScope, ev, funDefFileScope, p).asBoolean) {
                b(i) = a(j)
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
      Val.Arr(pos, a)
    }
  }

  private object Map_ extends Val.Builtin2("map", "func", "arr") {
    def evalRhs(_func: Eval, arr: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      val arg = arr.value
      arg match {
        case Val.Str(_, str) => evalStr(func, str, ev, pos.noOffset)
        case _               => evalArr(func, arg.asArr.asLazyArray, ev, pos)
      }
    }

    private def evalArr(
        _func: Val.Func,
        arg: Array[Eval],
        ev: EvalScope,
        pos: Position): Val.Arr = {
      val noOff = pos.noOffset
      // Pre-sized array with while-loop avoids .map closure overhead
      val result = new Array[Eval](arg.length)
      var i = 0
      while (i < arg.length) {
        result(i) = new LazyApply1(_func, arg(i), noOff, ev)
        i += 1
      }
      Val.Arr(pos, result)
    }

    private def evalStr(_func: Val.Func, arg: String, ev: EvalScope, pos: Position): Val.Arr = {
      evalArr(_func, stringChars(pos, arg).asLazyArray, ev, pos)
    }
  }

  private object MapWithIndex extends Val.Builtin2("mapWithIndex", "func", "arr") {
    def evalRhs(_func: Eval, _arr: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      val arr = _arr.value.asArr.asLazyArray
      val a = new Array[Eval](arr.length)
      val noOff = pos.noOffset
      var i = 0
      while (i < a.length) {
        val x = arr(i)
        val idx = Val.cachedNum(pos, i)
        a(i) = new LazyApply2(func, idx, x, noOff, ev)
        i += 1
      }
      Val.Arr(pos, a)
    }
  }

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

  private object FlattenArrays extends Val.Builtin1("flattenArrays", "arrs") {
    def evalRhs(arrs: Eval, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuilder.ofRef[Eval]
      val arr = arrs.value.asArr
      out.sizeHint(arr.length * 4) // Rough size hint
      for (x <- arr) {
        x.value match {
          case Val.Null(_) => // do nothing
          case v: Val.Arr  => out ++= v.asLazyArray
          case x           => Error.fail("Cannot call flattenArrays on " + x)
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  private object FlattenDeepArrays extends Val.Builtin1("flattenDeepArray", "value") {
    def evalRhs(value: Eval, ev: EvalScope, pos: Position): Val = {
      val lazyArray = value.value.asArr.asLazyArray
      val out = new mutable.ArrayBuilder.ofRef[Eval]
      out.sizeHint(lazyArray.length)
      val q = new java.util.ArrayDeque[Eval](lazyArray.length)
      lazyArray.foreach(q.add)
      while (!q.isEmpty) {
        q.removeFirst().value match {
          case v: Val.Arr =>
            val inner = v.asLazyArray
            var i = inner.length - 1
            while (i >= 0) { q.push(inner(i)); i -= 1 }
          case x => out += x
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  private object Reverse extends Val.Builtin1("reverse", "arrs") {
    def evalRhs(arrs: Eval, ev: EvalScope, pos: Position): Val = {
      // Zero-copy reverse: create a reversed view sharing the same backing array.
      // This avoids allocating and copying a new array, reducing GC pressure.
      arrs.value.asArr.reversed(pos)
    }
  }

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
            val la = a.asLazyArray
            var i = 0
            var found = false
            while (i < la.length && !found) {
              if (ev.equal(la(i).value, x.value)) found = true
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
   * Detect pattern: function(acc, elem) acc + elem with string init → use StringBuilder O(n).
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
    body match {
      case e: Expr.BinaryOp if e.op == Expr.BinaryOp.OP_+ =>
        (e.lhs, e.rhs) match {
          case (l: Expr.ValidId, r: Expr.ValidId) =>
            val base = func.defSiteValScope.bindings.length
            if (l.nameIdx == base && r.nameIdx == base + 1) {
              val sb = new java.lang.StringBuilder(initStr)
              val lazyArr = arr.asLazyArray
              var i = 0
              while (i < lazyArr.length) {
                lazyArr(i).value match {
                  case s: Val.Str => sb.append(s.str)
                  case v          => sb.append(Materializer.stringify(v)(ev))
                }
                i += 1
              }
              return Val.Str(pos, sb.toString)
            }
            null
          case _ => null
        }
      case _ => null
    }
  }

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
          val lazyArr = arr.asLazyArray
          val noOff = pos.noOffset
          var i = 0
          while (i < lazyArr.length) {
            val c = current
            current = func.apply2(c, lazyArr(i), noOff)(ev, TailstrictModeDisabled)
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

  private object Foldr extends Val.Builtin3("foldr", "func", "arr", "init") {
    def evalRhs(_func: Eval, arr: Eval, init: Eval, ev: EvalScope, pos: Position): Val = {
      val func = _func.value.asFunc
      arr.value match {
        case arr: Val.Arr =>
          var current = init.value
          val lazyArr = arr.asLazyArray
          var i = lazyArr.length - 1
          while (i >= 0) {
            val c = current
            current = func.apply2(lazyArr(i), c, pos.noOffset)(ev, TailstrictModeDisabled)
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
    builtin("flatMap", "func", "arr") { (pos, ev, func: Val.Func, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val src = a.asLazyArray
          val noOff = pos.noOffset
          // Two-pass: first collect sub-arrays and count total size,
          // then copy into a single pre-sized array
          val subArrays = new Array[Array[Eval]](src.length)
          var totalLen = 0
          var i = 0
          while (i < src.length) {
            val fres = func.apply1(src(i), noOff)(ev, TailstrictModeDisabled)
            fres match {
              case va: Val.Arr =>
                val sub = va.asLazyArray
                subArrays(i) = sub
                totalLen += sub.length
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
              System.arraycopy(sub, 0, result, offset, sub.length)
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
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.str
                case _: Val.Null   => ""
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
    builtin("filterMap", "filter_func", "map_func", "arr") {
      (pos, ev, filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
        val noOff = pos.noOffset
        val src = arr.asLazyArray
        // While-loop with ArrayBuilder avoids flatMap + Option boxing
        val b = new mutable.ArrayBuilder.ofRef[Eval]
        b.sizeHint(src.length) // Worst case: all elements pass filter
        var i = 0
        while (i < src.length) {
          val elem = src(i)
          elem.value
          if (filter_func.apply1(elem, noOff)(ev, TailstrictModeDisabled).asBoolean) {
            b += new LazyApply1(map_func, elem, noOff, ev)
          }
          i += 1
        }
        Val.Arr(pos, b.result())
    },
    builtin("repeat", "what", "count") { (pos, ev, what: Val, count: Int) =>
      val res: Val = what match {
        case Val.Str(_, str) =>
          val builder = new StringBuilder(str.length * count)
          var i = 0
          while (i < count) {
            builder.append(str)
            i += 1
          }
          Val.Str(pos, builder.toString())
        case a: Val.Arr =>
          val lazyArray = a.asLazyArray
          val elemLen = lazyArray.length
          val result = new Array[Eval](elemLen * count)
          var i = 0
          while (i < count) {
            System.arraycopy(lazyArray, 0, result, i * elemLen, elemLen)
            i += 1
          }
          Val.Arr(pos, result)
        case x => Error.fail("std.repeat first argument must be an array or a string")
      }
      res
    },
    builtin("makeArray", "sz", "func") { (pos, ev, size: Val, func: Val.Func) =>
      Val.Arr(
        pos, {
          val sz = size.cast[Val.Num].asPositiveInt
          val a = new Array[Eval](sz)
          val body = func.bodyExpr
          if (func.params.names.length == 1 && body != null && body.isInstanceOf[Val.Literal]) {
            // Function body is a constant (e.g. `function(_) 'x'`).
            // Skip lazy thunk + Val.Num(index) allocation per element.
            val constVal = body.asInstanceOf[Val]
            java.util.Arrays.fill(a.asInstanceOf[Array[AnyRef]], constVal)
          } else {
            val noOff = pos.noOffset
            var i = 0
            while (i < sz) {
              a(i) = new LazyApply1(func, Val.cachedNum(pos, i), noOff, ev)
              i += 1
            }
          }
          a
        }
      )
    },
    builtin("contains", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      val la = arr.asLazyArray
      var i = 0
      var found = false
      while (i < la.length && !found) {
        if (ev.equal(la(i).value, elem)) found = true
        i += 1
      }
      found
    },
    builtin("remove", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      val la = arr.asLazyArray
      var idx = -1
      var i = 0
      while (i < la.length && idx == -1) {
        if (ev.equal(la(i).value, elem)) idx = i
        i += 1
      }
      if (idx == -1) {
        arr
      } else {
        Val.Arr(
          arr.pos,
          arr.asLazyArray.slice(0, idx) ++ arr.asLazyArray.slice(idx + 1, arr.length)
        )
      }
    },
    builtin("removeAt", "arr", "idx") { (_, _, arr: Val.Arr, idx: Int) =>
      if (!(0 <= idx && idx < arr.length)) {
        Error.fail("index out of bounds: 0 <= " + idx + " < " + arr.length)
      }
      Val.Arr(
        arr.pos,
        arr.asLazyArray.slice(0, idx) ++ arr.asLazyArray.slice(idx + 1, arr.length)
      )
    },
    builtin("sum", "arr") { (_, _, arr: Val.Arr) =>
      if (!arr.forall(_.isInstanceOf[Val.Num])) {
        Error.fail("Argument must be an array of numbers")
      }
      arr.asLazyArray.map(_.value.asDouble).sum
    },
    builtin("avg", "arr") { (_, _, arr: Val.Arr) =>
      if (!arr.forall(_.isInstanceOf[Val.Num])) {
        Error.fail("Argument must be an array of numbers")
      }
      if (arr.length == 0) {
        Error.fail("Cannot calculate average of an empty array")
      }
      arr.asLazyArray.map(_.value.asDouble).sum / arr.length
    }
  )
}
