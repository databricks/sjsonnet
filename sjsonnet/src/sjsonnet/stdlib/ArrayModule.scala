package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import scala.collection.mutable

class ArrayModule extends AbstractFunctionModule {
  def name = "array"

  private val dummyPos: Position = new Position(null, 0)

  private object MinArray
      extends Val.Builtin(
        "minArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, Val.False(dummyPos), Val.False(dummyPos))
      ) {
    override def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).force.asArr
      val keyF = args(1).force
      val onEmpty = args(2)
      if (arr.length == 0) {
        if (onEmpty.force.isInstanceOf[Val.False]) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty.force
        }
      } else if (keyF.isInstanceOf[Val.False]) {
        arr.asStrictArray.min(ev)
      } else {
        val minTuple = arr.asStrictArray
          .map(v =>
            keyF
              .asInstanceOf[Val.Func]
              .apply1(v, pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
          )
          .zipWithIndex
          .min((x: (Val, Int), y: (Val, Int)) => ev.compare(x._1, y._1))
        arr.force(minTuple._2)
      }
    }
  }

  private object MaxArray
      extends Val.Builtin(
        "maxArray",
        Array("arr", "keyF", "onEmpty"),
        Array(null, Val.False(dummyPos), Val.False(dummyPos))
      ) {
    override def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val = {
      val arr = args(0).force.asArr
      val keyF = args(1).force
      val onEmpty = args(2)
      if (arr.length == 0) {
        if (onEmpty.force.isInstanceOf[Val.False]) {
          Error.fail("Expected at least one element in array. Got none")
        } else {
          onEmpty.force
        }
      } else if (keyF.isInstanceOf[Val.False]) {
        arr.asStrictArray.max(ev)
      } else {
        val maxTuple = arr.asStrictArray
          .map(v =>
            keyF
              .asInstanceOf[Val.Func]
              .apply1(v, pos.fileScope.noOffsetPos)(ev, TailstrictModeDisabled)
          )
          .zipWithIndex
          .max((x: (Val, Int), y: (Val, Int)) => ev.compare(x._1, y._1))
        arr.force(maxTuple._2)
      }
    }
  }

  private object All extends Val.Builtin1("all", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr.force.asArr.forall(v => v.asBoolean))
    }
  }

  private object Any extends Val.Builtin1("any", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr.force.asArr.iterator.exists(v => v.asBoolean))
    }
  }

  private object Count extends Val.Builtin2("count", "arr", "x") {
    def evalRhs(arr: Lazy, x: Lazy, ev: EvalScope, pos: Position): Val = {
      var count = 0
      arr.force.asArr.foreach(v => if (ev.equal(v.force, x.force)) count += 1)
      Val.Num(pos, count)
    }
  }

  private object Filter extends Val.Builtin2("filter", "func", "arr") {
    def evalRhs(_func: Lazy, arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.force.asArr.asLazyArray
      var i = 0
      val func = _func.force.asFunc
      if (func.isInstanceOf[Val.Builtin] || func.params.names.length != 1) {
        while (i < a.length) {
          if (!func.apply1(a(i), p)(ev, TailstrictModeDisabled).isInstanceOf[Val.True]) {
            var b = new Array[Lazy](a.length - 1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i + 1
            while (j < a.length) {
              if (func.apply1(a(j), p)(ev, TailstrictModeDisabled).isInstanceOf[Val.True]) {
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
          if (!func.evalRhs(newScope, ev, funDefFileScope, p).isInstanceOf[Val.True]) {
            var b = new Array[Lazy](a.length - 1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i + 1
            while (j < a.length) {
              newScope.bindings(scopeIdx) = a(j)
              if (func.evalRhs(newScope, ev, funDefFileScope, p).isInstanceOf[Val.True]) {
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
    def evalRhs(_func: Lazy, arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      val arg = arr.force
      arg match {
        case Val.Str(_, str) => evalStr(func, str, ev, pos.noOffset)
        case _               => evalArr(func, arg.asArr.asLazyArray, ev, pos)
      }
    }

    private def evalArr(
        _func: Val.Func,
        arg: Array[Lazy],
        ev: EvalScope,
        pos: Position): Val.Arr = {
      Val.Arr(
        pos,
        arg.map(v => (() => _func.apply1(v, pos.noOffset)(ev, TailstrictModeDisabled)): Lazy)
      )
    }

    private def evalStr(_func: Val.Func, arg: String, ev: EvalScope, pos: Position): Val.Arr = {
      evalArr(_func, stringChars(pos, arg).asLazyArray, ev, pos)
    }
  }

  private object MapWithIndex extends Val.Builtin2("mapWithIndex", "func", "arr") {
    def evalRhs(_func: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      val arr = _arr.force.asArr.asLazyArray
      val a = new Array[Lazy](arr.length)
      var i = 0
      while (i < a.length) {
        val x = arr(i)
        val idx = Val.Num(pos, i)
        a(i) = () => func.apply2(idx, x, pos.noOffset)(ev, TailstrictModeDisabled)
        i += 1
      }
      Val.Arr(pos, a)
    }
  }

  private object Find extends Val.Builtin2("find", "value", "arr") {
    def evalRhs(value: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val arr = _arr.force.asArr
      val b = new mutable.ArrayBuilder.ofRef[Lazy]
      var i = 0
      while (i < arr.length) {
        if (ev.equal(arr.force(i), value.force)) {
          val finalI = i
          b.+=(Val.Num(pos, finalI))
        }
        i += 1
      }
      Val.Arr(pos, b.result())
    }
  }

  private object FlattenArrays extends Val.Builtin1("flattenArrays", "arrs") {
    def evalRhs(arrs: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuilder.ofRef[Lazy]
      for (x <- arrs.force.asArr) {
        x.force match {
          case Val.Null(_) => // do nothing
          case v: Val.Arr  => out ++= v.asLazyArray
          case x           => Error.fail("Cannot call flattenArrays on " + x)
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  private object FlattenDeepArrays extends Val.Builtin1("flattenDeepArray", "value") {
    def evalRhs(value: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuilder.ofRef[Lazy]
      val q = new java.util.ArrayDeque[Lazy]()
      value.force.asArr.asLazyArray.foreach(q.add)
      while (!q.isEmpty) {
        q.removeFirst().force match {
          case v: Val.Arr => v.asLazyArray.reverseIterator.foreach(q.push)
          case x          => out += x
        }
      }
      Val.Arr(pos, out.result())
    }
  }

  private object Reverse extends Val.Builtin1("reverse", "arrs") {
    def evalRhs(arrs: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Arr(pos, arrs.force.asArr.asLazyArray.reverse)
    }
  }

  private object Member extends Val.Builtin2("member", "arr", "x") {
    def evalRhs(arr: Lazy, x: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(
        pos,
        arr.force match {
          case str: Val.Str =>
            val secondArg = x.force match {
              case Val.Str(_, value) => value
              case n                 =>
                Error.fail("std.member second argument must be a string, got " + n.prettyName)
            }
            str.value.contains(secondArg)
          case a: Val.Arr =>
            a.asLazyArray.indexWhere(v => ev.equal(v.force, x.force)) >= 0
          case arr =>
            Error.fail(
              "std.member first argument must be an array or a string, got " + arr.prettyName
            )
        }
      )
    }
  }

  private object Range extends Val.Builtin2("range", "from", "to") {
    def evalRhs(from: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Arr(
        pos,
        (from.force.asInt to to.force.asInt).map(i => Val.Num(pos, i)).toArray
      )
  }

  private object Foldl extends Val.Builtin3("foldl", "func", "arr", "init") {
    def evalRhs(_func: Lazy, arr: Lazy, init: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      arr.force match {
        case arr: Val.Arr =>
          var current = init.force
          for (item <- arr.asLazyArray) {
            val c = current
            current = func.apply2(c, item, pos.noOffset)(ev, TailstrictModeDisabled)
          }
          current

        case s: Val.Str =>
          var current = init.force
          for (char <- s.value) {
            val c = current
            current = func.apply2(c, Val.Str(pos, new String(Array(char))), pos.noOffset)(
              ev,
              TailstrictModeDisabled
            )
          }
          current

        case arr => Error.fail("Cannot call foldl on " + arr.prettyName)
      }

    }
  }

  private object Foldr extends Val.Builtin3("foldr", "func", "arr", "init") {
    def evalRhs(_func: Lazy, arr: Lazy, init: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      arr.force match {
        case arr: Val.Arr =>
          var current = init.force
          for (item <- arr.asLazyArray.reverse) {
            val c = current
            current = func.apply2(item, c, pos.noOffset)(ev, TailstrictModeDisabled)
          }
          current
        case s: Val.Str =>
          var current = init.force
          for (char <- s.value.reverse) {
            val c = current
            current = func.apply2(Val.Str(pos, new String(Array(char))), c, pos.noOffset)(
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
          val arrResults = a.asLazyArray.flatMap { v =>
            {
              val fres = func.apply1(v, pos.noOffset)(ev, TailstrictModeDisabled)
              fres match {
                case va: Val.Arr => va.asLazyArray
                case unknown     => Error.fail("flatMap func must return an array, not " + unknown)
              }
            }
          }
          Val.Arr(pos, arrResults)

        case s: Val.Str =>
          val builder = new java.lang.StringBuilder()
          var i = 0
          while (i < s.value.length) {
            val codePoint = s.value.codePointAt(i)
            val codepointStr = Character.toString(codePoint)
            val fres =
              func.apply1(Val.Str(pos, codepointStr), pos.noOffset)(ev, TailstrictModeDisabled)
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.value
                case _: Val.Null   => ""
                case x             =>
                  Error.fail(
                    "flatMap func must return string, got " + fres
                      .asInstanceOf[Val]
                      .force
                      .prettyName
                  )
              }
            )
            i += Character.charCount(codePoint)
          }
          Val.Str(pos, builder.toString)
        case _ => Error.fail("Argument must be either array or string")
      }
      res
    },
    builtin("filterMap", "filter_func", "map_func", "arr") {
      (pos, ev, filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
        Val.Arr(
          pos,
          arr.asLazyArray.flatMap { i =>
            i.force
            if (
              !filter_func
                .apply1(i, pos.noOffset)(ev, TailstrictModeDisabled)
                .isInstanceOf[Val.True]
            ) None
            else Some[Lazy](() => map_func.apply1(i, pos.noOffset)(ev, TailstrictModeDisabled))
          }
        )
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
          val out = new mutable.ArrayBuffer[Lazy](lazyArray.length * count)
          var i = 0
          while (i < count) {
            out.appendAll(lazyArray)
            i += 1
          }
          Val.Arr(pos, out.toArray)
        case x => Error.fail("std.repeat first argument must be an array or a string")
      }
      res
    },
    builtin("makeArray", "sz", "func") { (pos, ev, size: Val, func: Val.Func) =>
      Val.Arr(
        pos, {
          val sz = size.cast[Val.Num].asPositiveInt
          val a = new Array[Lazy](sz)
          var i = 0
          while (i < sz) {
            val forcedI = i
            a(i) =
              () => func.apply1(Val.Num(pos, forcedI), pos.noOffset)(ev, TailstrictModeDisabled)
            i += 1
          }
          a
        }
      )
    },
    builtin("contains", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      arr.asLazyArray.indexWhere(s => ev.equal(s.force, elem)) != -1
    },
    builtin("remove", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      val idx = arr.asLazyArray.indexWhere(s => ev.equal(s.force, elem))
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
      arr.asLazyArray.map(_.force.asDouble).sum
    },
    builtin("avg", "arr") { (_, _, arr: Val.Arr) =>
      if (!arr.forall(_.isInstanceOf[Val.Num])) {
        Error.fail("Argument must be an array of numbers")
      }
      if (arr.length == 0) {
        Error.fail("Cannot calculate average of an empty array")
      }
      arr.asLazyArray.map(_.force.asDouble).sum / arr.length
    }
  )
}
