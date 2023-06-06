package sjsonnet

import java.io.StringWriter
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util
import java.util.regex.Pattern

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.BinaryOp
import sjsonnet.ArrayOps._

import scala.collection.mutable
import scala.util.matching.Regex

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
class Std {
  private val dummyPos: Position = new Position(null, 0)
  private val emptyLazyArray = new Array[Lazy](0)

  private object AssertEqual extends Val.Builtin2("a", "b") {
    def evalRhs(v1: Val, v2: Val, ev: EvalScope, pos: Position): Val = {
      val x1 = Materializer(v1)(ev)
      val x2 = Materializer(v2)(ev)
      if (x1 == x2) Val.True(pos)
      else Error.fail("assertEqual failed: " + x1 + " != " + x2)
    }
  }

  private object ToString extends Val.Builtin1("a") {
    def evalRhs(v1: Val, ev: EvalScope, pos: Position): Val = Val.Str(pos, v1 match {
      case Val.Str(_, s) => s
      case v => Materializer.stringify(v)(ev)
    })
  }

  private object Length extends Val.Builtin1("x") {
    def evalRhs(x: Val, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, x match {
        case Val.Str(_, s) => s.length
        case a: Val.Arr => a.length
        case o: Val.Obj => o.visibleKeyNames.length
        case o: Val.Func => o.params.names.length
        case x => Error.fail("Cannot get length of " + x.prettyName)
      })
    override def specialize(args: Array[Expr]) = args match {
      case Array(Expr.ApplyBuiltin2(_, Filter, f, a)) => (CountF, Array(f, a))
      case _ => null
    }
  }

  private object CountF extends Val.Builtin2("func", "arr") {
    def evalRhs(_func: Val, arr: Val, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.asArr.asLazyArray
      var i = 0
      val func = _func.asFunc
      var res = 0
      if(func.isInstanceOf[Val.Builtin] || func.params.names.length != 1) {
        while(i < a.length) {
          if(func.apply1(a(i), p)(ev).isInstanceOf[Val.True]) res += 1
          i += 1
        }
      } else {
        // Single-param non-builtin can benefit from scope reuse: We compute a strict boolean from
        // the function, there's no risk of the scope leaking (and being invalid at a later point)
        val funDefFileScope: FileScope = func.pos match { case null => p.fileScope case p => p.fileScope }
        val newScope: ValScope = func.defSiteValScope.extendBy(1)
        val scopeIdx = newScope.length-1
        while(i < a.length) {
          newScope.bindings(scopeIdx) = a(i)
          if(func.evalRhs(newScope, ev, funDefFileScope, p).isInstanceOf[Val.True]) res += 1
          i += 1
        }
      }
      new Val.Num(pos, res)
    }
  }

  private object Codepoint extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, str.asString.charAt(0).toInt)
  }

  private object ObjectHas extends Val.Builtin2("o", "f") {
    def evalRhs(o: Val, f: Val, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, o.asObj.containsVisibleKey(f.asString))
    override def specialize(args: Array[Expr]) = args match {
      case Array(o, s: Val.Str) => (new SpecF(s.value), Array(o))
      case _ => null
    }
    private class SpecF(f: String) extends Val.Builtin1("o") {
      def evalRhs(o: Val, ev: EvalScope, pos: Position): Val =
        Val.bool(pos, o.asObj.containsVisibleKey(f))
    }
  }

  private object ObjectHasAll extends Val.Builtin2("o", "f") {
    def evalRhs(o: Val, f: Val, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, o.asObj.containsKey(f.asString))
    override def specialize(args: Array[Expr]) = args match {
      case Array(o, s: Val.Str) => (new SpecF(s.value), Array(o))
      case _ => null
    }
    class SpecF(f: String) extends Val.Builtin1("o") {
      def evalRhs(o: Val, ev: EvalScope, pos: Position): Val =
        Val.bool(pos, o.asObj.containsKey(f))
    }
  }

  private object ObjectFields extends Val.Builtin1("o") {
    def evalRhs(o: Val, ev: EvalScope, pos: Position): Val = {
      val keys = getVisibleKeys(ev, o.asObj)
      new Val.Arr(pos, keys.map(k => Val.Str(pos, k)))
    }
  }

  private object ObjectFieldsAll extends Val.Builtin1("o") {
    def evalRhs(o: Val, ev: EvalScope, pos: Position): Val = {
      val keys = getAllKeys(ev, o.asObj)
      new Val.Arr(pos, keys.map(k => Val.Str(pos, k)))
    }
  }

  private object Type extends Val.Builtin1("x") {
    def evalRhs(x: Val, ev: EvalScope, pos: Position): Val = Val.Str(pos, x match {
      case _: Val.Bool => "boolean"
      case _: Val.Null => "null"
      case _: Val.Obj => "object"
      case _: Val.Arr => "array"
      case _: Val.Func => "function"
      case _: Val.Num => "number"
      case _: Val.Str => "string"
    })
  }

  private object Format_ extends Val.Builtin2("str", "vals") {
    def evalRhs(str: Val, vals: Val, ev: EvalScope, pos: Position): Val =
      new Val.Str(pos, Format.format(str.asString, vals, pos)(ev))
    override def specialize(args: Array[Expr]) = args match {
      case Array(str, fmt: Val.Str) =>
        try { (new Format.PartialApplyFmt(fmt.value), Array(str)) } catch { case _: Exception => null }
      case _ => null
    }
  }

  private object Foldl extends Val.Builtin3("func", "arr", "init") {
    def evalRhs(_func: Val, arr: Val, init: Val, ev: EvalScope, pos: Position): Val = {
      val func = _func.asFunc
      arr match{
        case arr: Val.Arr =>
          var current = init
          for (item <- arr.asLazyArray) {
            val c = current
            current = func.apply2(c, item, pos.noOffset)(ev)
          }
          current

        case s: Val.Str =>
          var current = init
          for (char <- s.value) {
            val c = current
            current = func.apply2(c, Val.Str(pos, new String(Array(char))), pos.noOffset)(ev)
          }
          current
      }


    }
  }

  private object Foldr extends Val.Builtin3("func", "arr", "init") {
    def evalRhs(_func: Val, arr: Val, init: Val, ev: EvalScope, pos: Position): Val = {
      val func = _func.asFunc
      arr match {
        case arr: Val.Arr =>
          var current = init
          for (item <- arr.asLazyArray.reverse) {
            val c = current
            current = func.apply2(item, c, pos.noOffset)(ev)
          }
          current
        case s: Val.Str =>
          var current = init
          for (char <- s.value) {
            val c = current
            current = func.apply2(Val.Str(pos, new String(Array(char))), c, pos.noOffset)(ev)
          }
          current
      }
    }
  }

  private object IsString extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Str])
  }

  private object IsBoolean extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Bool])
  }

  private object IsNumber extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Num])
  }

  private object IsObject extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Obj])
  }

  private object IsArray extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Arr])
  }

  private object IsFunction extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Func])
  }

  private object Count extends Val.Builtin2("arr", "x") {
    def evalRhs(arr: Val, x: Val, ev: EvalScope, pos: Position): Val = {
      var count = 0
      arr.asArr.foreach(v => if(ev.equal(v, x)) count += 1)
      Val.Num(pos, count)
    }
  }

  private object Filter extends Val.Builtin2("func", "arr") {
    def evalRhs(_func: Val, arr: Val, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.asArr.asLazyArray
      var i = 0
      val func = _func.asFunc
      if(func.isInstanceOf[Val.Builtin] || func.params.names.length != 1) {
        while(i < a.length) {
          if(!func.apply1(a(i), p)(ev).isInstanceOf[Val.True]) {
            var b = new Array[Lazy](a.length-1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i+1
            while(j < a.length) {
              if(func.apply1(a(j), p)(ev).isInstanceOf[Val.True]) {
                b(i) = a(j)
                i += 1
              }
              j += 1
            }
            if(i != b.length) b = util.Arrays.copyOf(b, i)
            return new Val.Arr(pos, b)
          }
          i += 1
        }
      } else {
        // Single-param non-builtin can benefit from scope reuse: We compute a strict boolean from
        // the function, there's no risk of the scope leaking (and being invalid at a later point)
        val funDefFileScope: FileScope = func.pos match { case null => p.fileScope case p => p.fileScope }
        val newScope: ValScope = func.defSiteValScope.extendBy(1)
        val scopeIdx = newScope.length-1
        while(i < a.length) {
          newScope.bindings(scopeIdx) = a(i)
          if(!func.evalRhs(newScope, ev, funDefFileScope, p).isInstanceOf[Val.True]) {
            var b = new Array[Lazy](a.length-1)
            System.arraycopy(a, 0, b, 0, i)
            var j = i+1
            while(j < a.length) {
              newScope.bindings(scopeIdx) = a(j)
              if(func.evalRhs(newScope, ev, funDefFileScope, p).isInstanceOf[Val.True]) {
                b(i) = a(j)
                i += 1
              }
              j += 1
            }
            if(i != b.length) b = util.Arrays.copyOf(b, i)
            return new Val.Arr(pos, b)
          }
          i += 1
        }
      }
      new Val.Arr(pos, a)
      //new Val.Arr(pos, arr.asArr.asLazyArray.filter(v => func.apply1(v, pos.noOffset)(ev).isInstanceOf[Val.True]))
    }
  }

  private object Map_ extends Val.Builtin2("func", "arr") {
    def evalRhs(_func: Val, arr: Val, ev: EvalScope, pos: Position): Val = {
      val func = _func.asFunc
      new Val.Arr(pos, arr.asArr.asLazyArray.map(v => (() => func.apply1(v, pos.noOffset)(ev)): Lazy))
    }
  }

  private object MapWithKey extends Val.Builtin2("func", "obj") {
    def evalRhs(_func: Val, _obj: Val, ev: EvalScope, pos: Position): Val = {
      val func = _func.asFunc
      val obj = _obj.asObj
      val allKeys = obj.allKeyNames
      val m = new util.LinkedHashMap[String, Val.Obj.Member]()
      var i = 0
      while(i < allKeys.length) {
        val k = allKeys(i)
        val v = new Val.Obj.Member(false, Visibility.Normal) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
            func.apply2(Val.Str(pos, k), () => obj.value(k, pos.noOffset)(ev), pos.noOffset)(ev)
        }
        m.put(k, v)
        i += 1
      }
      new Val.Obj(pos, m, false, null, null)
    }
  }

  private object MapWithIndex extends Val.Builtin2("func", "arr") {
    def evalRhs(_func: Val, _arr: Val, ev: EvalScope, pos: Position): Val = {
      val func = _func.asFunc
      val arr = _arr.asArr.asLazyArray
      val a = new Array[Lazy](arr.length)
      var i = 0
      while(i < a.length) {
        val x = arr(i)
        val idx = Val.Num(pos, i)
        a(i) = () => func.apply2(idx, x, pos.noOffset)(ev)
        i += 1
      }
      new Val.Arr(pos, a)
    }
  }

  private object Find extends Val.Builtin2("value", "arr") {
    def evalRhs(value: Val, _arr: Val, ev: EvalScope, pos: Position): Val = {
      val arr = _arr.asArr
      val b = new mutable.ArrayBuilder.ofRef[Lazy]
      var i = 0
      while(i < arr.length) {
        if(ev.equal(arr.force(i), value)) {
          val finalI = i
          b.+=(Val.Num(pos, finalI))
        }
        i += 1
      }
      new Val.Arr(pos, b.result())
    }
  }

  private object EncodeUTF8 extends Val.Builtin1("s") {
    def evalRhs(s: Val, ev: EvalScope, pos: Position): Val =
      new Val.Arr(pos, s.asString.getBytes(UTF_8).map(i => Val.Num(pos, i & 0xff)))
  }

  private object DecodeUTF8 extends Val.Builtin1("arr") {
    def evalRhs(arr: Val, ev: EvalScope, pos: Position): Val =
      new Val.Str(pos, new String(arr.asArr.iterator.map(_.cast[Val.Num].value.toByte).toArray, UTF_8))
  }

  private object Substr extends Val.Builtin3("s", "from", "len") {
    def evalRhs(_s: Val, from: Val, len: Val, ev: EvalScope, pos: Position): Val = {
      val s = _s.asString
      val safeOffset = math.min(from.asInt, s.length)
      val safeLength = math.min(len.asInt, s.length - safeOffset)
      Val.Str(pos, s.substring(safeOffset, safeOffset + safeLength))
    }
  }

  private object StartsWith extends Val.Builtin2("a", "b") {
    def evalRhs(a: Val, b: Val, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.asString.startsWith(b.asString))
  }

  private object EndsWith extends Val.Builtin2("a", "b") {
    def evalRhs(a: Val, b: Val, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.asString.endsWith(b.asString))
  }

  private object Char_ extends Val.Builtin1("n") {
    def evalRhs(n: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, n.asInt.toChar.toString)
  }

  private object StrReplace extends Val.Builtin3("str", "from", "to") {
    def evalRhs(str: Val, from: Val, to: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.asString.replace(from.asString, to.asString))
  }

  private object StrReplaceAll extends Val.Builtin3("str", "from", "to") {
    def evalRhs(str: Val, from: Val, to: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.asString.replaceAll(from.asString, to.asString))
    override def specialize(args: Array[Expr]) = args match {
      case Array(str, from: Val.Str, to) =>
        try { (new SpecFrom(Pattern.compile(from.value)), Array(str, to)) } catch { case _: Exception => null }
      case _ => null
    }
    private class SpecFrom(from: Pattern) extends Val.Builtin2("str", "to") {
      def evalRhs(str: Val, to: Val, ev: EvalScope, pos: Position): Val =
        Val.Str(pos, from.matcher(str.asString).replaceAll(to.asString))
    }
  }

  private object Join extends Val.Builtin2("sep", "arr") {
    def evalRhs(sep: Val, _arr: Val, ev: EvalScope, pos: Position): Val = {
      val arr = implicitly[ReadWriter[Val.Arr]].apply(_arr)
      sep match {
        case Val.Str(_, s) =>
          val b = new java.lang.StringBuilder()
          var i = 0
          var added = false
          while(i < arr.length) {
            arr.force(i) match {
              case _: Val.Null =>
              case Val.Str(_, x) =>
                if(added) b.append(s)
                added = true
                b.append(x)
              case x => Error.fail("Cannot join " + x.prettyName)
            }
            i += 1
          }
          Val.Str(pos, b.toString)
        case sep: Val.Arr =>
          val out = new mutable.ArrayBuffer[Lazy]
          var added = false
          for(x <- arr){
            x match{
              case Val.Null(_) => // do nothing
              case v: Val.Arr =>
                if (added) out.appendAll(sep.asLazyArray)
                added = true
                out.appendAll(v.asLazyArray)
              case x => Error.fail("Cannot join " + x.prettyName)
            }
          }
          new Val.Arr(pos, out.toArray)
        case x => Error.fail("Cannot join " + x.prettyName)
      }
    }
  }

  private object Member extends Val.Builtin2("arr", "x") {
    def evalRhs(arr: Val, x: Val, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr match {
        case str: Val.Str =>
          val secondArg = x match {
            case Val.Str(_, value) => value
            case n => Error.fail("std.member second argument must be a string, got " + x.prettyName)
          }
          str.value.contains(secondArg)
        case a: Val.Arr =>
          var c = 0
          a.foreach(v => if(ev.equal(v, x)) c += 1)
          c > 0
        case x => Error.fail("std.member first argument must be an array or a string, got " + arr.prettyName)
      })
    }
  }

  private object FlattenArrays extends Val.Builtin1("arrs") {
    def evalRhs(arrs: Val, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuffer[Lazy]
      for(x <- arrs.asArr) {
        x match{
          case Val.Null(_) => // do nothing
          case v: Val.Arr => out.appendAll(v.asLazyArray)
          case x => Error.fail("Cannot call flattenArrays on " + x)
        }
      }
      new Val.Arr(pos, out.toArray)
    }
  }

  private object Split extends Val.Builtin2("str", "c") {
    def evalRhs(_str: Val, _c: Val, ev: EvalScope, pos: Position): Val = {
      val str = _str.asString
      val cStr = _c.asString
      if(cStr.length != 1) Error.fail("std.split second parameter should have length 1, got "+cStr.length)
      val c = cStr.charAt(0)
      val b = new mutable.ArrayBuilder.ofRef[Lazy]
      var i = 0
      var start = 0
      while(i < str.length) {
        if(str.charAt(i) == c) {
          val finalStr = Val.Str(pos, str.substring(start, i))
          b.+=(finalStr)
          start = i+1
        }
        i += 1
      }
      b.+=(Val.Str(pos, str.substring(start, math.min(i, str.length))))
      new Val.Arr(pos, b.result())
    }
  }

  private object SplitLimit extends Val.Builtin3("str", "c", "maxSplits") {
    def evalRhs(str: Val, c: Val, maxSplits: Val, ev: EvalScope, pos: Position): Val = {
      new Val.Arr(pos, str.asString.split(java.util.regex.Pattern.quote(c.asString), maxSplits.asInt + 1).map(s => Val.Str(pos, s)))
    }
  }

  private object StringChars extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      stringChars(pos, str.asString)
  }

  private object ParseInt extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, str.asString.toInt)
  }

  private object ParseOctal extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, Integer.parseInt(str.asString, 8))
  }

  private object ParseHex extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, Integer.parseInt(str.asString, 16))
  }

  private object MD5 extends Val.Builtin1("s") {
    def evalRhs(s: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Platform.md5(s.asString))
  }

  private object AsciiUpper extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.asString.toUpperCase)
  }

  private object AsciiLower extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.asString.toLowerCase)
  }

  private object Trace extends Val.Builtin2("str", "rest") {
    def evalRhs(str: Val, rest: Val, ev: EvalScope, pos: Position): Val = {
      System.err.println(s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + str.asString)
      rest
    }
  }

  private object ExtVar extends Val.Builtin1("x") {
    def evalRhs(_x: Val, ev: EvalScope, pos: Position): Val = {
      val Val.Str(_, x) = _x
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }
    override def staticSafe = false
  }

  private object ObjectValues extends Val.Builtin1("o") {
    def evalRhs(_o: Val, ev: EvalScope, pos: Position): Val = {
      val o = _o.asObj
      val keys = getVisibleKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object ObjectValuesAll extends Val.Builtin1("o") {
    def evalRhs(_o: Val, ev: EvalScope, pos: Position): Val = {
      val o = _o.asObj
      val keys = getAllKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object Lines extends Val.Builtin1("arr") {
    def evalRhs(v1: Val, ev: EvalScope, pos: Position): Val = {
    v1.asArr.foreach {
      case _: Val.Str | _: Val.Null => // donothing
      case x => Error.fail("Cannot call .lines on " + x.prettyName)
    }
    Val.Str(pos, Materializer.apply(v1)(ev).asInstanceOf[ujson.Arr]
      .value
      .filter(_ != ujson.Null)
      .map{
        case ujson.Str(s) => s + "\n"
        case _ => ??? /* we ensure it's all strings above */
      }
      .mkString)
    }
  }

  private object Range extends Val.Builtin2("from", "to") {
    def evalRhs(from: Val, to: Val, ev: EvalScope, pos: Position): Val =
    new Val.Arr(
      pos,
      (from.asInt to to.asInt).map(i => Val.Num(pos, i)).toArray
    )
  }

  private object ManifestJson extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v, new MaterializeJsonRenderer())(ev).toString)
  }

  private object ManifestJsonMinified extends Val.Builtin1("v") {
    def evalRhs(v: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v, new MaterializeJsonRenderer(indent = -1))(ev).toString)
  }

  private object ManifestJsonEx extends Val.Builtin2("value", "indent") {
    def evalRhs(v: Val, i: Val, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer
        .apply0(v, new MaterializeJsonRenderer(indent = i.asString.length))(ev)
        .toString)
  }

  private object ParseJson extends Val.Builtin1("str") {
    def evalRhs(str: Val, ev: EvalScope, pos: Position): Val =
      ujson.StringParser.transform(str.asString, new ValVisitor(pos))
  }

  private object Set_ extends Val.Builtin2("arr", "keyF", Array(null, Val.False(dummyPos))) {
    def evalRhs(arr: Val, keyF: Val, ev: EvalScope, pos: Position): Val = {
      uniqArr(pos, ev, sortArr(pos, ev, arr, keyF), keyF)
    }
  }

  private object SetInter extends Val.Builtin3("a", "b", "keyF", Array(null, null, Val.False(dummyPos))) {
    def isStr(a: Val.Arr) = a.forall(_.isInstanceOf[Val.Str])
    def isNum(a: Val.Arr) = a.forall(_.isInstanceOf[Val.Num])

    override def specialize(args: Array[Expr]): (Val.Builtin, Array[Expr]) = args match {
      case Array(a: Val.Arr, b) if isStr(a) => (new Spec1Str(a), Array(b))
      case Array(a, b: Val.Arr) if isStr(b) => (new Spec1Str(b), Array(a))
      case args if args.length == 2 => (Spec2, args)
      case _ => null
    }

    def asArray(a: Val): Array[Lazy] = a match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.value).asLazyArray
      case _ => Error.fail("Arguments must be either arrays or strings")
    }

    def evalRhs(_a: Val, _b: Val, _keyF: Val, ev: EvalScope, pos: Position): Val = {
      if(_keyF.isInstanceOf[Val.False]) Spec2.evalRhs(_a, _b, ev, pos)
      else {
        val a = asArray(_a)
        val b = asArray(_b)
        val keyFFunc = _keyF.asInstanceOf[Val.Func]
        val out = new mutable.ArrayBuffer[Lazy]
        for (v <- a) {
          val appliedX = keyFFunc.apply1(v, pos.noOffset)(ev)
          if (b.exists(value => {
            val appliedValue = keyFFunc.apply1(value, pos.noOffset)(ev)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFFunc.apply1(value, pos.noOffset)(ev)
            ev.equal(mValue, appliedX)
          })) {
            out.append(v)
          }
        }
        sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyFFunc)
      }
    }

    private object Spec2 extends Val.Builtin2("a", "b") {
      def evalRhs(_a: Val, _b: Val, ev: EvalScope, pos: Position): Val = {
        val a = asArray(_a)
        val b = asArray(_b)
        val out = new mutable.ArrayBuffer[Lazy](a.length)
        for (v <- a) {
          val vf = v.force
          if (b.exists(value => {
            ev.equal(value.force, vf)
          }) && !out.exists(value => {
            ev.equal(value.force, vf)
          })) {
            out.append(v)
          }
        }
        sortArr(pos, ev, new Val.Arr(pos, out.toArray), null)
      }
    }

    private class Spec1Str(_a: Val.Arr) extends Val.Builtin1("b") {
      private[this] val a =
        ArrayOps.sortInPlaceBy(ArrayOps.distinctBy(_a.asLazyArray)(_.asInstanceOf[Val.Str].value))(_.asInstanceOf[Val.Str].value)
        // 2.13+: _a.asLazyArray.distinctBy(_.asInstanceOf[Val.Str].value).sortInPlaceBy(_.asInstanceOf[Val.Str].value)

      def evalRhs(_b: Val, ev: EvalScope, pos: Position): Val = {
        val b = asArray(_b)
        val bs = new mutable.HashSet[String]
        var i = 0
        while(i < b.length) {
          b(i).force match {
            case s: Val.Str => bs.add(s.value)
            case _ =>
          }
          i += 1
        }
        val out = new mutable.ArrayBuilder.ofRef[Lazy]
        i = 0
        while(i < a.length) {
          val s = a(i).asInstanceOf[Val.Str]
          if(bs.contains(s.value)) out.+=(s)
          i += 1
        }
        new Val.Arr(pos, out.result())
      }
    }
  }

  val functions: Map[String, Val.Func] = Map(
    "assertEqual" -> AssertEqual,
    "toString" -> ToString,
    "codepoint" -> Codepoint,
    "length" -> Length,
    "objectHas" -> ObjectHas,
    "objectHasAll" -> ObjectHasAll,
    "objectFields" -> ObjectFields,
    "objectFieldsAll" -> ObjectFieldsAll,
    "objectValues" -> ObjectValues,
    "objectValuesAll" -> ObjectValuesAll,
    "type" -> Type,
    "lines" -> Lines,
    "format" -> Format_,
    "foldl" -> Foldl,
    "foldr" -> Foldr,
    "range" -> Range,
    builtin("mergePatch", "target", "patch"){ (pos, ev, target: Val, patch: Val) =>
      val mergePosition = pos
      def createMember(v: => Val) = new Val.Obj.Member(false, Visibility.Unhide) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = v
      }
      def recPair(l: Val, r: Val): Val = (l, r) match{
        case (l: Val.Obj, r: Val.Obj) =>
          val kvs = for {
            k <- (l.visibleKeyNames ++ r.visibleKeyNames).distinct
            val lValue = Option(l.valueRaw(k, l, pos)(ev))
            val rValue = Option(r.valueRaw(k, r, pos)(ev))
            if !rValue.exists(_.isInstanceOf[Val.Null])
          } yield (lValue, rValue) match{
            case (Some(lChild), None) => k -> createMember{lChild}
            case (Some(lChild: Val.Obj), Some(rChild: Val.Obj)) => k -> createMember{recPair(lChild, rChild)}
            case (_, Some(rChild)) => k -> createMember{recSingle(rChild)}
          }

          Val.Obj.mk(mergePosition, kvs:_*)

        case (_, _) => recSingle(r)
      }
      def recSingle(v: Val): Val  = v match{
        case obj: Val.Obj =>
          val kvs = for{
            k <- obj.visibleKeyNames
            val value = obj.value(k, pos, obj)(ev)
            if !value.isInstanceOf[Val.Null]
          } yield (k, createMember{recSingle(value)})

          Val.Obj.mk(obj.pos, kvs:_*)

        case _ => v
      }
      recPair(target, patch)
    },
    builtin("sqrt", "x"){ (pos, ev, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b"){ (pos, ev, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b"){ (pos, ev, a: Double, b: Double) =>
      math.min(a, b)
    },
    builtin("mod", "a", "b"){ (pos, ev, a: Int, b: Int) =>
      a % b
    },
    builtin("clamp", "x", "minVal", "maxVal"){ (pos, ev, x: Double, minVal: Double, maxVal: Double) =>
      math.max(minVal, math.min(x, maxVal))
    },
    builtin("slice", "indexable", "index", "end", "step"){ (pos, ev, indexable: Val, index: Int, end: Int, step: Int) =>
      val res = indexable match {
        case Val.Str(pos0, s) => Val.Str(pos, Util.sliceStr(s, index, end, step))
        case arr: Val.Arr => new Val.Arr(pos, Util.sliceArr(arr.asLazyArray, index, end, step))
        case _ => Error.fail("std.slice first argument must be indexable")
      }
      res: Val
    },

    builtin("makeArray", "sz", "func"){ (pos, ev, sz: Int, func: Val.Func) =>
      new Val.Arr(
        pos,
        {
          val a = new Array[Lazy](sz)
          var i = 0
          while(i < sz) {
            val forcedI = i
            a(i) = () => func.apply1(Val.Num(pos, forcedI), pos.noOffset)(ev)
            i += 1
          }
          a
        }
      )
    },

    builtin("pow", "x", "n"){ (pos, ev, x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (pos, ev, x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (pos, ev, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (pos, ev, x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (pos, ev, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (pos, ev, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (pos, ev, x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (pos, ev, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (pos, ev, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (pos, ev, x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (pos, ev, x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (pos, ev, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (pos, ev, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (pos, ev, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      //val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    "isString" -> IsString,
    "isBoolean" -> IsBoolean,
    "isNumber" -> IsNumber,
    "isObject" -> IsObject,
    "isArray" -> IsArray,
    "isFunction" -> IsFunction,
    "count" -> Count,
    "filter" -> Filter,
    "map" -> Map_,
    "mapWithKey" -> MapWithKey,
    "mapWithIndex" -> MapWithIndex,
    builtin("flatMap", "func", "arr"){ (pos, ev, func: Val.Func, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val arrResults = a.asLazyArray.flatMap {
            v => {
              val fres = func.apply1(v, pos.noOffset)(ev)
              fres match {
                case va: Val.Arr => va.asLazyArray
                case unknown => Error.fail("flatMap func must return an array, not " + unknown)
              }
            }
          }
          new Val.Arr(pos, arrResults)

        case s: Val.Str =>
          val builder = new StringBuilder()
          for (c: Char <- s.value) {
            val fres = func.apply1(Val.Str(pos, c.toString), pos.noOffset)(ev)
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.value
                case _: Val.Null => ""
                case x => Error.fail("flatMap func must return string, got " + fres.asInstanceOf[Val].prettyName)
              }
            )
          }
          Val.Str(pos, builder.toString)
      }
      res
    },

    builtin("filterMap", "filter_func", "map_func", "arr"){ (pos, ev, filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
      new Val.Arr(
        pos,
        arr.asLazyArray.flatMap { i =>
          i.force
          if (!filter_func.apply1(i, pos.noOffset)(ev).isInstanceOf[Val.True]) None
          else Some[Lazy](() => map_func.apply1(i, pos.noOffset)(ev))
        }
      )
    },
    "find" -> Find,
    builtin("findSubstr", "pat", "str") { (pos, ev, pat: String, str: String) =>
      if (pat.length == 0) new Val.Arr(pos, emptyLazyArray)
      else {
        val indices = mutable.ArrayBuffer[Int]()
        var matchIndex = str.indexOf(pat)
        while (0 <= matchIndex && matchIndex < str.length) {
          indices.append(matchIndex)
          matchIndex = str.indexOf(pat, matchIndex + 1)
        }
        new Val.Arr(pos, indices.map(x => Val.Num(pos, x)).toArray)
      }
    },
    "substr" -> Substr,
    "startsWith" -> StartsWith,
    "endsWith" -> EndsWith,
    "char" -> Char_,
    "strReplace" -> StrReplace,
    "strReplaceAll" -> StrReplaceAll,

    builtin("rstripChars", "str", "chars"){ (pos, ev, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "")
    },
    builtin("lstripChars", "str", "chars"){ (pos, ev, str: String, chars: String) =>
      str.replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },
    builtin("stripChars", "str", "chars"){ (pos, ev, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "").replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },
    "join" -> Join,
    "member" -> Member,

    builtin("repeat", "what", "count"){ (pos, ev, what: Val, count: Int) =>
      val res: Val = what match {
        case str: Val.Str =>
          val builder = new StringBuilder
          for (i <- 1 to count) {
            builder.append(str.value)
          }
          Val.Str(pos, builder.toString())
        case a: Val.Arr =>
          val out = new mutable.ArrayBuffer[Lazy]
          for (i <- 1 to count) {
            out.appendAll(a.asLazyArray)
          }
          new Val.Arr(pos, out.toArray)
        case x => Error.fail("std.repeat first argument must be an array or a string")
      }
      res
    },

    "flattenArrays" -> FlattenArrays,

    builtin("manifestIni", "v"){ (pos, ev, v: Val) =>
      val materialized = Materializer(v)(ev)
      def render(x: ujson.Value) = x match{
        case ujson.Str(v) => v
        case ujson.Num(v) => RenderUtils.renderDouble(v)
        case ujson.Bool(v) => v.toString
        case ujson.Null => "null"
        case _ => x.transform(new sjsonnet.Renderer())
      }
      def sect(x: ujson.Obj) = {
        x.value.flatMap{
          case (k, ujson.Arr(vs)) => vs.map(x => k + " = " + render(x))
          case (k, v) => Seq(k + " = " + render(v))
        }
      }
      val lines = materialized.obj.get("main").fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Obj])) ++
        materialized.obj.get("sections").fold(Iterable[String]())(x =>
          x.obj.flatMap{case (k, v) => Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Obj])}
        )
      lines.flatMap(Seq(_, "\n")).mkString
    },
    builtin("escapeStringJson", "str"){ (pos, ev, str: String) =>
      val out = new StringWriter()
      BaseCharRenderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (pos, ev, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (pos, ev, str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (pos, ev, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    "manifestJson" -> ManifestJson,
    "manifestJsonMinified" -> ManifestJsonMinified,
    "manifestJsonEx" -> ManifestJsonEx,
    builtinWithDefaults("manifestYamlDoc",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos)){ (args, pos, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1)  match {
          case Val.False(_) => false
          case Val.True(_) => true
          case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
        }
      Materializer.apply0(
        v,
        new YamlRenderer(indentArrayInObject = indentArrayInObject)
      )(ev).toString
    },
    builtinWithDefaults("manifestYamlStream",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos)){ (args, pos, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1)  match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      v match {
        case arr: Val.Arr => arr.asLazyArray
          .map { item =>
            Materializer.apply0(
              item.force,
              new YamlRenderer(indentArrayInObject = indentArrayInObject)
            )(ev).toString()
          }
          .mkString("---\n", "\n---\n", "\n...\n")
        case _ => Error.fail("manifestYamlStream only takes arrays, got " + v.getClass)
      }
    },
    builtin("manifestPythonVars", "v"){ (pos, ev, v: Val.Obj) =>
      Materializer(v)(ev).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (pos, ev, value: Val) =>
      import scalatags.Text.all.{value => _, _}
      def rec(v: ujson.Value): Frag = {
        v match {
          case ujson.Str(s) => s
          case ujson.Arr(mutable.Seq(ujson.Str(t), attrs: ujson.Obj, children@_*)) =>
            tag(t)(
              attrs.value.map {
                case (k, ujson.Str(v)) => attr(k) := v

                // use ujson.write to make sure output number format is same as
                // google/jsonnet, e.g. whole numbers are printed without the
                // decimal point and trailing zero
                case (k, ujson.Num(v)) => attr(k) := ujson.write(v)

                case (k, v) => Error.fail("Cannot call manifestXmlJsonml on " + v.getClass)
              }.toSeq,
              children.map(rec)
            )
          case ujson.Arr(mutable.Seq(ujson.Str(t), children@_*)) =>
            tag(t)(children.map(rec).toSeq)
          case x =>
            Error.fail("Cannot call manifestXmlJsonml on " + x.getClass)
        }
      }
      rec(Materializer(value)(ev)).render
    },
    builtin("base64", "v"){ (pos, ev, v: Val) =>
      v match{
        case Val.Str(_, value) => Base64.getEncoder().encodeToString(value.getBytes)
        case arr: Val.Arr => Base64.getEncoder().encodeToString(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => Error.fail("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (pos, ev, s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (pos, ev, s: String) =>
      new Val.Arr(pos, Base64.getDecoder().decode(s).map(i => Val.Num(pos, i)))
    },

    builtin("gzip", "v"){ (pos, ev, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.gzipString(value)
        case arr: Val.Arr => Platform.gzipBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => Error.fail("Cannot gzip encode " + x.prettyName)
      }
    },

    builtin("xz", "v"){ (pos, ev, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.xzString(value)
        case arr: Val.Arr => Platform.xzBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => Error.fail("Cannot xz encode " + x.prettyName)
      }
    },

    "encodeUTF8" -> EncodeUTF8,
    "decodeUTF8" -> DecodeUTF8,

    builtinWithDefaults("uniq", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      uniqArr(pos, ev, args(0), args(1))
    },
    builtinWithDefaults("sort", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      sortArr(pos, ev, args(0), args(1))
    },
    "set" -> Set_,
    builtinWithDefaults("setUnion", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val a = args(0) match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => Error.fail("Arguments must be either arrays or strings")
      }
      val b = args(1) match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => Error.fail("Arguments must be either arrays or strings")
      }
      val concat = new Val.Arr(pos, a ++ b)
      uniqArr(pos, ev, sortArr(pos, ev, concat, args(2)), args(2))
    },
    "setInter" -> SetInter,
    builtinWithDefaults("setDiff", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>

      val a = args(0) match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => Error.fail("Arguments must be either arrays or strings")
      }
      val b = args(1) match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => Error.fail("Arguments must be either arrays or strings")
      }

      val keyF = args(2)
      val out = new mutable.ArrayBuffer[Lazy]

      for (v <- a) {
        if (keyF.isInstanceOf[Val.False]) {
          val vf = v.force
          if (!b.exists(value => {
            ev.equal(value.force, vf)
          }) && !out.exists(value => {
            ev.equal(value.force, vf)
          })) {
            out.append(v)
          }
        } else {
          val keyFFunc = keyF.asInstanceOf[Val.Func]
          val appliedX = keyFFunc.apply1(v, pos.noOffset)(ev)

          if (!b.exists(value => {
            val appliedValue = keyFFunc.apply1(value, pos.noOffset)(ev)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFFunc.apply1(value, pos.noOffset)(ev)
            ev.equal(mValue, appliedX)
          })) {
            out.append(v)
          }
        }
      }

      sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyF)
    },
    builtinWithDefaults("setMember", "x" -> null, "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val keyF = args(2)

      if (keyF.isInstanceOf[Val.False]) {
        val ujson.Arr(mArr) = Materializer(args(1))(ev)
        val mx = Materializer(args(0))(ev)
        mArr.contains(mx)
      } else {
        val arr = args(1).asInstanceOf[Val.Arr].asLazyArray
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val appliedX = keyFFunc.apply1(args(0), pos.noOffset)(ev)
        arr.exists(value => {
          val appliedValue = keyFFunc.apply1(value, pos.noOffset)(ev)
          ev.equal(appliedValue, appliedX)
        })
      }
    },

    "split" -> Split,
    "splitLimit" -> SplitLimit,
    "stringChars" -> StringChars,
    "parseInt" -> ParseInt,
    "parseOctal" -> ParseOctal,
    "parseHex" -> ParseHex,
    "parseJson" -> ParseJson,
    "md5" -> MD5,
    builtin("prune", "x"){ (pos, ev, s: Val) =>
      def filter(x: Val) = x match{
        case c: Val.Arr if c.length == 0 => false
        case c: Val.Obj if c.visibleKeyNames.length == 0 => false
        case Val.Null(_) => false
        case _ => true
      }
      def rec(x: Val): Val = x match{
        case o: Val.Obj =>
          val bindings = for{
            k <- o.visibleKeyNames
            v = rec(o.value(k, pos.fileScope.noOffsetPos)(ev))
            if filter(v)
          }yield (k, new Val.Obj.ConstMember(false, Visibility.Normal, v))
          Val.Obj.mk(pos, bindings: _*)
        case a: Val.Arr =>
          new Val.Arr(pos, a.asStrictArray.map(rec).filter(filter).map(identity))
        case _ => x
      }
      rec(s)
    },

    "asciiUpper" -> AsciiUpper,
    "asciiLower" -> AsciiLower,
    "trace" -> Trace,
    "extVar" -> ExtVar
  )
  val Std = Val.Obj.mk(
    null,
    functions.toSeq
      .map{
        case (k, v) =>
          (
            k,
            new Val.Obj.ConstMember(false, Visibility.Hidden, v)
          )
      } ++ Seq(
      (
        "thisFile",
        new Val.Obj.Member(false, Visibility.Hidden, cached = false) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
            Val.Str(self.pos, fs.currentFile.relativeToString(ev.wd))
        }
      )
    ): _*
  )

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (Position, EvalScope, T1) => R): (String, Val.Func) = {
    (name, new Val.Builtin1(p1) {
      def evalRhs(arg1: Val, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Position, EvalScope, T1, T2) => R): (String, Val.Func) = {
    (name, new Val.Builtin2(p1, p2) {
      def evalRhs(arg1: Val, arg2: Val, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Position, EvalScope, T1, T2, T3) => R): (String, Val.Func) = {
    (name, new Val.Builtin3(p1, p2, p3) {
      def evalRhs(arg1: Val, arg2: Val, arg3: Val, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter, T4: ReadWriter]
             (name: String, p1: String, p2: String, p3: String, p4: String)
             (eval: (Position, EvalScope, T1, T2, T3, T4) => R): (String, Val.Func) = {
    (name, new Val.Builtin4(p1, p2, p3, p4) {
      def evalRhs(arg1: Val, arg2: Val, arg3: Val, arg4: Val, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3)
        val v4: T4 = implicitly[ReadWriter[T4]].apply(arg4)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3, v4))
      }
    })
  }

  /**
    * Helper function that can define a built-in function with default parameters
    *
    * Arguments of the eval function are (args, ev)
    */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Val.Literal)*)
                                        (eval: (Array[Val], Position, EvalScope) => R): (String, Val.Func) = {
    val indexedParamKeys = params.zipWithIndex.map{case ((k, v), i) => (k, i)}.toArray
    name -> new Val.Builtin(params.map(_._1).toArray, params.map(_._2).toArray) {
      def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
        implicitly[ReadWriter[R]].write(pos, eval(args, pos, ev))
    }
  }

  def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    val arrValue = arr match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.value).asLazyArray
      case _ => Error.fail("Argument must be either array or string")
    }

    val out = new mutable.ArrayBuffer[Lazy]
    for (v <- arrValue) {
      if (out.isEmpty) {
        out.append(v)
      } else if (keyF.isInstanceOf[Val.False]) {
        if (!ev.equal(out.last.force, v.force)) {
          out.append(v)
        }
      } else if (!keyF.isInstanceOf[Val.False]) {
        val keyFFunc = keyF.asInstanceOf[Val.Func]

        val o1Key = keyFFunc.apply1(v, pos.noOffset)(ev)
        val o2Key = keyFFunc.apply1(out.last, pos.noOffset)(ev)
        val o1KeyExpr = Materializer.toExpr(Materializer.apply(o1Key)(ev))(ev)
        val o2KeyExpr = Materializer.toExpr(Materializer.apply(o2Key)(ev))(ev)

        val comparisonExpr = Expr.BinaryOp(dummyPos, o1KeyExpr, BinaryOp.OP_!=, o2KeyExpr)
        val exprResult = ev.visitExpr(comparisonExpr)(ValScope.empty)

        val res = Materializer.apply(exprResult)(ev).asInstanceOf[ujson.Bool]

        if (res.value) {
          out.append(v)
        }
      }
    }

    new Val.Arr(pos, out.toArray)
  }

  def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    arr match{
      case vs: Val.Arr =>
        new Val.Arr(
          pos,

          if (vs.forall(_.isInstanceOf[Val.Str])){
            vs.asStrictArray.map(_.cast[Val.Str]).sortBy(_.value)
          }else if (vs.forall(_.isInstanceOf[Val.Num])) {
            vs.asStrictArray.map(_.cast[Val.Num]).sortBy(_.value)
          }else if (vs.forall(_.isInstanceOf[Val.Obj])){
            if (keyF == null || keyF.isInstanceOf[Val.False]) {
              Error.fail("Unable to sort array of objects without key function")
            } else {
              val objs = vs.asStrictArray.map(_.cast[Val.Obj])

              val keyFFunc = keyF.asInstanceOf[Val.Func]
              val keys = objs.map((v) => keyFFunc(Array(v), null, pos.noOffset)(ev))

              if (keys.forall(_.isInstanceOf[Val.Str])){
                objs.sortBy((v) => keyFFunc(Array(v), null, pos.noOffset)(ev).cast[Val.Str].value)
              } else if (keys.forall(_.isInstanceOf[Val.Num])) {
                objs.sortBy((v) => keyFFunc(Array(v), null, pos.noOffset)(ev).cast[Val.Num].value)
              } else {
                Error.fail("Cannot sort with key values that are " + keys(0).prettyName + "s")
              }
            }
          }else {
            ???
          }
        )
      case Val.Str(pos, s) => new Val.Arr(pos, s.sorted.map(c => Val.Str(pos, c.toString)).toArray)
      case x => Error.fail("Cannot sort " + x.prettyName)
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    val a = new Array[Lazy](str.length)
    var i = 0
    while(i < a.length) {
      a(i) = new Val.Str(pos, String.valueOf(str.charAt(i)))
      i += 1
    }
    new Val.Arr(pos, a)
  }
  
  def getVisibleKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.visibleKeyNames)

  def getAllKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.allKeyNames)
  
  @inline private[this] def maybeSortKeys(ev: EvalScope, keys: Array[String]): Array[String] =
    if(ev.settings.preserveOrder) keys else keys.sorted

  def getObjValuesFromKeys(pos: Position, ev: EvalScope, v1: Val.Obj, keys: Array[String]): Val.Arr =
    new Val.Arr(pos, keys.map { k =>
      (() => v1.value(k, pos.noOffset)(ev)): Lazy
    })
}
