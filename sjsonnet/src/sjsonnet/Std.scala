package sjsonnet

import java.io.StringWriter
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util
import sjsonnet.Expr.Member.Visibility

import scala.collection.Searching._
import scala.collection.mutable

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
class Std(private val additionalNativeFunctions: Map[String, Val.Builtin] = Map.empty) {
  private val dummyPos: Position = new Position(null, 0)
  private val emptyLazyArray = new Array[Lazy](0)
  private val leadingWhiteSpacePattern = Platform.getPatternFromCache("^[ \t\n\f\r\u0085\u00A0']+")
  private val trailingWhiteSpacePattern = Platform.getPatternFromCache("[ \t\n\f\r\u0085\u00A0']+$")
  private val builtinNativeFunctions = Map(
    builtin("gzip", "v"){ (_, _, v: Val) =>
      v match {
        case Val.Str(_, value) => Platform.gzipString(value)
        case arr: Val.Arr => Platform.gzipBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => Error.fail("Cannot gzip encode " + x.prettyName)
      }
    },

    builtinWithDefaults("xz", "v" -> null, "compressionLevel" -> Val.Null(dummyPos)){ (args, pos, ev) =>
      val compressionLevel: Option[Int] = args(1) match {
        case Val.Null(_) =>
          // Use default compression level if the user didn't set one
          None
        case Val.Num(_, n) =>
          Some(n.toInt)
        case x =>
          Error.fail("Cannot xz encode with compression level " + x.prettyName)
      }
      args(0) match {
        case Val.Str(_, value) => Platform.xzString(value, compressionLevel)
        case arr: Val.Arr => Platform.xzBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray, compressionLevel)
        case x => Error.fail("Cannot xz encode " + x.prettyName)
      }
    },
  ) ++ StdRegex.functions
  require(builtinNativeFunctions.forall(k => !additionalNativeFunctions.contains(k._1)), "Conflicting native functions")
  private val nativeFunctions = builtinNativeFunctions ++ additionalNativeFunctions

  private object AssertEqual extends Val.Builtin2("assertEqual", "a", "b") {
    def evalRhs(v1: Lazy, v2: Lazy, ev: EvalScope, pos: Position): Val = {
      val x1 = Materializer(v1.force)(ev)
      val x2 = Materializer(v2.force)(ev)
      if (x1 == x2) Val.True(pos)
      else Error.fail("assertEqual failed: " + x1 + " != " + x2)
    }
  }

  private object ToString extends Val.Builtin1("toString", "a") {
    def evalRhs(v1: Lazy, ev: EvalScope, pos: Position): Val = Val.Str(pos, v1.force match {
      case Val.Str(_, s) => s
      case v => Materializer.stringify(v)(ev)
    })
  }

  private object Length extends Val.Builtin1("length", "x") {
    def evalRhs(x: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, x.force match {
        case Val.Str(_, s) => s.length
        case a: Val.Arr => a.length
        case o: Val.Obj => o.visibleKeyNames.length
        case o: Val.Func => o.params.names.length
        case x => Error.fail("Cannot get length of " + x.prettyName)
      })
    override def specialize(args: Array[Expr], tailstrict: Boolean) = args match {
      case Array(Expr.ApplyBuiltin2(_, Filter, f, a, tailstrict)) => (CountF, Array(f, a))
      case _ => null
    }
  }

  private object CountF extends Val.Builtin2("length", "func", "arr") {
    def evalRhs(_func: Lazy, arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.force.asArr.asLazyArray
      var i = 0
      val func = _func.force.asFunc
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

  private object Codepoint extends Val.Builtin1("codepoint", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, str.force.asString.charAt(0).toLong)
  }

  private object ObjectHas extends Val.Builtin2("objectHas", "o", "f") {
    def evalRhs(o: Lazy, f: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, o.force.asObj.containsVisibleKey(f.force.asString))
    override def specialize(args: Array[Expr], tailstrict: Boolean) = args match {
      case Array(o, s: Val.Str) => (new SpecF(s.value), Array(o))
      case _ => null
    }
    private class SpecF(f: String) extends Val.Builtin1("objectHas", "o") {
      def evalRhs(o: Lazy, ev: EvalScope, pos: Position): Val =
        Val.bool(pos, o.force.asObj.containsVisibleKey(f))
    }
  }

  private object ObjectHasAll extends Val.Builtin2("objectHasAll", "o", "f") {
    def evalRhs(o: Lazy, f: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, o.force.asObj.containsKey(f.force.asString))
    override def specialize(args: Array[Expr], tailstrict: Boolean) = args match {
      case Array(o, s: Val.Str) => (new SpecF(s.value), Array(o))
      case _ => null
    }
    class SpecF(f: String) extends Val.Builtin1("objectHasAll", "o") {
      def evalRhs(o: Lazy, ev: EvalScope, pos: Position): Val =
        Val.bool(pos, o.force.asObj.containsKey(f))
    }
  }

  private object ObjectFields extends Val.Builtin1("objectFields", "o") {
    def evalRhs(o: Lazy, ev: EvalScope, pos: Position): Val = {
      val keys = getVisibleKeys(ev, o.force.asObj)
      new Val.Arr(pos, keys.map(k => Val.Str(pos, k)))
    }
  }

  private object ObjectFieldsAll extends Val.Builtin1("objectFieldsAll", "o") {
    def evalRhs(o: Lazy, ev: EvalScope, pos: Position): Val = {
      val keys = getAllKeys(ev, o.force.asObj)
      new Val.Arr(pos, keys.map(k => Val.Str(pos, k)))
    }
  }

  private object All extends Val.Builtin1("all", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr.force.asArr.forall(v => v.asBoolean))
    }
  }


  private object Get extends Val.Builtin("get", Array("o", "f", "default", "inc_hidden"), Array(null, null, Val.Null(dummyPos), Val.True(dummyPos))) {
    override def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val = {
      val obj = args(0).force.asObj
      val k = args(1).force.asString
      val incHidden = args(3).force.asBoolean
      if (incHidden && obj.containsKey(k)) {
        obj.value(k, pos.noOffset, obj)(ev)
      } else if (!incHidden && obj.containsVisibleKey(k)) {
        obj.value(k, pos.noOffset, obj)(ev)
      } else {
        args(2).force
      }
    }
  }

  private object MinArray extends Val.Builtin("minArray", Array("arr", "keyF", "onEmpty"), Array(null, Val.False(dummyPos), Val.False(dummyPos))) {
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
        arr.asStrictArray.map(v => keyF.asInstanceOf[Val.Func].apply1(v, pos.fileScope.noOffsetPos)(ev)).min(ev)
      }
    }
  }

  private object MaxArray extends Val.Builtin("maxArray", Array("arr", "keyF", "onEmpty"), Array(null, Val.False(dummyPos), Val.False(dummyPos))) {
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
        arr.asStrictArray.map(v => keyF.asInstanceOf[Val.Func].apply1(v, pos.fileScope.noOffsetPos)(ev)).max(ev)
      }
    }
  }

  private object Any extends Val.Builtin1("any", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr.force.asArr.iterator.exists(v => v.asBoolean))
    }
  }

  private object Type extends Val.Builtin1("type", "x") {
    def evalRhs(x: Lazy, ev: EvalScope, pos: Position): Val = Val.Str(pos, x.force.prettyName)
  }

  private object Format_ extends Val.Builtin2("format", "str", "vals") {
    def evalRhs(str: Lazy, vals: Lazy, ev: EvalScope, pos: Position): Val =
      new Val.Str(pos, Format.format(str.force.asString, vals.force, pos)(ev))
    override def specialize(args: Array[Expr], tailstrict: Boolean) = args match {
      case Array(str, fmt: Val.Str) =>
        try { (new Format.PartialApplyFmt(fmt.value), Array(str)) } catch { case _: Exception => null }
      case _ => null
    }
  }

  private object Foldl extends Val.Builtin3("foldl", "func", "arr", "init") {
    def evalRhs(_func: Lazy, arr: Lazy, init: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      arr.force match {
        case arr: Val.Arr =>
          var current = init.force
          for (item <- arr.asLazyArray) {
            val c = current
            current = func.apply2(c, item, pos.noOffset)(ev)
          }
          current

        case s: Val.Str =>
          var current = init.force
          for (char <- s.value) {
            val c = current
            current = func.apply2(c, Val.Str(pos, new String(Array(char))), pos.noOffset)(ev)
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
            current = func.apply2(item, c, pos.noOffset)(ev)
          }
          current
        case s: Val.Str =>
          var current = init.force
          for (char <- s.value) {
            val c = current
            current = func.apply2(Val.Str(pos, new String(Array(char))), c, pos.noOffset)(ev)
          }
          current
        case arr => Error.fail("Cannot call foldr on " + arr.prettyName)
      }
    }
  }

  private object IsString extends Val.Builtin1("isString", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Str])
  }

  private object IsBoolean extends Val.Builtin1("isBoolean", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Bool])
  }

  private object IsNumber extends Val.Builtin1("isNumber", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Num])
  }

  private object IsObject extends Val.Builtin1("isObject", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Obj])
  }

  private object IsArray extends Val.Builtin1("isArray", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Arr])
  }

  private object IsFunction extends Val.Builtin1("isFunction", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val = Val.bool(pos, v.force.isInstanceOf[Val.Func])
  }

  private object Count extends Val.Builtin2("count", "arr", "x") {
    def evalRhs(arr: Lazy, x: Lazy, ev: EvalScope, pos: Position): Val = {
      var count = 0
      arr.force.asArr.foreach(v => if(ev.equal(v.force, x.force)) count += 1)
      Val.Num(pos, count)
    }
  }

  private object Filter extends Val.Builtin2("filter", "func", "arr") {
    def evalRhs(_func: Lazy, arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val p = pos.noOffset
      val a = arr.force.asArr.asLazyArray
      var i = 0
      val func = _func.force.asFunc
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
    }
  }

  private object Map_ extends Val.Builtin2("map", "func", "arr") {
    def evalRhs(_func: Lazy, arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      new Val.Arr(pos, arr.force.asArr.asLazyArray.map(v => (() => func.apply1(v, pos.noOffset)(ev)): Lazy))
    }
  }

  private object MapWithKey extends Val.Builtin2("mapWithKey", "func", "obj") {
    def evalRhs(_func: Lazy, _obj: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      val obj = _obj.force.asObj
      val allKeys = obj.allKeyNames
      val m = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](allKeys.length)
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
      val valueCache = Val.Obj.getEmptyValueCacheForObjWithoutSuper(allKeys.length)
      new Val.Obj(pos, m, false, null, null, valueCache)
    }
  }

  private object MapWithIndex extends Val.Builtin2("mapWithIndex", "func", "arr") {
    def evalRhs(_func: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val func = _func.force.asFunc
      val arr = _arr.force.asArr.asLazyArray
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

  private object Find extends Val.Builtin2("find", "value", "arr") {
    def evalRhs(value: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val arr = _arr.force.asArr
      val b = new mutable.ArrayBuilder.ofRef[Lazy]
      var i = 0
      while(i < arr.length) {
        if(ev.equal(arr.force(i), value.force)) {
          val finalI = i
          b.+=(Val.Num(pos, finalI))
        }
        i += 1
      }
      new Val.Arr(pos, b.result())
    }
  }

  private object EncodeUTF8 extends Val.Builtin1("encodeUTF8", "s") {
    def evalRhs(s: Lazy, ev: EvalScope, pos: Position): Val =
      new Val.Arr(pos, s.force.asString.getBytes(UTF_8).map(i => Val.Num(pos, i & 0xff)))
  }

  private object DecodeUTF8 extends Val.Builtin1("decodeUTF8", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val =
      new Val.Str(pos, new String(arr.force.asArr.iterator.map(_.cast[Val.Num].value.toByte).toArray, UTF_8))
  }

  private object Substr extends Val.Builtin3("substr", "s", "from", "len") {
    def evalRhs(_s: Lazy, from: Lazy, len: Lazy, ev: EvalScope, pos: Position): Val = {
      val s = _s.force.asString
      val safeOffset = math.min(from.force.asInt, s.length)
      val safeLength = math.min(len.force.asInt, s.length - safeOffset)
      Val.Str(pos, s.substring(safeOffset, safeOffset + safeLength))
    }
  }

  private object StartsWith extends Val.Builtin2("startsWith", "a", "b") {
    def evalRhs(a: Lazy, b: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.force.asString.startsWith(b.force.asString))
  }

  private object EndsWith extends Val.Builtin2("endsWith", "a", "b") {
    def evalRhs(a: Lazy, b: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.force.asString.endsWith(b.force.asString))
  }

  private object Char_ extends Val.Builtin1("char", "n") {
    def evalRhs(n: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, n.force.asLong.toChar.toString)
  }

  private object StrReplace extends Val.Builtin3("strReplace", "str", "from", "to") {
    def evalRhs(str: Lazy, from: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.replace(from.force.asString, to.force.asString))
  }

  private object StrReplaceAll extends Val.Builtin3("strReplaceAll", "str", "from", "to") {
    def evalRhs(str: Lazy, from: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.replaceAll(from.force.asString, to.force.asString))
    override def specialize(args: Array[Expr], tailstrict: Boolean) = args match {
      case Array(str, from: Val.Str, to) =>
        try { (new SpecFrom(from.value), Array(str, to)) } catch { case _: Exception => null }
      case _ => null
    }
    private class SpecFrom(from: String) extends Val.Builtin2("strReplaceAll", "str", "to") {
      private val pattern = Platform.getPatternFromCache(from)
      def evalRhs(str: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val =
        Val.Str(pos, pattern.matcher(str.force.asString).replaceAll(to.force.asString))
    }
  }

  private object StripUtils {
    private def getLeadingPattern(chars: String): String = "^[" + Platform.regexQuote(chars) + "]+"

    private def getTrailingPattern(chars: String): String = "[" + Platform.regexQuote(chars) + "]+$"

    def unspecializedStrip(str: String, chars: String, left: Boolean, right: Boolean): String = {
      var s = str
      if (right) s = Platform.getPatternFromCache(getTrailingPattern(chars)).matcher(s).replaceAll("")
      if (left) s = Platform.getPatternFromCache(getLeadingPattern(chars)).matcher(s).replaceAll("")
      s
    }

    private class SpecStrip(
      chars: String,
      left: Boolean,
      right: Boolean,
      functionName: String
    ) extends Val.Builtin1(functionName, "str") {
      private val leftPattern = Platform.getPatternFromCache(getLeadingPattern(chars))
      private val rightPattern = Platform.getPatternFromCache(getTrailingPattern(chars))

      def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val = {
        var s = str.force.asString
        if (right) s = rightPattern.matcher(s).replaceAll("")
        if (left) s = leftPattern.matcher(s).replaceAll("")
        Val.Str(pos, s)
      }
    }

    def trySpecialize(str: Expr, chars: Val.Str, left: Boolean, right: Boolean, name: String): (Val.Builtin, Array[Expr]) = {
      try {
        (new SpecStrip(chars.value, left, right, name), Array(str))
      } catch {
        case _: Exception => null
      }
    }
  }

  object StripChars extends Val.Builtin2("stripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Str(pos, StripUtils.unspecializedStrip(str.force.asString, chars.force.asString, left = true, right = true))
    }

    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) = args match {
      case Array(str, chars: Val.Str) =>
        StripUtils.trySpecialize(str, chars, left = true, right = true, functionName)
      case _ => null
    }
  }

  object LStripChars extends Val.Builtin2("lstripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Str(pos, StripUtils.unspecializedStrip(str.force.asString, chars.force.asString,  left = true, right = false))
    }

    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) = args match {
      case Array(str, chars: Val.Str) =>
        StripUtils.trySpecialize(str, chars, left = true, right = false, functionName)
      case _ => null
    }
  }

  object RStripChars extends Val.Builtin2("rstripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Str(pos, StripUtils.unspecializedStrip(str.force.asString, chars.force.asString, left = false, right = true))
    }

    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) = args match {
      case Array(str, chars: Val.Str) =>
        StripUtils.trySpecialize(str, chars, left = false, right = true, functionName)
      case _ => null
    }
  }

  private object Join extends Val.Builtin2("join", "sep", "arr") {
    def evalRhs(sep: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val arr = implicitly[ReadWriter[Val.Arr]].apply(_arr.force)
      sep.force match {
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
            x match {
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

  private object Member extends Val.Builtin2("member", "arr", "x") {
    def evalRhs(arr: Lazy, x: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.bool(pos, arr.force match {
        case str: Val.Str =>
          val secondArg = x.force match {
            case Val.Str(_, value) => value
            case n => Error.fail("std.member second argument must be a string, got " + n.prettyName)
          }
          str.value.contains(secondArg)
        case a: Val.Arr =>
          a.asLazyArray.indexWhere(v => ev.equal(v.force, x.force)) >= 0
        case arr => Error.fail("std.member first argument must be an array or a string, got " + arr.prettyName)
      })
    }
  }

  private object FlattenArrays extends Val.Builtin1("flattenArrays", "arrs") {
    def evalRhs(arrs: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new mutable.ArrayBuilder.ofRef[Lazy]
      for(x <- arrs.force.asArr) {
        x.force match {
          case Val.Null(_) => // do nothing
          case v: Val.Arr => out ++= v.asLazyArray
          case x => Error.fail("Cannot call flattenArrays on " + x)
        }
      }
      new Val.Arr(pos, out.result())
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
          case x => out += x
        }
      }
      new Val.Arr(pos, out.result())
    }
  }

  private object DeepJoin extends Val.Builtin1("deepJoin", "arr") {
    def evalRhs(value: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter()
      val q = new java.util.ArrayDeque[Lazy]()
      q.add(value)
      while (!q.isEmpty) {
        q.removeFirst().force match {
          case v: Val.Arr => v.asLazyArray.reverseIterator.foreach(q.push)
          case s: Val.Str => out.write(s.value)
          case s => Error.fail("Cannot call deepJoin on " + s.prettyName)
        }
      }
      Val.Str(pos, out.toString)
    }
  }

  private object Reverse extends Val.Builtin1("reverse", "arrs") {
    def evalRhs(arrs: Lazy, ev: EvalScope, pos: Position): Val = {
      new Val.Arr(pos, arrs.force.asArr.asLazyArray.reverse)
    }
  }

  private def splitLimit(pos: Position, str: String, cStr: String, maxSplits: Int): Array[Lazy] = {
    val b = new mutable.ArrayBuilder.ofRef[Lazy]
    var sz = 0
    var i = 0
    var start = 0

    while (i <= str.length - cStr.length && (maxSplits < 0 || sz < maxSplits)) {
      if (str.startsWith(cStr, i)) {
        val finalStr = Val.Str(pos, str.substring(start, i))
        b.+=(finalStr)
        start = i + cStr.length
        sz += 1
        i += cStr.length
      } else {
        i += 1
      }
    }
    b.+=(Val.Str(pos, str.substring(start)))
    sz += 1
    b.result()
  }

  private object Split extends Val.Builtin2("split", "str", "c") {
    def evalRhs(str: Lazy, c: Lazy, ev: EvalScope, pos: Position): Val = {
      new Val.Arr(pos, splitLimit(pos, str.force.asString, c.force.asString, -1))
    }
  }

  private object SplitLimit extends Val.Builtin3("splitLimit", "str", "c", "maxSplits") {
    def evalRhs(str: Lazy, c: Lazy, maxSplits: Lazy, ev: EvalScope, pos: Position): Val = {
      new Val.Arr(pos, splitLimit(pos, str.force.asString, c.force.asString, maxSplits.force.asInt))
    }
  }

  private object SplitLimitR extends Val.Builtin3("splitLimitR", "str", "c", "maxSplits") {
    def evalRhs(str: Lazy, c: Lazy, maxSplits: Lazy, ev: EvalScope, pos: Position): Val = {
      new Val.Arr(pos, splitLimit(pos, str.force.asString.reverse, c.force.asString.reverse, maxSplits.force.asInt)
        .map(s => Val.Str(pos, s.force.force.asString.reverse)).reverse)
    }
  }

  private object StringChars extends Val.Builtin1("stringChars", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      stringChars(pos, str.force.asString)
  }

  private object ParseInt extends Val.Builtin1("parseInt", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, str.force.asString.toLong)
  }

  private object ParseOctal extends Val.Builtin1("parseOctal", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, java.lang.Long.parseLong(str.force.asString, 8))
  }

  private object ParseHex extends Val.Builtin1("parseHex", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, java.lang.Long.parseLong(str.force.asString, 16))
  }

  private object MD5 extends Val.Builtin1("md5", "s") {
    def evalRhs(s: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Platform.md5(s.force.asString))
  }

  private object AsciiUpper extends Val.Builtin1("asciiUpper", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.toUpperCase)
  }

  private object AsciiLower extends Val.Builtin1("asciiLower", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.toLowerCase)
  }

  private object Trace extends Val.Builtin2("trace", "str", "rest") {
    def evalRhs(str: Lazy, rest: Lazy, ev: EvalScope, pos: Position): Val = {
      System.err.println(s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + str.force.asString)
      rest.force
    }
  }

  private object ExtVar extends Val.Builtin1("extVar", "x") {
    def evalRhs(_x: Lazy, ev: EvalScope, pos: Position): Val = {
      val Val.Str(_, x) = _x
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }
    override def staticSafe = false
  }

  private object ObjectValues extends Val.Builtin1("objectValues", "o") {
    def evalRhs(_o: Lazy, ev: EvalScope, pos: Position): Val = {
      val o = _o.force.asObj
      val keys = getVisibleKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object ObjectValuesAll extends Val.Builtin1("objectValuesAll", "o") {
    def evalRhs(_o: Lazy, ev: EvalScope, pos: Position): Val = {
      val o = _o.force.asObj
      val keys = getAllKeys(ev, o)
      getObjValuesFromKeys(pos, ev, o, keys)
    }
  }

  private object Lines extends Val.Builtin1("lines", "arr") {
    def evalRhs(v1: Lazy, ev: EvalScope, pos: Position): Val = {
    v1.force.asArr.foreach {
      case _: Val.Str | _: Val.Null => // donothing
      case x => Error.fail("Cannot call .lines on " + x.force.prettyName)
    }
    Val.Str(pos, Materializer.apply(v1.force)(ev).asInstanceOf[ujson.Arr]
      .value
      .filter(_ != ujson.Null)
      .map{
        case ujson.Str(s) => s + "\n"
        case _ => ??? /* we ensure it's all strings above */
      }
      .mkString)
    }
  }

  private object Range extends Val.Builtin2("range", "from", "to") {
    def evalRhs(from: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val =
    new Val.Arr(
      pos,
      (from.force.asInt to to.force.asInt).map(i => Val.Num(pos, i)).toArray
    )
  }

  private object ManifestJson extends Val.Builtin1("manifestJson", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v.force, MaterializeJsonRenderer())(ev).toString)
  }

  private object ManifestJsonMinified extends Val.Builtin1("manifestJsonMinified", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v.force, new MaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":"))(ev).toString)
  }

  private object ManifestJsonEx extends Val.Builtin4("manifestJsonEx", "value", "indent", "newline", "key_val_sep", Array(null, null, Val.Str(dummyPos, "\n"), Val.Str(dummyPos, ": "))) {
    def evalRhs(v: Lazy, i: Lazy, newline: Lazy, keyValSep: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer
        .apply0(v.force, MaterializeJsonRenderer(indent = i.force.asString.length, newline = newline.force.asString, keyValueSeparator = keyValSep.force.asString))(ev)
        .toString)
  }

  private object ParseJson extends Val.Builtin1("parseJson", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      ujson.StringParser.transform(str.force.asString, new ValVisitor(pos))
  }

  private object ParseYaml extends Val.Builtin1("parseYaml", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val = {
      try {
        ujson.StringParser.transform(Platform.yamlToJson(str.force.asString), new ValVisitor(pos))
      } catch {
        case _: Exception => null
      }
    }
  }

  private object ManifestTomlEx extends Val.Builtin2("manifestTomlEx", "value", "indent") {
    private def isTableArray(v: Val) = v.force match {
      case s: Val.Arr => s.length > 0 && s.asLazyArray.forall(_.isInstanceOf[Val.Obj])
      case _ => false
    }

    private def isSection(v: Val) = v.force.isInstanceOf[Val.Obj] || isTableArray(v.force)

    private def renderTableInternal(out: StringWriter, v: Val.Obj, cumulatedIndent: String, indent: String, path: Seq[String], indexedPath: Seq[String])(implicit ev : EvalScope): StringWriter = {
      val (sections, nonSections) = v.visibleKeyNames.partition(k => isSection(v.value(k, v.pos)(ev)))
      for (k <- nonSections.sorted) {
        out.write(cumulatedIndent)
        out.write(TomlRenderer.escapeKey(k))
        out.write(" = ")
        Materializer.apply0(v.value(k, v.pos)(ev), new TomlRenderer(out, cumulatedIndent, indent))(ev)
      }
      out.write('\n')

      for (k <- sections.sorted) {
        val v0 = v.value(k, v.pos, v)(ev)
        if (isTableArray(v0)) {
          for (i <- 0 until v0.asArr.length) {
            out.write(cumulatedIndent)
            renderTableArrayHeader(out, path :+ k)
            out.write('\n')
            renderTableInternal(out, v0.asArr.force(i).asObj, cumulatedIndent + indent, indent, path :+ k,
              indexedPath ++ Seq(k, i.toString))
          }
        } else {
          out.write(cumulatedIndent)
          renderTableHeader(out, path :+ k)
          out.write('\n')
          renderTableInternal(out, v0.asObj, cumulatedIndent + indent, indent, path :+ k, indexedPath :+ k)
        }
      }
      out
    }

    private def renderTableHeader(out: StringWriter, path: Seq[String]) = {
      out.write('[')
      out.write(path.map(TomlRenderer.escapeKey).mkString("."))
      out.write(']')
      out
    }

    private def renderTableArrayHeader(out: StringWriter, path: Seq[String]) = {
      out.write('[')
      renderTableHeader(out, path)
      out.write(']')
      out
    }

    def evalRhs(v: Lazy, indent: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter
      renderTableInternal(out, v.force.asObj, "", indent.force.asString, Seq.empty[String], Seq.empty[String])(ev)
      Val.Str(pos, out.toString.strip)
    }
  }

  private object Set_ extends Val.Builtin2("set", "arr", "keyF", Array(null, Val.False(dummyPos))) {
    def evalRhs(arr: Lazy, keyF: Lazy, ev: EvalScope, pos: Position): Val = {
      uniqArr(pos, ev, sortArr(pos, ev, arr.force, keyF.force), keyF.force)
    }
  }

  val functions: Map[String, Val.Func] = Map(
    builtin(AssertEqual),
    builtin(ToString),
    builtin(Codepoint),
    builtin(Length),
    builtin(ObjectHas),
    builtin(ObjectHasAll),
    builtin(ObjectFields),
    builtin(ObjectFieldsAll),
    builtin(ObjectValues),
    builtin(ObjectValuesAll),
    builtin(Type),
    builtin(Lines),
    builtin(Format_),
    builtin(Foldl),
    builtin(Foldr),
    builtin(Range),
    builtin("mergePatch", "target", "patch"){ (pos, ev, target: Val, patch: Val) =>
      val mergePosition = pos
      def createLazyMember(v: => Val) = new Val.Obj.Member(false, Visibility.Normal) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = v
      }
      def recPair(l: Val, r: Val): Val = (l, r) match{
        case (l: Val.Obj, r: Val.Obj) =>
          val keys: Array[String] = distinctKeys(l.visibleKeyNames, r.visibleKeyNames)
          val kvs: Array[(String, Val.Obj.Member)] = new Array[(String, Val.Obj.Member)](keys.length)
          var kvsIdx = 0
          var i = 0
          while (i < keys.length) {
            val key = keys(i)
            val lValue = if (l.containsVisibleKey(key)) l.valueRaw(key, l, pos)(ev) else null
            val rValue = if (r.containsVisibleKey(key)) r.valueRaw(key, r, pos)(ev) else null
            if (!rValue.isInstanceOf[Val.Null]) { // if we are not removing the key
              if (lValue != null && rValue == null) {
                // Preserve the LHS/target value:
                kvs(kvsIdx) = (key, new Val.Obj.ConstMember(false, Visibility.Normal, lValue))
              } else if (lValue.isInstanceOf[Val.Obj] && rValue.isInstanceOf[Val.Obj]) {
                // Recursively merge objects:
                kvs(kvsIdx) = (key, createLazyMember(recPair(lValue, rValue)))
              } else if (rValue != null) {
                // Use the RHS/patch value and recursively remove Null or hidden fields:
                kvs(kvsIdx) = (key, createLazyMember(recSingle(rValue)))
              } else {
                Error.fail("std.mergePatch: This should never happen")
              }
              kvsIdx += 1
            }
            i += 1
          }

          val trimmedKvs = if (kvsIdx == i) kvs else kvs.slice(0, kvsIdx)
          Val.Obj.mk(mergePosition, trimmedKvs)

        case (_, _) => recSingle(r)
      }
      def recSingle(v: Val): Val  = v match {
        case obj: Val.Obj =>
          val keys: Array[String] = obj.visibleKeyNames
          val kvs: Array[(String, Val.Obj.Member)] = new Array[(String, Val.Obj.Member)](keys.length)
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
          // Fallback: Use hash-based deduplication for large RHS arrays:
          (lKeys ++ rKeys).distinct
        }
      }
      recPair(target.force, patch.force)
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
    builtin("round", "x") { (pos, ev, x: Double) =>
      math.round(x)
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
    builtin("isEven", "x"){ (_, _, x: Double) =>
      math.round(x) % 2 == 0
    },
    builtin("isInteger", "x"){ (_, _, x: Double) =>
      math.round(x).toDouble == x
    },
    builtin("isOdd", "x"){ (_, _, x: Double) =>
      math.round(x) % 2 != 0
    },
    builtin("isDecimal", "x"){ (_, _, x: Double) =>
      math.round(x).toDouble != x
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
    builtin("atan2", "x", "y"){ (pos, ev, x: Double, y: Double) =>
      math.atan2(x, y)
    },
    builtin("hypot", "x", "y"){ (pos, ev, x: Double, y: Double) =>
      math.hypot(x, y)
    },
    builtin("deg2rad", "x"){ (pos, ev, x: Double) =>
      math.toRadians(x)
    },
    builtin("rad2deg", "x"){ (pos, ev, x: Double) =>
      math.toDegrees(x)
    },
    builtin("log", "x"){ (pos, ev, x: Double) =>
      math.log(x)
    },
    builtin("log2", "x"){ (pos, ev, x: Double) =>
      // no scala log2, do our best without getting fancy with numerics
      math.log(x) / math.log(2.0)
    },
    builtin("log10", "x"){ (pos, ev, x: Double) =>
      math.log10(x)
    },
    builtin("exp", "x"){ (pos, ev, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (pos, ev, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toLong + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (pos, ev, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toLong + 1
      //val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    builtin(IsString),
    builtin(IsBoolean),
    builtin(IsNumber),
    builtin(IsObject),
    builtin(IsArray),
    builtin(IsFunction),
    builtin(Count),
    builtin(Filter),
    builtin(Map_),
    builtin(MapWithKey),
    builtin(MapWithIndex),
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
                case x => Error.fail("flatMap func must return string, got " + fres.asInstanceOf[Val].force.prettyName)
              }
            )
          }
          Val.Str(pos, builder.toString)
        case _ => Error.fail("Argument must be either array or string")
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
    builtin(Find),
    builtin("findSubstr", "pat", "str") { (pos, ev, pat: String, str: String) =>
      if (pat.length == 0) new Val.Arr(pos, emptyLazyArray)
      else {
        var matchIndex = str.indexOf(pat)
        if (matchIndex == -1) new Val.Arr(pos, emptyLazyArray)
        else {
          val indices = new mutable.ArrayBuilder.ofRef[Val.Num]
          while (0 <= matchIndex && matchIndex < str.length) {
            indices.+=(Val.Num(pos, matchIndex))
            matchIndex = str.indexOf(pat, matchIndex + 1)
          }
          new Val.Arr(pos, indices.result())
        }
      }
    },
    builtin(Substr),
    builtin(StartsWith),
    builtin(EndsWith),
    builtin(Char_),
    builtin(StrReplace),
    builtin(StrReplaceAll),
    builtin(RStripChars),
    builtin(LStripChars),
    builtin(StripChars),
    builtin(Join),
    builtin(Member),

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

    builtin(FlattenArrays),
    builtin(FlattenDeepArrays),
    builtin(DeepJoin),
    builtin(Reverse),

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
      BaseRenderer.escape(out, str, unicode = true)
      out.toString
    },

    builtin("escapeStringXML", "str"){ (_, _, str: String) =>
      val out = new StringWriter()
      for (c <- str) {
        c match {
          case '<' => out.write("&lt;")
          case '>' => out.write("&gt;")
          case '&' => out.write("&amp;")
          case '"' => out.write("&quot;")
          case '\'' => out.write("&apos;")
          case _ => out.write(c)
        }
      }
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
    builtin(ManifestJson),
    builtin(ManifestJsonMinified),
    builtin(ManifestJsonEx),
    builtin("manifestToml", "value"){ (pos, ev, value: Val) =>
      ManifestTomlEx.evalRhs(value, Val.Str(pos, ""), ev, pos)
    },
    builtin(ManifestTomlEx),
    builtinWithDefaults("manifestYamlDoc",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos),
                        "quote_keys" -> Val.True(dummyPos)){ (args, pos, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1)  match {
          case Val.False(_) => false
          case Val.True(_) => true
          case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
        }
      val quoteKeys = args(2) match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => Error.fail("quote_keys has to be a boolean, got " + v.getClass)
      }
      Materializer.apply0(
        v,
        new YamlRenderer(indentArrayInObject = indentArrayInObject, quoteKeys = quoteKeys)
      )(ev).toString
    },
    builtinWithDefaults("manifestYamlStream",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos),
                        "c_document_end" -> Val.True(dummyPos),
                        "quote_keys" -> Val.True(dummyPos)){ (args, _, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1)  match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      val cDocumentEnd = args(2) match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => Error.fail("c_document_end has to be a boolean, got " + v.getClass)
      }
      val quoteKeys = args(3) match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => Error.fail("quote_keys has to be a boolean, got " + v.getClass)
      }
      v match {
        case arr: Val.Arr => arr.asLazyArray
          .map { item =>
            Materializer.apply0(
              item.force,
              new YamlRenderer(indentArrayInObject = indentArrayInObject, quoteKeys = quoteKeys)
            )(ev).toString()
          }
          .mkString("---\n", "\n---\n", if (cDocumentEnd) "\n...\n" else "\n")
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
      v match {
        case Val.Str(_, value) => Base64.getEncoder.encodeToString(value.getBytes)
        case arr: Val.Arr => Base64.getEncoder.encodeToString(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => Error.fail("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (pos, ev, s: String) =>
      new String(Base64.getDecoder.decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (pos, ev, s: String) =>
      new Val.Arr(pos, Base64.getDecoder.decode(s).map(i => Val.Num(pos, i)))
    },

    builtin(EncodeUTF8),
    builtin(DecodeUTF8),

    builtinWithDefaults("uniq", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      uniqArr(pos, ev, args(0), args(1))
    },
    builtinWithDefaults("sort", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      sortArr(pos, ev, args(0), args(1))
    },
    builtin(Set_),
    builtinWithDefaults("setUnion", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val a = toSetArrOrString(args, 0, pos, ev)
      val b = toSetArrOrString(args, 1, pos, ev)
      if (a.isEmpty) {
        uniqArr(pos, ev, sortArr(pos, ev, args(1), args(2)), args(2))
      } else if (b.isEmpty) {
        uniqArr(pos, ev, sortArr(pos, ev, args(0), args(2)), args(2))
      } else {
        val concat = new Val.Arr(pos, a ++ b)
        uniqArr(pos, ev, sortArr(pos, ev, concat, args(2)), args(2))
      }
    },
    builtinWithDefaults("setInter", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val keyF = args(2)
      validateSet(ev, pos, keyF, args(0))
      validateSet(ev, pos, keyF, args(1))

      val a = toSetArrOrString(args, 0, pos, ev)
      val b = toSetArrOrString(args, 1, pos, ev)

      val out = new mutable.ArrayBuffer[Lazy]

      // The intersection will always be, at most, the size of the smallest set.
      val sets = if (b.length < a.length) (b, a) else (a, b)
      for (v <- sets._1) {
        if (existsInSet(ev, pos, keyF, sets._2, v.force) &&
          (ev.settings.strictSetOperations || !existsInSet(ev, pos, keyF, out, v.force))) {
          out.append(v)
        }
      }
      if (ev.settings.strictSetOperations) {
        new Val.Arr(pos, out.toArray)
      } else {
        sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyF)
      }
    },
    builtinWithDefaults("setDiff", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val keyF = args(2)
      validateSet(ev, pos, keyF, args(0))
      validateSet(ev, pos, keyF, args(1))

      val a = toSetArrOrString(args, 0, pos, ev)
      val b = toSetArrOrString(args, 1, pos, ev)
      val out = new mutable.ArrayBuffer[Lazy]

      for (v <- a) {
        if (!existsInSet(ev, pos, keyF, b, v.force) &&
          (ev.settings.strictSetOperations || !existsInSet(ev, pos, keyF, out, v.force))) {
          out.append(v)
        }
      }

      if (ev.settings.strictSetOperations) {
        new Val.Arr(pos, out.toArray)
      } else {
        sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyF)
      }
    },
    builtinWithDefaults("setMember", "x" -> null, "arr" -> null, "keyF" -> Val.False(dummyPos)) { (args, pos, ev) =>
      val keyF = args(2)
      validateSet(ev, pos, keyF, args(1))
      val arr = args(1).asArr.asLazyArray
      existsInSet(ev, pos, keyF, arr, args(0))
    },

    builtin(Split),
    builtin(SplitLimit),
    builtin(SplitLimitR),
    builtin(StringChars),
    builtin(ParseInt),
    builtin(ParseOctal),
    builtin(ParseHex),
    builtin(ParseJson),
    builtin(ParseYaml),
    builtin(MD5),
    builtin("prune", "x"){ (pos, ev, s: Val) =>
      def filter(x: Val) = x match {
        case c: Val.Arr if c.length == 0 => false
        case c: Val.Obj if c.visibleKeyNames.length == 0 => false
        case Val.Null(_) => false
        case _ => true
      }
      def rec(x: Lazy): Val = x.force match {
        case o: Val.Obj =>
          val bindings: Array[(String, Val.Obj.Member)] = for {
            k <- o.visibleKeyNames
            v = rec(o.value(k, pos.fileScope.noOffsetPos)(ev))
            if filter(v)
          } yield (k, new Val.Obj.ConstMember(false, Visibility.Normal, v))
          Val.Obj.mk(pos, bindings)
        case a: Val.Arr =>
          new Val.Arr(pos, a.asLazyArray.map(rec).filter(filter))
        case x => x
      }
      rec(s)
    },

    builtin(AsciiUpper),
    builtin(AsciiLower),
    builtin(Trace),
    builtin(ExtVar),
    builtin(Get),
    builtin(All),
    builtin(Any),
    builtin("isEmpty", "str") { (_, _, str: String) =>
      str.isEmpty
    },
    builtin("trim", "str") { (_, _, str: String) =>
      trailingWhiteSpacePattern.matcher(leadingWhiteSpacePattern.matcher(str).replaceAll("")).replaceAll("")
    },
    builtin("equals", "a", "b") { (_, ev, a: Val, b: Val) =>
      ev.equal(a, b)
    },
    builtin("equalsIgnoreCase", "str1", "str2") { (_, _, str1: String, str2: String) =>
      str1.equalsIgnoreCase(str2)
    },
    builtin("xor", "bool1", "bool2") { (_, _, bool1: Boolean, bool2: Boolean) =>
        bool1 ^ bool2
    },
    builtin("xnor", "bool1", "bool2") { (_, _, bool1: Boolean, bool2: Boolean) =>
        !(bool1 ^ bool2)
    },
    builtin("sha1", "str") { (_, _, str: String) =>
      Platform.sha1(str)
    },
    builtin("sha256", "str") { (_, _, str: String) =>
      Platform.sha256(str)
    },
    builtin("sha512", "str") { (_, _, str: String) =>
      Platform.sha512(str)
    },
    builtin("sha3", "str") { (_, _, str: String) =>
      Platform.sha3(str)
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
      arr.asLazyArray.map(_.force.asDouble).sum/arr.length
    },
    builtin("contains", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      arr.asLazyArray.indexWhere(s => ev.equal(s.force, elem)) != -1
    },
    builtin("remove", "arr", "elem") { (_, ev, arr: Val.Arr, elem: Val) =>
      val idx = arr.asLazyArray.indexWhere(s => ev.equal(s.force, elem))
      if (idx == -1) {
        arr
      } else {
        new Val.Arr(arr.pos, arr.asLazyArray.slice(0, idx) ++ arr.asLazyArray.slice(idx + 1, arr.length))
      }
    },
    builtin("removeAt", "arr", "idx") { (_, _, arr: Val.Arr, idx: Int) =>
      if (!(0 <= idx && idx < arr.length)) {
        Error.fail("index out of bounds: 0 <= " + idx + " < " + arr.length)
      }
      new Val.Arr(arr.pos, arr.asLazyArray.slice(0, idx) ++ arr.asLazyArray.slice(idx + 1, arr.length))
    },
    builtin("objectKeysValues", "o") { (pos, ev, o: Val.Obj) =>
      val keys = getVisibleKeys(ev, o)
      new Val.Arr(pos, keys.map(k => Val.Obj.mk(
        pos.fileScope.noOffsetPos,
        "key" -> new Val.Obj.ConstMember(false, Visibility.Normal, Val.Str(pos.fileScope.noOffsetPos, k)),
        "value" -> new Val.Obj.ConstMember(false, Visibility.Normal, o.value(k, pos.fileScope.noOffsetPos)(ev))
      )))
    },
    builtin("objectKeysValuesAll", "o") { (pos, ev, o: Val.Obj) =>
      val keys = getAllKeys(ev, o)
      new Val.Arr(pos, keys.map(k => Val.Obj.mk(
        pos.fileScope.noOffsetPos,
        "key" -> new Val.Obj.ConstMember(false, Visibility.Normal, Val.Str(pos.fileScope.noOffsetPos, k)),
        "value" -> new Val.Obj.ConstMember(false, Visibility.Normal, o.value(k, pos.fileScope.noOffsetPos)(ev))
      )))
    },
    builtin("objectRemoveKey", "obj", "key") { (pos, ev, o: Val.Obj, key: String) =>
      val bindings: Array[(String, Val.Obj.Member)] = for{
        k <- o.visibleKeyNames
        v = o.value(k, pos.fileScope.noOffsetPos)(ev)
        if k != key
      }yield (k, new Val.Obj.ConstMember(false, Visibility.Normal, v))
      Val.Obj.mk(pos, bindings)
    },
    builtin(MinArray),
    builtin(MaxArray),
    builtin("primitiveEquals", "x", "y") { (_, ev, x: Val, y: Val) =>
      x.isInstanceOf[y.type] && ev.compare(x, y) == 0
    },
    builtin("native", "name") { (pos, ev, name: String) =>
      if (nativeFunctions.contains(name)) {
        nativeFunctions(name)
      } else {
        Error.fail("Native function " + name + " not found", pos)(ev)
      }
    },
  ) ++ builtinNativeFunctions

  private def toSetArrOrString(args: Array[Val], idx: Int, pos: Position, ev: EvalScope) = {
    args(idx) match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str if !ev.settings.strictSetOperations => stringChars(pos, str.value).asLazyArray
      case _ if !ev.settings.strictSetOperations => Error.fail(f"Argument $idx must be either arrays or strings")
      case _ => Error.fail(f"Argument $idx must be an array")
    }
  }

  private def toArrOrString(arg: Val, pos: Position, ev: EvalScope) = {
    arg match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.value).asLazyArray
      case _ => Error.fail(f"Argument must be either arrays or strings")
    }
  }


  private def validateSet(ev: EvalScope, pos: Position, keyF: Val, arr: Val): Unit = {
    if (ev.settings.throwErrorForInvalidSets) {
      val sorted = uniqArr(pos.noOffset, ev, sortArr(pos.noOffset, ev, arr, keyF), keyF)
      if (!ev.equal(arr, sorted)) {
        val err = new Error("Set operation on " + arr.force.prettyName + " was called with a non-set")
        if (ev.settings.strictSetOperations) {
          throw err
        } else {
          ev.warn(err)
        }
      }
    }
  }

  private def existsInSet(ev: EvalScope, pos: Position, keyF: Val, arr: mutable.IndexedSeq[? <: Lazy], toFind: Val): Boolean = {
    val appliedX = keyF match {
      case keyFFunc: Val.Func => keyFFunc.apply1(toFind, pos.noOffset)(ev)
      case _ => toFind
    }
    if (ev.settings.strictSetOperations) {
      arr.search(appliedX.force)((toFind: Lazy, value: Lazy) => {
        val appliedValue = keyF match {
          case keyFFunc: Val.Func => keyFFunc.apply1(value, pos.noOffset)(ev)
          case _ => value
        }
        ev.compare(toFind.force, appliedValue.force)
      }).isInstanceOf[Found]
    } else {
      arr.exists(value => {
        val appliedValue = keyF match {
          case keyFFunc: Val.Func => keyFFunc.apply1(value, pos.noOffset)(ev)
          case _ => value
        }
        ev.equal(appliedValue.force, appliedX.force)
      })
    }
  }

  val Std: Val.Obj = Val.Obj.mk(
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
      ),
      (
        "pi",
        new Val.Obj.Member(false, Visibility.Hidden, cached = false) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev:EvalScope): Val = 
            Val.Num(self.pos, math.Pi)
        }
      )
    ): _*
  )

  private def builtin(obj : Val.Builtin) = (obj.functionName, obj)

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (Position, EvalScope, T1) => R): (String, Val.Func) = {
    (name, new Val.Builtin1(name, p1) {
      def evalRhs(arg1: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Position, EvalScope, T1, T2) => R): (String, Val.Func) = {
    (name, new Val.Builtin2(name, p1, p2) {
      def evalRhs(arg1: Lazy, arg2: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Position, EvalScope, T1, T2, T3) => R): (String, Val.Func) = {
    (name, new Val.Builtin3(name, p1, p2, p3) {
      def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter, T4: ReadWriter]
             (name: String, p1: String, p2: String, p3: String, p4: String)
             (eval: (Position, EvalScope, T1, T2, T3, T4) => R): (String, Val.Func) = {
    (name, new Val.Builtin4(name, p1, p2, p3, p4) {
      def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, arg4: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.force)
        val v4: T4 = implicitly[ReadWriter[T4]].apply(arg4.force)
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
    name -> new Val.Builtin(name, params.map(_._1).toArray, params.map(_._2).toArray) {
      def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val =
        implicitly[ReadWriter[R]].write(pos, eval(args.map(_.force), pos, ev))
    }
  }

  private def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val): Val = {
    val arrValue = toArrOrString(arr, pos, ev)
    if (arrValue.length <= 1) {
      return arr
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
        if (!ev.equal(o1Key, o2Key)) {
          out.append(v)
        }
      }
    }

    new Val.Arr(pos, out.toArray)
  }

  private def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    val vs = toArrOrString(arr, pos, ev)
    if (vs.length <= 1) {
      arr
    } else {
      val keyFFunc = if (keyF == null || keyF.isInstanceOf[Val.False]) null else keyF.asInstanceOf[Val.Func]
      new Val.Arr(pos, if (keyFFunc != null) {
        val keys: Array[Val] = vs.map(v => keyFFunc(Array(v.force), null, pos.noOffset)(ev).force)
        val keyType = keys(0).getClass
        if (classOf[Val.Bool].isAssignableFrom(keyType)) {
          Error.fail("Cannot sort with key values that are booleans")
        }
        if (!keys.forall(_.getClass == keyType)) {
          Error.fail("Cannot sort with key values that are not all the same type")
        }

        val indices = Array.range(0, vs.length)

        val sortedIndices = if (keyType == classOf[Val.Str]) {
          indices.sortBy(i => keys(i).cast[Val.Str].asString)
        } else if (keyType == classOf[Val.Num]) {
          indices.sortBy(i => keys(i).cast[Val.Num].asDouble)
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
          vs.map(_.force.cast[Val.Str]).sortBy(_.asString)
        } else if (keyType == classOf[Val.Num]) {
          vs.map(_.force.cast[Val.Num]).sortBy(_.asDouble)
        } else if (keyType == classOf[Val.Obj]) {
          Error.fail("Unable to sort array of objects without key function")
        } else {
          Error.fail("Cannot sort array of " + vs(0).force.prettyName)
        }
      })
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

  @inline private def maybeSortKeys(ev: EvalScope, keys: Array[String]): Array[String] =
    if(ev.settings.preserveOrder) keys else keys.sorted

  def getObjValuesFromKeys(pos: Position, ev: EvalScope, v1: Val.Obj, keys: Array[String]): Val.Arr =
    new Val.Arr(pos, keys.map { k =>
      (() => v1.value(k, pos.noOffset)(ev)): Lazy
    })
}
