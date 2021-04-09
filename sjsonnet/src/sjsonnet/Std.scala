package sjsonnet

import java.io.StringWriter
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.zip.GZIPOutputStream
import java.util

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{BinaryOp, Params}
import sjsonnet.Val.False

import scala.collection.mutable
import scala.collection.compat._
import scala.util.matching.Regex

import ujson.Value

import scala.util.control.Breaks._

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
object Std {
  private val dummyPos: Position = new Position(null, 0)
  private val emptyLazyArray = new Array[Val.Lazy](0)

  val functions: Seq[(String, Val.Func)] = Seq(
    "assertEqual" -> new Val.Builtin2("a", "b") {
      def evalRhs(v1: Val, v2: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val x1 = Materializer(v1)(ev)
        val x2 = Materializer(v2)(ev)
        if (x1 == x2) Val.True(pos)
        else throw new Error.Delegate("assertEqual failed: " + x1 + " != " + x2)
      }
    },
    "toString" -> new Val.Builtin1("a") {
      def evalRhs(v1: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Str(pos, v1 match {
        case Val.Str(_, s) => s
        case v => Materializer.stringify(v)(ev)
      })
    },
    builtin("codepoint", "str"){ (offset, ev, fs, v1: Val) =>
      v1.cast[Val.Str].value.charAt(0).toInt
    },
    "length" -> new Val.Builtin1("x") {
      def evalRhs(x: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        Val.Num(pos, x match {
          case Val.Str(_, s) => s.length
          case a: Val.Arr => a.length
          case o: Val.Obj => o.visibleKeyNames.length
          case o: Val.Func => o.params.names.length
          case x => throw new Error.Delegate("Cannot get length of " + x.prettyName)
        })
      }
    },
    "objectHas" -> new Val.Builtin2("o", "f") {
      def evalRhs(o: Val, f: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.bool(pos, o.asObj.containsVisibleKey(f.asString))
    },
    "objectHasAll" -> new Val.Builtin2("o", "f") {
      def evalRhs(o: Val, f: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.bool(pos, o.asObj.containsKey(f.asString))
    },
    "objectFields" -> new Val.Builtin1("o") {
      def evalRhs(o: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val keys = getVisibleKeys(ev, o.asObj)
        new Val.Arr(pos, keys.map(k => (() => Val.Str(pos, k)): Val.Lazy))
      }
    },
    "objectFieldsAll" -> new Val.Builtin1("o") {
      def evalRhs(o: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val keys = getAllKeys(ev, o.asObj)
        new Val.Arr(pos, keys.map(k => (() => Val.Str(pos, k)): Val.Lazy))
      }
    },
    builtin("objectValues", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getVisibleKeys(ev, v1)
      getObjValuesFromKeys(pos, ev, fs, v1, keys)
    },
    builtin("objectValuesAll", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getAllKeys(ev, v1)
      getObjValuesFromKeys(pos, ev, fs, v1, keys)
    },
    "type" -> new Val.Builtin1("x") {
      def evalRhs(x: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Str(pos, x match {
        case _: Val.Bool => "boolean"
        case _: Val.Null => "null"
        case _: Val.Obj => "object"
        case _: Val.Arr => "array"
        case _: Val.Func => "function"
        case _: Val.Num => "number"
        case _: Val.Str => "string"
      })
    },
    builtin("lines", "arr"){ (pos, ev, fs, v1: Val.Arr) =>
      v1.foreach{
        case _: Val.Str | _: Val.Null => // donothing
        case x => throw new Error.Delegate("Cannot call .lines on " + x.prettyName)
      }
      Materializer.apply(v1)(ev).asInstanceOf[ujson.Arr]
        .value
        .filter(_ != ujson.Null)
        .map{
          case ujson.Str(s) => s + "\n"
          case _ => ??? /* we ensure it's all strings above */
        }
        .mkString
    },
    "format" -> new Val.Builtin2("str", "vals") {
      def evalRhs(str: Val, vals: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        new Val.Str(pos, Format.format(str.asString, vals, new Position(fs, -1))(ev))
    },

    "foldl" -> new Val.Builtin3("func", "arr", "init") {
      def evalRhs(_func: Val, arr: Val, init: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        var current = init
        for(item <- arr.asArr.asLazyArray){
          val c = current
          current = func.apply2(() => c, item, fs.noOffsetPos)(ev)
        }
        current
      }
    },
    "foldr" -> new Val.Builtin3("func", "arr", "init") {
      def evalRhs(_func: Val, arr: Val, init: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        var current = init
        for(item <- arr.asArr.asLazyArray.reverse){
          val c = current
          current = func.apply2(item, () => c, fs.noOffsetPos)(ev)
        }
        current
      }
    },
    builtin("range", "from", "to"){ (pos, ev, fs, from: Int, to: Int) =>
      new Val.Arr(
        pos,
        (from to to).map(i => (() => Val.Num(pos, i)): Val.Lazy).toArray
      )
    },
    builtin("mergePatch", "target", "patch"){ (pos, ev, fs, target: Val, patch: Val) =>
      val mergePosition = pos
      def createMember(v: => Val) = Val.Obj.Member(false, Visibility.Unhide, (_, _, _, _) => v)
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
    builtin("sqrt", "x"){ (pos, ev, fs, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b"){ (pos, ev, fs, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b"){ (pos, ev, fs, a: Double, b: Double) =>
      math.min(a, b)
    },
    builtin("mod", "a", "b"){ (pos, ev, fs, a: Int, b: Int) =>
      a % b
    },
    builtin("clamp", "x", "minVal", "maxVal"){ (pos, ev, fs, x: Double, minVal: Double, maxVal: Double) =>
      math.max(minVal, math.min(x, maxVal))
    },

    builtin("makeArray", "sz", "func"){ (pos, ev, fs, sz: Int, func: Val.Func) =>
      new Val.Arr(
        pos,
        {
          val a = new Array[Val.Lazy](sz)
          var i = 0
          while(i < sz) {
            val forcedI = i
            a(i) = () => func.apply1(Val.Num(pos, forcedI), fs.noOffsetPos)(ev)
            i += 1
          }
          a
        }
      )
    },

    builtin("pow", "x", "n"){ (pos, ev, fs, x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (pos, ev, fs, x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (pos, ev, fs, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (pos, ev, fs, x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (pos, ev, fs, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (pos, ev, fs, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (pos, ev, fs, x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (pos, ev, fs, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (pos, ev, fs, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (pos, ev, fs, x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (pos, ev, fs, x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (pos, ev, fs, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (pos, ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (pos, ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      //val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    "isString" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Str])
    },
    "isBoolean" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Bool])
    },
    "isNumber" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Num])
    },
    "isObject" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Obj])
    },
    "isArray" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Arr])
    },
    "isFunction" -> new Val.Builtin1("v") {
      def evalRhs(v: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.bool(pos, v.isInstanceOf[Val.Func])
    },
    "count" -> new Val.Builtin2("arr", "x") {
      def evalRhs(arr: Val, x: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        var count = 0
        arr.asArr.foreach(v => if(ev.equal(v, x)) count += 1)
        Val.Num(pos, count)
      }
    },
    "filter" -> new Val.Builtin2("func", "arr") {
      def evalRhs(_func: Val, arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        new Val.Arr(pos, arr.asArr.asLazyArray.filter(v => func.apply1(v, fs.noOffsetPos)(ev).isInstanceOf[Val.True]))
      }
    },
    "map" -> new Val.Builtin2("func", "arr") {
      def evalRhs(_func: Val, arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        new Val.Arr(pos, arr.asArr.asLazyArray.map(v => (() => func.apply1(v, fs.noOffsetPos)(ev)): Val.Lazy))
      }
    },
    "mapWithKey" -> new Val.Builtin2("func", "obj") {
      def evalRhs(_func: Val, _obj: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        val obj = _obj.asObj
        val allKeys = obj.allKeyNames
        val m = new util.LinkedHashMap[String, Val.Obj.Member]()
        var i = 0
        while(i < allKeys.length) {
          val k = allKeys(i)
          val v = Val.Obj.Member(false, Visibility.Normal,
            (_, _, _, _) =>
              func.apply2(() => Val.Str(pos, k), () => obj.value(k, fs.noOffsetPos)(ev), fs.noOffsetPos)(ev)
          )
          m.put(k, v)
          i += 1
        }
        new Val.Obj(pos, m, false, null, null)
      }
    },
    "mapWithIndex" -> new Val.Builtin2("func", "arr") {
      def evalRhs(_func: Val, _arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val func = _func.asFunc
        val arr = _arr.asArr.asLazyArray
        val a = new Array[Val.Lazy](arr.length)
        var i = 0
        while(i < a.length) {
          val x = arr(i)
          val idx = Val.Num(pos, i)
          a(i) = () => func.apply2(() => idx, x, fs.noOffsetPos)(ev)
          i += 1
        }
        new Val.Arr(pos, a)
      }
    },
    builtin("flatMap", "func", "arr"){ (pos, ev, fs, func: Val.Func, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val arrResults = a.asLazyArray.flatMap {
            v => {
              val fres = func.apply1(v, fs.noOffsetPos)(ev)
              fres match {
                case va: Val.Arr => va.asLazyArray
                case unknown => throw new Error.Delegate("flatMap func must return an array, not " + unknown)
              }
            }
          }
          new Val.Arr(pos, arrResults)

        case s: Val.Str =>
          val builder = new StringBuilder()
          for (c: Char <- s.value) {
            val fres = func.apply1(Val.Str(pos, c.toString), fs.noOffsetPos)(ev)
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.value
                case _: Val.Null => ""
                case x => throw Error.Delegate("flatMap func must return string, got " + fres.asInstanceOf[Val].prettyName)
              }
            )
          }
          Val.Str(pos, builder.toString)
      }
      res
    },

    builtin("filterMap", "filter_func", "map_func", "arr"){ (pos, ev, fs, filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
      new Val.Arr(
        pos,
        arr.asLazyArray.flatMap { i =>
          i.force
          if (!filter_func.apply1(i, fs.noOffsetPos)(ev).isInstanceOf[Val.True]) None
          else Some[Val.Lazy](() => map_func.apply1(i, fs.noOffsetPos)(ev))
        }
      )
    },
    "find" -> new Val.Builtin2("value", "arr") {
      def evalRhs(value: Val, _arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val arr = _arr.asArr
        val b = new mutable.ArrayBuilder.ofRef[Val.Lazy]
        var i = 0
        while(i < arr.length) {
          if(ev.equal(arr.force(i), value)) {
            val finalI = i
            b.+=(() => Val.Num(pos, finalI))
          }
          i += 1
        }
        new Val.Arr(pos, b.result())
      }
    },
    builtin("findSubstr", "pat", "str") { (pos, ev, fs, pat: String, str: String) =>
      if (pat.length == 0) new Val.Arr(pos, emptyLazyArray)
      else {
        val indices = mutable.ArrayBuffer[Int]()
        var matchIndex = str.indexOf(pat)
        while (0 <= matchIndex && matchIndex < str.length) {
          indices.append(matchIndex)
          matchIndex = str.indexOf(pat, matchIndex + 1)
        }
        new Val.Arr(pos, indices.map(x => (() => Val.Num(pos, x)): Val.Lazy).toArray)
      }
    },
    "substr" -> new Val.Builtin3("s", "from", "len") {
      def evalRhs(_s: Val, from: Val, len: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val s = _s.asString
        val safeOffset = math.min(from.asInt, s.length)
        val safeLength = math.min(len.asInt, s.length - safeOffset)
        Val.Str(pos, s.substring(safeOffset, safeOffset + safeLength))
      }
    },
    "startsWith" -> new Val.Builtin2("a", "b") {
      def evalRhs(a: Val, b: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.bool(pos, a.asString.startsWith(b.asString))
    },
    "endsWith" -> new Val.Builtin2("a", "b") {
      def evalRhs(a: Val, b: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.bool(pos, a.asString.endsWith(b.asString))
    },
    builtin("char", "n"){ (pos, ev, fs, n: Double) =>
      n.toInt.toChar.toString
    },

    "strReplace" -> new Val.Builtin3("str", "from", "to") {
      def evalRhs(str: Val, from: Val, to: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Str(pos, str.asString.replace(from.asString, to.asString))
    },
    "strReplaceAll" -> new Val.Builtin3("str", "from", "to") {
      def evalRhs(str: Val, from: Val, to: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Str(pos, str.asString.replaceAll(from.asString, to.asString))
    },

    builtin("rstripChars", "str", "chars"){ (pos, ev, fs, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "")
    },
    builtin("lstripChars", "str", "chars"){ (pos, ev, fs, str: String, chars: String) =>
      str.replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },
    builtin("stripChars", "str", "chars"){ (pos, ev, fs, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "").replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },

    "join" -> new Val.Builtin2("sep", "arr") {
      def evalRhs(sep: Val, _arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
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
                case x => throw new Error.Delegate("Cannot join " + x.prettyName)
              }
              i += 1
            }
            Val.Str(pos, b.toString)
          case sep: Val.Arr =>
            val out = new mutable.ArrayBuffer[Val.Lazy]
            var added = false
            for(x <- arr){
              x match{
                case Val.Null(_) => // do nothing
                case v: Val.Arr =>
                  if (added) out.appendAll(sep.asLazyArray)
                  added = true
                  out.appendAll(v.asLazyArray)
                case x => throw new Error.Delegate("Cannot join " + x.prettyName)
              }
            }
            new Val.Arr(pos, out.toArray)
          case x => throw new Error.Delegate("Cannot join " + x.prettyName)
        }
      }
    },

    "member" -> new Val.Builtin2("arr", "x") {
      def evalRhs(arr: Val, x: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        Val.bool(pos, arr match {
          case str: Val.Str =>
            val secondArg = x match {
              case Val.Str(_, value) => value
              case n => throw new Error.Delegate("std.member second argument must be a string, got " + x.prettyName)
            }
            str.value.contains(secondArg)
          case a: Val.Arr =>
            var c = 0
            a.foreach(v => if(ev.equal(v, x)) c += 1)
            c > 0
          case x => throw new Error.Delegate("std.member first argument must be an array or a string, got " + arr.prettyName)
        })
      }
    },

    builtin("repeat", "what", "count"){ (pos, ev, fs, what: Val, count: Int) =>
      val res: Val = what match {
        case str: Val.Str =>
          val builder = new StringBuilder
          for (i <- 1 to count) {
            builder.append(str.value)
          }
          Val.Str(pos, builder.toString())
        case a: Val.Arr =>
          val out = new mutable.ArrayBuffer[Val.Lazy]
          for (i <- 1 to count) {
            out.appendAll(a.asLazyArray)
          }
          new Val.Arr(pos, out.toArray)
        case x => throw new Error.Delegate("std.repeat first argument must be an array or a string")
      }
      res
    },

    "flattenArrays" -> new Val.Builtin1("arrs") {
      def evalRhs(arrs: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val out = new mutable.ArrayBuffer[Val.Lazy]
        for(x <- arrs.asArr) {
          x match{
            case Val.Null(_) => // do nothing
            case v: Val.Arr => out.appendAll(v.asLazyArray)
            case x => throw new Error.Delegate("Cannot call flattenArrays on " + x)
          }
        }
        new Val.Arr(pos, out.toArray)
      }
    },

    builtin("manifestIni", "v"){ (pos, ev, fs, v: Val) =>
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
    builtin("escapeStringJson", "str"){ (pos, ev, fs, str: String) =>
      val out = new StringWriter()
      BaseRenderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (pos, ev, fs, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (pos, ev, fs, str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (pos, ev, fs, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    builtin("manifestJson", "v"){ (pos, ev, fs, v: Val) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = 4))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtin("manifestJsonEx", "value", "indent"){ (pos, ev, fs, v: Val, i: String) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJsonEx
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = i.length))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtinWithDefaults("manifestYamlDoc",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos)){ (pos, args, fs, ev) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
          case Val.False(_) => false
          case Val.True(_) => true
          case _ => throw Error.Delegate("indent_array_in_object has to be a boolean, got" + v.getClass)
        }
      Materializer.apply0(
        v,
        new YamlRenderer(indentArrayInObject = indentArrayInObject)
      )(ev).toString
    },
    builtinWithDefaults("manifestYamlStream",
                        "v" -> null,
                        "indent_array_in_object" -> Val.False(dummyPos)){ (pos, args, fs, ev) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => throw Error.Delegate("indent_array_in_object has to be a boolean, got" + v.getClass)
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
        case _ => throw new Error.Delegate("manifestYamlStream only takes arrays, got " + v.getClass)
      }
    },
    builtin("manifestPythonVars", "v"){ (pos, ev, fs, v: Val.Obj) =>
      Materializer(v)(ev).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (pos, ev, fs, value: Val) =>
      import scalatags.Text.all.{value => _, _}


      def rec(v: ujson.Value): Frag = {
        v match {
          case ujson.Str(s) => s
          case ujson.Arr(mutable.Seq(ujson.Str(t), attrs: ujson.Obj, children@_*)) =>
            tag(t)(
              attrs.value.map {
                case (k, ujson.Str(v)) => attr(k) := v
                case (k, v) => throw new Error.Delegate("Cannot call manifestXmlJsonml on " + v.getClass)
              }.toSeq,
              children.map(rec)
            )
          case ujson.Arr(mutable.Seq(ujson.Str(t), children@_*)) =>
            tag(t)(children.map(rec).toSeq)
          case x =>
            throw new Error.Delegate("Cannot call manifestXmlJsonml on " + x.getClass)
        }
      }

      rec(Materializer(value)(ev)).render

    },
    builtin("base64", "v"){ (pos, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Base64.getEncoder().encodeToString(value.getBytes)
        case arr: Val.Arr => Base64.getEncoder().encodeToString(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (pos, ev, fs, s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (pos, ev, fs, s: String) =>
      new Val.Arr(pos, Base64.getDecoder().decode(s).map(i => (() => Val.Num(pos, i)): Val.Lazy))
    },

    builtin("gzip", "v"){ (pos, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.gzipString(value)
        case arr: Val.Arr => Platform.gzipBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot gzip encode " + x.prettyName)
      }
    },

    builtin("xz", "v"){ (pos, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.xzString(value)
        case arr: Val.Arr => Platform.xzBytes(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot xz encode " + x.prettyName)
      }
    },

    "encodeUTF8" -> new Val.Builtin1("s") {
      def evalRhs(s: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        new Val.Arr(pos, s.asString.getBytes(UTF_8).map(i => (() => Val.Num(pos, i & 0xff)): Val.Lazy))
    },
    "decodeUTF8" -> new Val.Builtin1("arr") {
      def evalRhs(arr: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        new Val.Str(pos, new String(arr.asArr.iterator.map(_.cast[Val.Num].value.toByte).toArray, UTF_8))
    },

    builtinWithDefaults("uniq", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      val arr = args("arr")
      val keyF = args("keyF")

      uniqArr(pos, ev, arr, keyF, fs)
    },
    builtinWithDefaults("sort", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      val arr = args("arr")
      val keyF = args("keyF")

      sortArr(pos, ev, arr, keyF, fs)
    },

    builtinWithDefaults("set", "arr" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      uniqArr(pos, ev, sortArr(pos, ev, args("arr"), args("keyF"), fs), args("keyF"), fs)
    },
    builtinWithDefaults("setUnion", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      val a = args("a") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val concat = new Val.Arr(pos, a ++ b)
      uniqArr(pos, ev, sortArr(pos, ev, concat, args("keyF"), fs), args("keyF"), fs)
    },
    builtinWithDefaults("setInter", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      val a = args("a") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val keyF = args("keyF")
      val out = new mutable.ArrayBuffer[Val.Lazy]

      for (v <- a) {
        if (keyF.isInstanceOf[Val.False]) {
          val vf = v.force
          if (b.exists(value => {
            ev.equal(value.force, vf)
          }) && !out.exists(value => {
            ev.equal(value.force, vf)
          })) {
            out.append(v)
          }
        } else {
          val keyFFunc = keyF.asInstanceOf[Val.Func]
          val appliedX = keyFFunc.apply1(v, fs.noOffsetPos)(ev)

          if (b.exists(value => {
            val appliedValue = keyFFunc.apply1(value, fs.noOffsetPos)(ev)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFFunc.apply1(value, fs.noOffsetPos)(ev)
            ev.equal(mValue, appliedX)
          })) {
            out.append(v)
          }
        }
      }

      sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyF, fs)
    },
    builtinWithDefaults("setDiff", "a" -> null, "b" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>

      val a = args("a") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.asLazyArray
        case str: Val.Str => stringChars(pos, str.value).asLazyArray
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val keyF = args("keyF")
      val out = new mutable.ArrayBuffer[Val.Lazy]

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
          val appliedX = keyFFunc.apply1(v, fs.noOffsetPos)(ev)

          if (!b.exists(value => {
            val appliedValue = keyFFunc.apply1(value, fs.noOffsetPos)(ev)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFFunc.apply1(value, fs.noOffsetPos)(ev)
            ev.equal(mValue, appliedX)
          })) {
            out.append(v)
          }
        }
      }

      sortArr(pos, ev, new Val.Arr(pos, out.toArray), keyF, fs)
    },
    builtinWithDefaults("setMember", "x" -> null, "arr" -> null, "keyF" -> Val.False(dummyPos)) { (pos, args, fs, ev) =>
      val keyF = args("keyF")

      if (keyF.isInstanceOf[Val.False]) {
        val ujson.Arr(mArr) = Materializer(args("arr"))(ev)
        val mx = Materializer(args("x"))(ev)
        mArr.contains(mx)
      } else {
        val arr = args("arr").asInstanceOf[Val.Arr].asLazyArray
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val appliedX = keyFFunc.apply1(args("x"), fs.noOffsetPos)(ev)
        arr.exists(value => {
          val appliedValue = keyFFunc.apply1(value, fs.noOffsetPos)(ev)
          ev.equal(appliedValue, appliedX)
        })
      }
    },

    "split" -> new Val.Builtin2("str", "c") {
      def evalRhs(_str: Val, _c: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val str = _str.asString
        val cStr = _c.asString
        if(cStr.length != 1) throw new Error.Delegate("std.split second parameter should have length 1, got "+cStr.length)
        val c = cStr.charAt(0)
        val b = new mutable.ArrayBuilder.ofRef[Val.Lazy]
        var i = 0
        var start = 0
        while(i < str.length) {
          if(str.charAt(i) == c) {
            val finalStr = Val.Str(pos, str.substring(start, i))
            b.+=(() => finalStr)
            start = i+1
          }
          i += 1
        }
        b.+=(() => Val.Str(pos, str.substring(start, math.min(i, str.length))))
        new Val.Arr(pos, b.result())
      }
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ (pos, ev, fs, str: String, c: String, maxSplits: Int) =>
      new Val.Arr(pos, str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => (() => Val.Str(pos, s)): Val.Lazy))
    },
    "stringChars" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        stringChars(pos, str.asString)
    },
    "parseInt" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Num(pos, str.asString.toInt)
    },
    "parseOctal" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Num(pos, Integer.parseInt(str.asString, 8))
    },
    "parseHex" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Num(pos, Integer.parseInt(str.asString, 16))
    },
    builtin("parseJson", "str") { (pos, ev, fs, str: String) =>
      def recursiveTransform(js: ujson.Value): Val = {
        js match {
          case ujson.Null => Val.Null(pos)
          case ujson.True => Val.True(pos)
          case ujson.False => Val.False(pos)
          case ujson.Num(value) => Val.Num(pos, value)
          case ujson.Str(value) => Val.Str(pos, value)
          case ujson.Arr(values) =>
            val transformedValue: Array[Val.Lazy] = values.map(v => (() => recursiveTransform(v)): Val.Lazy).toArray
            new Val.Arr(pos, transformedValue)
          case ujson.Obj(valueMap) =>
            val m = new util.LinkedHashMap[String, Val.Obj.Member]
            valueMap.foreach { case (k, v) =>
              m.put(k, Val.Obj.Member(false, Expr.Member.Visibility.Normal, (_, _, _, _) => recursiveTransform(v)))
            }
            new Val.Obj(pos, m, false, null, null)
        }
      }
      recursiveTransform(ujson.read(str))
    },
    "md5" -> new Val.Builtin1("s") {
      def evalRhs(s: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Str(pos, Platform.md5(s.asString))
    },
    builtin("prune", "x"){ (pos, ev, fs, s: Val) =>
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
          }yield (k, Val.Obj.Member(false, Visibility.Normal, (_, _, _, _) => v))
          Val.Obj.mk(pos, bindings: _*)
        case a: Val.Arr =>
          new Val.Arr(pos, a.asStrictArray.map(rec).filter(filter).map(x => (() => x): Val.Lazy))
        case _ => x
      }
      rec(s)
    },

    "asciiUpper" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Str(pos, str.asString.toUpperCase)
    },
    "asciiLower" -> new Val.Builtin1("str") {
      def evalRhs(str: Val, ev: EvalScope, fs: FileScope, pos: Position): Val =
        Val.Str(pos, str.asString.toLowerCase)
    },

    "trace" -> new Val.Builtin2("str", "rest") {
      def evalRhs(_str: Val, rest: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val Val.Str(_, msg) = _str
        System.err.println(s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + msg)
        rest
      }
    },

    "extVar" -> new Val.Builtin1("x") {
      def evalRhs(_x: Val, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val Val.Str(_, x) = _x
        Materializer.reverse(
          pos,
          ev.extVars.getOrElse(
            x,
            throw new Error.Delegate("Unknown extVar: " + x)
          )
        )
      }
    }
  )
  val Std = Val.Obj.mk(
    null,
    functions
      .map{
        case (k, v) =>
          (
            k,
            Val.Obj.Member(
              false,
              Visibility.Hidden,
              (self: Val.Obj, sup: Val.Obj, _, _) => v
            )
          )
      } ++ Seq(
      (
        "thisFile",
        Val.Obj.Member(
          false,
          Visibility.Hidden,
          { (self: Val.Obj, sup: Val.Obj, fs: FileScope, eval: EvalScope) =>
            Val.Str(self.pos, fs.currentFile.relativeToString(eval.wd))
          },
          cached = false
        )
      )
    ): _*
  )

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (Position, EvalScope, FileScope, T1) => R): (String, Val.Func) = {
    (name, new Val.Builtin1(p1) {
      def evalRhs(arg1: Val, ev: EvalScope, fs: FileScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Position, EvalScope, FileScope, T1, T2) => R): (String, Val.Func) = {
    (name, new Val.Builtin2(p1, p2) {
      def evalRhs(arg1: Val, arg2: Val, ev: EvalScope, fs: FileScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1, v2))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Position, EvalScope, FileScope, T1, T2, T3) => R): (String, Val.Func) = {
    (name, new Val.Builtin(p1, p2, p3) {
      def evalRhs(args: Array[Val], ev: EvalScope, fs: FileScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(args(0))
        val v2: T2 = implicitly[ReadWriter[T2]].apply(args(1))
        val v3: T3 = implicitly[ReadWriter[T3]].apply(args(2))
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1, v2, v3))
      }
    })
  }

  /**
    * Helper function that can define a built-in function with default parameters
    *
    * Arguments of the eval function are (args, ev)
    */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Val.Literal)*)
                                        (eval: (Position, scala.collection.Map[String, Val], FileScope, EvalScope) => R): (String, Val.Func) = {
    val indexedParams = params.zipWithIndex.map{case ((k, v), i) => (k, v, i)}.toArray
    val indexedParamKeys = params.zipWithIndex.map{case ((k, v), i) => (k, i)}
    name -> new Val.Func(null, null, Params.mk(indexedParams: _*)) {
      def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = {
        val args = indexedParamKeys.map {case (k, i) => k -> scope.bindings(i).force }.toMap
        implicitly[ReadWriter[R]].write(pos, eval(pos, args, fs, ev))
      }
      override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = expr.asInstanceOf[Val]
    }
  }

  def scope(size: Int) = {
    new ValScope(
      null, null, null, Array[Val.Lazy](() => Std).padTo(size, null)
    )
  }

  def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val, fs: FileScope) = {
    val arrValue = arr match {
      case arr: Val.Arr => arr.asLazyArray
      case str: Val.Str => stringChars(pos, str.value).asLazyArray
      case _ => throw new Error.Delegate("Argument must be either array or string")
    }

    val out = new mutable.ArrayBuffer[Val.Lazy]
    for (v <- arrValue) {
      if (out.isEmpty) {
        out.append(v)
      } else if (keyF.isInstanceOf[Val.False]) {
        if (!ev.equal(out.last.force, v.force)) {
          out.append(v)
        }
      } else if (!keyF.isInstanceOf[Val.False]) {
        val keyFFunc = keyF.asInstanceOf[Val.Func]

        val o1Key = keyFFunc.apply1(v, fs.noOffsetPos)(ev)
        val o2Key = keyFFunc.apply1(out.last, fs.noOffsetPos)(ev)
        val o1KeyExpr = Materializer.toExpr(Materializer.apply(o1Key)(ev))(ev)
        val o2KeyExpr = Materializer.toExpr(Materializer.apply(o2Key)(ev))(ev)

        val comparisonExpr = Expr.BinaryOp(dummyPos, o1KeyExpr, BinaryOp.`!=`, o2KeyExpr)
        val exprResult = ev.visitExpr(comparisonExpr)(scope(0))

        val res = Materializer.apply(exprResult)(ev).asInstanceOf[ujson.Bool]

        if (res.value) {
          out.append(v)
        }
      }
    }

    new Val.Arr(pos, out.toArray)
  }

  def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val, fs: FileScope) = {
    arr match{
      case vs: Val.Arr =>
        new Val.Arr(
          pos,

          if (vs.forall(_.isInstanceOf[Val.Str])){
            vs.asStrictArray.map(_.cast[Val.Str]).sortBy(_.value).map(x => (() => x): Val.Lazy)
          }else if (vs.forall(_.isInstanceOf[Val.Num])) {
            vs.asStrictArray.map(_.cast[Val.Num]).sortBy(_.value).map(x => (() => x): Val.Lazy)
          }else if (vs.forall(_.isInstanceOf[Val.Obj])){
            if (keyF.isInstanceOf[Val.False]) {
              throw new Error.Delegate("Unable to sort array of objects without key function")
            } else {
              val objs = vs.asStrictArray.map(_.cast[Val.Obj])

              val keyFFunc = keyF.asInstanceOf[Val.Func]
              val keys = objs.map((v) => keyFFunc(null, Array((() => v): Val.Lazy), fs.noOffsetPos)(ev))

              if (keys.forall(_.isInstanceOf[Val.Str])){
                objs.sortBy((v) => keyFFunc(null, Array((() => v): Val.Lazy), fs.noOffsetPos)(ev).cast[Val.Str].value).map(x => (() => x): Val.Lazy)
              } else if (keys.forall(_.isInstanceOf[Val.Num])) {
                objs.sortBy((v) => keyFFunc(null, Array((() => v): Val.Lazy), fs.noOffsetPos)(ev).cast[Val.Num].value).map(x => (() => x): Val.Lazy)
              } else {
                throw new Error.Delegate("Cannot sort with key values that are " + keys(0).prettyName + "s")
              }
            }
          }else {
            ???
          }
        )
      case Val.Str(pos, s) => new Val.Arr(pos, s.sorted.map(c => (() => Val.Str(pos, c.toString)): Val.Lazy).toArray)
      case x => throw new Error.Delegate("Cannot sort " + x.prettyName)
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    val a = new Array[Val.Lazy](str.length)
    var i = 0
    while(i < a.length) {
      val bound = Val.Str(pos, String.valueOf(str.charAt(i)))
      a(i) = () => bound
      i += 1
    }
    new Val.Arr(pos, a)
  }
  
  def getVisibleKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.visibleKeyNames)

  def getAllKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.allKeyNames)
  
  @inline private[this] def maybeSortKeys(ev: EvalScope, keys: Array[String]): Array[String] = {
    if(ev.preserveOrder) {
      keys
    } else {
      keys.sorted
    }
  }
  
  def getObjValuesFromKeys(pos: Position, ev: EvalScope, fs: FileScope, v1: Val.Obj, keys: Array[String]): Val.Arr = {
    new Val.Arr(pos, keys.map { k =>
      (() => v1.value(k, fs.noOffsetPos)(ev)): Val.Lazy
    })
  }
}
