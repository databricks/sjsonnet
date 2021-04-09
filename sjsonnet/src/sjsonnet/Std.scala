package sjsonnet

import java.io.StringWriter
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.zip.GZIPOutputStream

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{BinaryOp, Params}
import sjsonnet.Val.False

import scala.collection.mutable
import scala.collection.compat._
import scala.util.matching.Regex

import ujson.Value

import util.control.Breaks._

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
object Std {
  private val dummyPos: Position = new Position(null, 0)
  private val emptyLazyArray = new Array[Val.Lazy](0)

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin("assertEqual", "a", "b"){ (offset, ev, fs, v1: Val, v2: Val) =>
      val x1 = Materializer(v1)(ev)
      val x2 = Materializer(v2)(ev)
      if (x1 == x2) true
      else throw new Error.Delegate("assertEqual failed: " + x1 + " != " + x2)
    },
    builtin("toString", "a"){ (offset, ev, fs, v1: Val) =>
      v1 match{
        case Val.Str(_, s) => s
        case v => Materializer.stringify(v)(ev)
      }
    },
    builtin("codepoint", "str"){ (offset, ev, fs, v1: Val) =>
      v1.cast[Val.Str].value.charAt(0).toInt
    },
    "length" -> Val.Func(null, null, Params.mk(("x", null, 0)), { (scope, ev, fs, pos) =>
      val x = scope.bindings(0).force
      Val.Num(pos, x match{
        case Val.Str(_, s) => s.length
        case a: Val.Arr => a.length
        case o: Val.Obj => o.visibleKeyNames.length
        case o: Val.Func => o.params.names.length
        case _ => throw new Error.Delegate("Cannot get length of " + x.prettyName)
      })
    }),
    "objectHas" -> Val.Func(null, null, Params.mk(("o", null, 0), ("f", null, 1)), { (scope, ev, fs, pos) =>
      val o = implicitly[ReadWriter[Val.Obj]].apply(scope.bindings(0).force, ev, fs)
      val f = implicitly[ReadWriter[String]].apply(scope.bindings(1).force, ev, fs)
      Val.bool(pos, o.containsVisibleKey(f))
    }),
    "objectHasAll" -> Val.Func(null, null, Params.mk(("o", null, 0), ("f", null, 1)), { (scope, ev, fs, pos) =>
      val o = implicitly[ReadWriter[Val.Obj]].apply(scope.bindings(0).force, ev, fs)
      val f = implicitly[ReadWriter[String]].apply(scope.bindings(1).force, ev, fs)
      Val.bool(pos, o.containsKey(f))
    }),
    builtin("objectFields", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getVisibleKeys(ev, v1)
      new Val.Arr(pos, keys.map(k => (() => Val.Str(pos, k)): Val.Lazy))
    },
    builtin("objectFieldsAll", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getAllKeys(ev, v1)
      new Val.Arr(pos, keys.map(k => (() => Val.Str(pos, k)): Val.Lazy))
    },
    builtin("objectValues", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getVisibleKeys(ev, v1)
      getObjValuesFromKeys(pos, ev, fs, v1, keys)
    },
    builtin("objectValuesAll", "o"){ (pos, ev, fs, v1: Val.Obj) =>
      val keys = getAllKeys(ev, v1)
      getObjValuesFromKeys(pos, ev, fs, v1, keys)
    },
    builtin("type", "x"){ (pos, ev, fs, v1: Val) =>
      v1 match{
        case Val.True(_) | Val.False(_) => "boolean"
        case Val.Null(_) => "null"
        case _: Val.Obj => "object"
        case _: Val.Arr => "array"
        case _: Val.Func => "function"
        case _: Val.Num => "number"
        case _: Val.Str => "string"
      }
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
    builtin("format", "str", "vals"){ (pos, ev, fs, v1: String, v2: Val) =>
      Format.format(v1, v2, new Position(fs, -1))(ev)
    },
    builtin("foldl", "func", "arr", "init"){ (pos, ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.asLazyArray){
        val c = current
        current = func.apply(() => c, item)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ (pos, ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.asLazyArray.reverse){
        val c = current
        current = func.apply(item, () => c)
      }
      current
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

    builtin("makeArray", "sz", "func"){ (pos, ev, fs, sz: Int, func: Applyer) =>
      new Val.Arr(
        pos,
        {
          val a = new Array[Val.Lazy](sz)
          var i = 0
          while(i < sz) {
            val forcedI = i
            a(i) = () => func.apply(() => Val.Num(pos, forcedI))
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
    builtin("isString", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Str]
    },
    builtin("isBoolean", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.True] || v.isInstanceOf[Val.False]
    },
    builtin("isNumber", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Num]
    },
    builtin("isObject", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Obj]
    },
    builtin("isArray", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Arr]
    },
    builtin("isFunction", "v"){ (pos, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Func]
    },
    "count" -> Val.Func(null, null, Params.mk(("arr", null, 0), ("x", null, 1)), { (scope, ev, fs, pos) =>
      val arr = implicitly[ReadWriter[Val.Arr]].apply(scope.bindings(0).force, ev, fs)
      val x = scope.bindings(1).force
      var count = 0
      arr.foreach(v => if(ev.equal(v, x)) count += 1)
      Val.Num(pos, count)
    }),
    "filter" -> Val.Func(null, null, Params.mk(("func", null, 0), ("arr", null, 1)), { (scope, ev, fs, pos) =>
      val func = implicitly[ReadWriter[Applyer]].apply(scope.bindings(0).force, ev, fs)
      val arr = implicitly[ReadWriter[Val.Arr]].apply(scope.bindings(1).force, ev, fs)
      new Val.Arr(pos, arr.asLazyArray.filter(v => func.apply(v).isInstanceOf[Val.True]))
    }),
    "map" -> Val.Func(null, null, Params.mk(("func", null, 0), ("arr", null, 1)), { (scope, ev, fs, pos) =>
      val func = implicitly[ReadWriter[Applyer]].apply(scope.bindings(0).force, ev, fs)
      val arr = implicitly[ReadWriter[Val.Arr]].apply(scope.bindings(1).force, ev, fs)
      new Val.Arr(pos, arr.asLazyArray.map(v => (() => func.apply(v)): Val.Lazy))
    }),
    builtin("mapWithKey", "func", "obj"){ (pos, ev, fs, func: Applyer, obj: Val.Obj) =>
      val allKeys = obj.allKeyNames
      Val.Obj.mk(
        pos,
        allKeys.map{ k =>
          k -> (Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Val.Obj, _, _) =>
            func.apply(
              () => Val.Str(pos, k),
              () => obj.value(k, fs.noOffsetPos)(ev)
            )
          ))
        }: _*
      )
    },
    builtin("mapWithIndex", "func", "arr"){ (pos, ev, fs, func: Applyer, arr: Val.Arr) =>
      new Val.Arr(
        pos,
        arr.asLazyArray.zipWithIndex.map{ case (x, i) =>
          (() => func.apply(() => Val.Num(pos, i), x)): Val.Lazy
        }
      )
    },
    builtin("flatMap", "func", "arr"){ (pos, ev, fs, func: Applyer, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val arrResults = a.asLazyArray.flatMap {
            v => {
              val fres = func.apply(v)
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
            val fres = func.apply(() => Val.Str(pos, c.toString))
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

    builtin("filterMap", "filter_func", "map_func", "arr"){ (pos, ev, fs, filter_func: Applyer, map_func: Applyer, arr: Val.Arr) =>
      new Val.Arr(
        pos,
        arr.asLazyArray.flatMap { i =>
          val x = i.force
          if (!filter_func.apply(() => x).isInstanceOf[Val.True]) None
          else Some[Val.Lazy](() => map_func.apply(() => x))
        }
      )
    },
    "find" -> Val.Func(null, null, Params.mk(("value", null, 0), ("arr", null, 1)), { (scope, ev, fs, pos) =>
      val value = scope.bindings(0).force
      val arr = implicitly[ReadWriter[Val.Arr]].apply(scope.bindings(1).force, ev, fs)
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
    }),
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
    builtin("substr", "s", "from", "len"){ (pos, ev, fs, s: String, from: Int, len: Int) =>
      val safeOffset = math.min(from, s.length)
      val safeLength = math.min(len, s.length - safeOffset)
      s.substring(safeOffset, safeOffset + safeLength)
    },
    "startsWith" -> Val.Func(null, null, Params.mk(("a", null, 0), ("b", null, 1)), { (scope, ev, fs, pos) =>
      val a = implicitly[ReadWriter[String]].apply(scope.bindings(0).force, ev, fs)
      val b = implicitly[ReadWriter[String]].apply(scope.bindings(1).force, ev, fs)
      Val.bool(pos, a.startsWith(b))
    }),
    "endsWith" -> Val.Func(null, null, Params.mk(("a", null, 0), ("b", null, 1)), { (scope, ev, fs, pos) =>
      val a = implicitly[ReadWriter[String]].apply(scope.bindings(0).force, ev, fs)
      val b = implicitly[ReadWriter[String]].apply(scope.bindings(1).force, ev, fs)
      Val.bool(pos, a.endsWith(b))
    }),
    builtin("char", "n"){ (pos, ev, fs, n: Double) =>
      n.toInt.toChar.toString
    },

    builtin("strReplace", "str", "from", "to"){ (pos, ev, fs, str: String, from: String, to: String) =>
      str.replace(from, to)
    },
    builtin("strReplaceAll", "str", "from", "to"){ (pos, ev, fs, str: String, from: String, to: String) =>
      str.replaceAll(from, to)
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

    "join" -> Val.Func(null, null, Params.mk(("sep", null, 0), ("arr", null, 1)), { (scope, ev, fs, pos) =>
      val sep = scope.bindings(0).force
      val arr = implicitly[ReadWriter[Val.Arr]].apply(scope.bindings(1).force, ev, fs)
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
    }),

    "member" -> Val.Func(null, null, Params.mk(("arr", null, 0), ("x", null, 1)), { (scope, ev, fs, pos) =>
      val arr = scope.bindings(0).force
      val x = scope.bindings(1).force
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
    }),

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

    builtin("flattenArrays", "arrs"){ (pos, ev, fs, arrs: Val.Arr) =>
      val out = new mutable.ArrayBuffer[Val.Lazy]
      for(x <- arrs){
        x match{
          case Val.Null(_) => // do nothing
          case v: Val.Arr => out.appendAll(v.asLazyArray)
          case x => throw new Error.Delegate("Cannot call flattenArrays on " + x)
        }
      }
      new Val.Arr(pos, out.toArray)
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

    builtin("encodeUTF8", "s"){ (pos, ev, fs, s: String) =>
      new Val.Arr(pos, s.getBytes(UTF_8).map(i => (() => Val.Num(pos, i & 0xff)): Val.Lazy))
    },
    builtin("decodeUTF8", "arr"){ (pos, ev, fs, arr: Val.Arr) =>
      new String(arr.iterator.map(_.cast[Val.Num].value.toByte).toArray, UTF_8)
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
          val keyFApplyer = Applyer(keyFFunc, ev, fs)
          val appliedX = keyFApplyer.apply(v)

          if (b.exists(value => {
            val appliedValue = keyFApplyer.apply(value)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFApplyer.apply(value)
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
          val keyFApplyer = Applyer(keyFFunc, ev, fs)
          val appliedX = keyFApplyer.apply(v)

          if (!b.exists(value => {
            val appliedValue = keyFApplyer.apply(value)
            ev.equal(appliedValue, appliedX)
          }) && !out.exists(value => {
            val mValue = keyFApplyer.apply(value)
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
        val x: Val.Lazy = () => args("x")
        val arr = args("arr").asInstanceOf[Val.Arr].asLazyArray
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val keyFApplyer = Applyer(keyFFunc, ev, fs)
        val appliedX = keyFApplyer.apply(x)
        arr.exists(value => {
          val appliedValue = keyFApplyer.apply(value)
          ev.equal(appliedValue, appliedX)
        })
      }
    },

    "split" -> Val.Func(null, null, Params.mk(("str", null, 0), ("c", null, 1)), { (scope, ev, fs, pos) =>
      val str = implicitly[ReadWriter[String]].apply(scope.bindings(0).force, ev, fs)
      val cStr = implicitly[ReadWriter[String]].apply(scope.bindings(1).force, ev, fs)
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
    }),
    builtin("splitLimit", "str", "c", "maxSplits"){ (pos, ev, fs, str: String, c: String, maxSplits: Int) =>
      new Val.Arr(pos, str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => (() => Val.Str(pos, s)): Val.Lazy))
    },
    builtin("stringChars", "str"){ (pos, ev, fs, str: String) =>
      stringChars(pos, str)
    },
    builtin("parseInt", "str"){ (pos, ev, fs, str: String) =>
      str.toInt
    },
    builtin("parseOctal", "str"){ (pos, ev, fs, str: String) =>
      Integer.parseInt(str, 8)
    },
    builtin("parseHex", "str"){ (pos, ev, fs, str: String) =>
      Integer.parseInt(str, 16)
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
            val m = new java.util.LinkedHashMap[String, Val.Obj.Member]
            valueMap.foreach { case (k, v) =>
              m.put(k, Val.Obj.Member(false, Expr.Member.Visibility.Normal, (_, _, _, _) => recursiveTransform(v)))
            }
            new Val.Obj(pos, m, false, null, null)
        }
      }
      recursiveTransform(ujson.read(str))
    },
    builtin("md5", "s"){ (pos, ev, fs, s: String) =>
      Platform.md5(s)
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

    builtin("asciiUpper", "str"){ (pos, ev, fs, str: String) => str.toUpperCase},
    builtin("asciiLower", "str"){ (pos, ev, fs, str: String) => str.toLowerCase()},
    "trace" -> Val.Func(
      null, null,
      Params.mk(("str", null, 0), ("rest", null, 1)),
      { (scope, ev, fs, outerPos) =>
        val Val.Str(_, msg) = scope.bindings(0).force
        System.err.println(s"TRACE: ${outerPos.fileScope.currentFileLastPathElement} " + msg)
        scope.bindings(1).force
      }
    ),

    "extVar" -> Val.Func(
      null, null,
      Params.mk(("x", null, 0)),
      { (scope, ev, fs, outerPos) =>
        val Val.Str(_, x) = scope.bindings(0).force
        Materializer.reverse(
          outerPos,
          ev.extVars.getOrElse(
            x,
            throw new Error.Delegate("Unknown extVar: " + x)
          )
        )
      }
    )
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
    val params = Array(p1)
    (name, Val.Func(
      null, null,
      Params(params, new Array[Expr](params.length), params.indices.toArray),
      { (scope, ev, fs, outerPos) =>
        val v1: T1 = implicitly[ReadWriter[T1]].apply(scope.bindings(0).force, ev, fs)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1))
      }
    ))
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Position, EvalScope, FileScope, T1, T2) => R): (String, Val.Func) = {
    val params = Array(p1, p2)
    (name, Val.Func(
      null, null,
      Params(params, new Array[Expr](params.length), params.indices.toArray),
      { (scope, ev, fs, outerPos) =>
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(scope.bindings(0).force, ev, fs)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(scope.bindings(1).force, ev, fs)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1, v2))
      }
    ))
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Position, EvalScope, FileScope, T1, T2, T3) => R): (String, Val.Func) = {
    val params = Array(p1, p2, p3)
    (name, Val.Func(
      null, null,
      Params(params, new Array[Expr](params.length), params.indices.toArray),
      { (scope, ev, fs, outerPos) =>
        val v1: T1 = implicitly[ReadWriter[T1]].apply(scope.bindings(0).force, ev, fs)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(scope.bindings(1).force, ev, fs)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(scope.bindings(2).force, ev, fs)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, fs, v1, v2, v3))
      }
    ))
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
    name -> Val.Func(
      null, null,
      Params.mk(indexedParams: _*),
      { (scope, ev, fs, outerPos) =>
        val args = indexedParamKeys.map {case (k, i) => k -> scope.bindings(i).force }.toMap
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, args, fs, ev))
      },
      { (expr, scope, eval) => expr.asInstanceOf[Val] }
    )
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
        val keyFApplyer = Applyer(keyFFunc, ev, fs)

        val o1Key = keyFApplyer.apply(v)
        val o2Key = keyFApplyer.apply(out.last)
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
              val keyFApplyer = Applyer(keyFFunc, ev, fs)
              val keys = objs.map((v) => keyFApplyer((() => v): Val.Lazy))

              if (keys.forall(_.isInstanceOf[Val.Str])){
                objs.sortBy((v) => keyFApplyer((() => v): Val.Lazy).cast[Val.Str].value).map(x => (() => x): Val.Lazy)
              } else if (keys.forall(_.isInstanceOf[Val.Num])) {
                objs.sortBy((v) => keyFApplyer((() => v): Val.Lazy).cast[Val.Num].value).map(x => (() => x): Val.Lazy)
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
    val output = str.toCharArray
    new Val.Arr(pos, output.map(s => (() => Val.Str(pos, s.toString())): Val.Lazy))
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
