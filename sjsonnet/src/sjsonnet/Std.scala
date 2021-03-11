package sjsonnet

import java.io.StringWriter
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.zip.GZIPOutputStream

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{BinaryOp, False, Params}

import scala.collection.mutable
import scala.collection.compat._
import sjsonnet.Std.builtinWithDefaults
import scala.util.matching.Regex

import ujson.Value

import util.control.Breaks._

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
object Std {
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
    builtin("length", "x"){ (offset, ev, fs, v1: Val) =>
      v1 match{
        case Val.Str(_, s) => s.length
        case Val.Arr(_, s) => s.length
        case o: Val.Obj => o.getVisibleKeys().count(!_._2)
        case o: Val.Func => o.params.args.length
        case _ => throw new Error.Delegate("Cannot get length of " + v1.prettyName)
      }
    },
    builtin("objectHas", "o", "f"){ (offset, ev, fs, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2) == Some(false)
    },
    builtin("objectHasAll", "o", "f"){ (offset, ev, fs, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2).isDefined
    },
    builtin("objectFields", "o"){ (offset, ev, fs, v1: Val.Obj) =>
      val pos = Position(fs.currentFile, offset)
      val keys = getVisibleKeys(ev, v1)
      Val.Arr(pos, keys.map(k => Val.Lazy(Val.Str(pos, k))))
    },
    builtin("objectFieldsAll", "o"){ (offset, ev, fs, v1: Val.Obj) =>
      val pos = Position(fs.currentFile, offset)
      val keys = getAllKeys(ev, v1)
      Val.Arr(pos, keys.map(k => Val.Lazy(Val.Str(pos, k))))
    },
    builtin("objectValues", "o"){ (offset, ev, fs, v1: Val.Obj) =>
      val keys = getVisibleKeys(ev, v1)
      getObjValuesFromKeys(offset, ev, fs, v1, keys)
    },
    builtin("objectValuesAll", "o"){ (offset, ev, fs, v1: Val.Obj) =>
      val keys = getAllKeys(ev, v1)
      getObjValuesFromKeys(offset, ev, fs, v1, keys)
    },
    builtin("type", "x"){ (offset, ev, fs, v1: Val) =>
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
    builtin("lines", "arr"){ (offset, ev, fs, v1: Val.Arr) =>
      v1.value.map(_.force).foreach{
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
    builtin("format", "str", "vals"){ (offset, ev, fs, v1: String, v2: Val) =>
      Format.format(v1, v2, -1)(fs, ev)
    },
    builtin("foldl", "func", "arr", "init"){ (offset, ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value){
        val c = current
        current = func.apply(Val.Lazy(c), item)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ (offset, ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value.reverse){
        val c = current
        current = func.apply(item, Val.Lazy(c))
      }
      current
    },
    builtin("range", "from", "to"){ (offset, ev, fs, from: Int, to: Int) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        (from to to).map(i => Val.Lazy(Val.Num(Position(fs.currentFile, offset), i)))
      )
    },
    builtin("mergePatch", "target", "patch"){ (offset, ev, fs, target: Val, patch: Val) =>
      val mergePosition = Position(fs.currentFile, offset)
      def getNonHiddenKeys(v: Val.Obj) = v.getVisibleKeys().collect{case (k, false) => k}.toSeq
      def createMember(v: => Val) = Val.Obj.Member(false, Visibility.Unhide, (_, _, _, _) => v)
      def recPair(l: Val, r: Val): Val = (l, r) match{
        case (l: Val.Obj, r: Val.Obj) =>
          val kvs = for {
            k <- (getNonHiddenKeys(l) ++ getNonHiddenKeys(r)).distinct
            val lValue = l.valueRaw(k, l, offset)(fs, ev).map(_._1)
            val rValue = r.valueRaw(k, r, offset)(fs, ev).map(_._1)
            if !rValue.exists(_.isInstanceOf[Val.Null])
          } yield (lValue, rValue) match{
            case (Some(lChild), None) => k -> createMember{lChild}
            case (Some(lChild: Val.Obj), Some(rChild: Val.Obj)) => k -> createMember{recPair(lChild, rChild)}
            case (_, Some(rChild)) => k -> createMember{recSingle(rChild)}
          }

          new Val.Obj(mergePosition, mutable.LinkedHashMap(kvs:_*), _ => (), None)

        case (_, _) => recSingle(r)
      }
      def recSingle(v: Val): Val  = v match{
        case obj: Val.Obj =>
          val kvs = for{
            k <- getNonHiddenKeys(obj)
            val value = obj.value(k, offset, obj)(fs, ev)
            if !value.isInstanceOf[Val.Null]
          } yield (k, createMember{recSingle(value)})

          new Val.Obj(obj.pos, mutable.LinkedHashMap(kvs:_*), _ => (), None)

        case _ => v
      }
      recPair(target, patch)
    },
    builtin("sqrt", "x"){ (offset, ev, fs, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b"){ (offset, ev, fs, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b"){ (offset, ev, fs, a: Double, b: Double) =>
      math.min(a, b)
    },
    builtin("mod", "a", "b"){ (offset, ev, fs, a: Int, b: Int) =>
      a % b
    },
    builtin("clamp", "x", "minVal", "maxVal"){ (offset, ev, fs, x: Double, minVal: Double, maxVal: Double) =>
      math.max(minVal, math.min(x, maxVal))
    },

    builtin("makeArray", "sz", "func"){ (offset, ev, fs, sz: Int, func: Applyer) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        (0 until sz).map(i =>
          Val.Lazy(func.apply(Val.Lazy(Val.Num(Position(fs.currentFile, offset), i))))
        )
      )
    },

    builtin("pow", "x", "n"){ (offset, ev, fs, x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (offset, ev, fs, x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (offset, ev, fs, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (offset, ev, fs, x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (offset, ev, fs, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (offset, ev, fs, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (offset, ev, fs, x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (offset, ev, fs, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (offset, ev, fs, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (offset, ev, fs, x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (offset, ev, fs, x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (offset, ev, fs, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (offset, ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (offset, ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    builtin("isString", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Str]
    },
    builtin("isBoolean", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.True] || v.isInstanceOf[Val.False]
    },
    builtin("isNumber", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Num]
    },
    builtin("isObject", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Obj]
    },
    builtin("isArray", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Arr]
    },
    builtin("isFunction", "v"){ (offset, ev, fs, v: Val) =>
      v.isInstanceOf[Val.Func]
    },
    builtin("count", "arr", "x"){ (offset, ev, fs, arr: Val.Arr, x: Val) =>
      val res =  arr.value.count{i =>
        Materializer(i.force)(ev) == Materializer(x)(ev)
      }
      res
    },
    builtin("filter", "func", "arr"){ (offset, ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        arr.value.filter{ i =>
          func.apply(i).isInstanceOf[Val.True]
        }
      )
    },
    builtin("map", "func", "arr"){ (offset, ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        arr.value.map{ i =>
          Val.Lazy(func.apply(i))
        }
      )
    },
    builtin("mapWithKey", "func", "obj"){ (offset, ev, fs, func: Applyer, obj: Val.Obj) =>
      val allKeys = obj.getVisibleKeys()
      new Val.Obj(
        Position(fs.currentFile, offset),
        mutable.LinkedHashMap() ++
        allKeys.map{ k =>
          k._1 -> (Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _, _) =>
            func.apply(
              Val.Lazy(Val.Str(Position(fs.currentFile, offset), k._1)),
              Val.Lazy(obj.value(k._1, -1)(fs,ev))
            )
          ))
        },
        _ => (),
        None
      )
    },
    builtin("mapWithIndex", "func", "arr"){ (offset, ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        arr.value.zipWithIndex.map{ case (x, i) =>
          Val.Lazy(func.apply(Val.Lazy(Val.Num(Position(fs.currentFile, offset), i)), x))
        }
      )
    },
    builtin("flatMap", "func", "arr"){ (offset, ev, fs, func: Applyer, arr: Val) =>
      val res: Val = arr match {
        case a: Val.Arr =>
          val arrResults = a.value.flatMap {
            v => {
              val fres = func.apply(v)
              fres match {
                case va: Val.Arr => va.value
                case unknown => throw new Error.Delegate("flatMap func must return an array, not " + unknown)
              }
            }
          }
          Val.Arr(Position(fs.currentFile, offset), arrResults)

        case s: Val.Str =>
          val builder = new StringBuilder()
          for (c: Char <- s.value) {
            val fres = func.apply(Val.Lazy(Val.Str(Position(fs.currentFile, offset), c.toString)))
            builder.append(
              fres match {
                case fstr: Val.Str => fstr.value
                case _: Val.Null => ""
                case x => throw Error.Delegate("flatMap func must return string, got " + fres.asInstanceOf[Val].prettyName)
              }
            )
          }
          Val.Str(Position(fs.currentFile, offset), builder.toString)
      }
      res
    },

    builtin("filterMap", "filter_func", "map_func", "arr"){ (offset, ev, fs, filter_func: Applyer, map_func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        arr.value.flatMap { i =>
          val x = i.force
          if (!filter_func.apply(Val.Lazy(x)).isInstanceOf[Val.True]) None
          else Some(Val.Lazy(map_func.apply(Val.Lazy(x))))
        }
      )
    },
    builtin("find", "value","arr"){ (offset, ev, fs, value: Val, arr: Val.Arr) =>
      Val.Arr(
        Position(fs.currentFile, offset),
        for (
          (v, i) <- arr.value.zipWithIndex
          if Materializer(v.force)(ev) == Materializer(value)(ev)
        ) yield Val.Lazy(Val.Num(Position(fs.currentFile, offset), i))
      )
    },
    builtin("findSubstr", "pat", "str") { (offset, ev, fs, pat: String, str: String) =>
      if (pat.length == 0) Val.Arr(Position(fs.currentFile, offset), Seq())
      else {
        val indices = mutable.ArrayBuffer[Int]()
        var matchIndex = str.indexOf(pat)
        while (0 <= matchIndex && matchIndex < str.length) {
          indices.append(matchIndex)
          matchIndex = str.indexOf(pat, matchIndex + 1)
        }
        Val.Arr(Position(fs.currentFile, offset), indices.map(x => Val.Lazy(Val.Num(Position(fs.currentFile, offset), x))).toSeq)
      }
    },
    builtin("substr", "s", "from", "len"){ (offset, ev, fs, s: String, from: Int, len: Int) =>
      val safeOffset = math.min(from, s.length)
      val safeLength = math.min(len, s.length - safeOffset)
      s.substring(safeOffset, safeOffset + safeLength)
    },
    builtin("startsWith", "a", "b"){ (offset, ev, fs, a: String, b: String) =>
      a.startsWith(b)
    },
    builtin("endsWith", "a", "b"){ (offset, ev, fs, a: String, b: String) =>
      a.endsWith(b)
    },
    builtin("char", "n"){ (offset, ev, fs, n: Double) =>
      n.toInt.toChar.toString
    },

    builtin("strReplace", "str", "from", "to"){ (offset, ev, fs, str: String, from: String, to: String) =>
      str.replace(from, to)
    },
    builtin("strReplaceAll", "str", "from", "to"){ (offset, ev, fs, str: String, from: String, to: String) =>
      str.replaceAll(from, to)
    },

    builtin("rstripChars", "str", "chars"){ (offset, ev, fs, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "")
    },
    builtin("lstripChars", "str", "chars"){ (offset, ev, fs, str: String, chars: String) =>
      str.replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },
    builtin("stripChars", "str", "chars"){ (offset, ev, fs, str: String, chars: String) =>
      str.replaceAll("[" + Regex.quote(chars) + "]+$", "").replaceAll("^[" + Regex.quote(chars) + "]+", "")
    },

    builtin("join", "sep", "arr"){ (offset, ev, fs, sep: Val, arr: Val.Arr) =>
      val res: Val = sep match{
        case Val.Str(_, s) =>
          Val.Str(
            Position(fs.currentFile, offset),
            arr.value
              .map(_.force)
              .filter(!_.isInstanceOf[Val.Null])
              .map{
                case Val.Str(_, x) => x
                case x => throw new Error.Delegate("Cannot join " + x.prettyName)
              }
              .mkString(s)
          )
        case Val.Arr(_, sep) =>
          val out = collection.mutable.Buffer.empty[Val.Lazy]
          for(x <- arr.value){
            x.force match{
              case Val.Null(_) => // do nothing
              case Val.Arr(_, v) =>
                if (out.nonEmpty) out.appendAll(sep)
                out.appendAll(v)
              case x => throw new Error.Delegate("Cannot join " + x.prettyName)
            }
          }
          Val.Arr(Position(fs.currentFile, offset), out.toSeq)
        case x => throw new Error.Delegate("Cannot join " + x.prettyName)
      }
      res
    },
    builtin("member", "arr", "x"){ (offset, ev, fs, arr: Val, x: Val) =>
      val res = arr match {
        case str: Val.Str =>
          val secondArg = x match {
            case Val.Str(_, value) => value
            case n => throw new Error.Delegate("std.member second argument must be a string, got " + x.prettyName)
          }
          str.value.contains(secondArg)
        case a: Val.Arr =>
          val c = a.value.count {
            i => Materializer(i.force)(ev) == Materializer(x)(ev)
          }
          c > 0
        case x => throw new Error.Delegate("std.member first argument must be an array or a string, got " + arr.prettyName)
      }
      res
    },
    builtin("repeat", "what", "count"){ (offset, ev, fs, what: Val, count: Int) =>
      val res: Val = what match {
        case str: Val.Str =>
          val builder = new StringBuilder
          for (i <- 1 to count) {
            builder.append(str.value)
          }
          Val.Str(Position(fs.currentFile, offset), builder.toString())
        case a: Val.Arr =>
          val out = collection.mutable.Buffer.empty[Val.Lazy]
          for (i <- 1 to count) {
            out.appendAll(a.value)
          }
          Val.Arr(Position(fs.currentFile, offset), out.toSeq)
        case x => throw new Error.Delegate("std.repeat first argument must be an array or a string")
      }
      res
    },

    builtin("flattenArrays", "arrs"){ (offset, ev, fs, arrs: Val.Arr) =>
      val out = collection.mutable.Buffer.empty[Val.Lazy]
      for(x <- arrs.value){
        x.force match{
          case Val.Null(_) => // do nothing
          case Val.Arr(_, v) => out.appendAll(v)
          case x => throw new Error.Delegate("Cannot call flattenArrays on " + x)
        }
      }
      Val.Arr(Position(fs.currentFile, offset), out.toSeq)
    },

    builtin("manifestIni", "v"){ (offset, ev, fs, v: Val) =>
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
    builtin("escapeStringJson", "str"){ (offset, ev, fs, str: String) =>
      val out = new StringWriter()
      ujson.Renderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (offset, ev, fs, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (offset, ev, fs, str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (offset, ev, fs, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    builtin("manifestJson", "v"){ (offset, ev, fs, v: Val) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = 4))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtin("manifestJsonEx", "value", "indent"){ (offset, ev, fs, v: Val, i: String) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJsonEx
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = i.length))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtinWithDefaults("manifestYamlDoc",
                        "v" -> None,
                        "indent_array_in_object" -> Some(Expr.False(0))){ (offset, args, fs, ev) =>
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
                        "v" -> None,
                        "indent_array_in_object" -> Some(Expr.False(0))){ (offset, args, fs, ev) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
        case Val.False(_) => false
        case Val.True(_) => true
        case _ => throw Error.Delegate("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      v match {
        case Val.Arr(_, values) => values
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
    builtin("manifestPythonVars", "v"){ (offset, ev, fs, v: Val.Obj) =>
      Materializer(v)(ev).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (offset, ev, fs, value: Val) =>
      import scalatags.Text.all.{value => _, _}


      def rec(v: ujson.Value): Frag = {
        v match {
          case ujson.Str(s) => s
          case ujson.Arr(collection.mutable.Seq(ujson.Str(t), attrs: ujson.Obj, children@_*)) =>
            tag(t)(
              attrs.value.map {
                case (k, ujson.Str(v)) => attr(k) := v
                case (k, v) => throw new Error.Delegate("Cannot call manifestXmlJsonml on " + v.getClass)
              }.toSeq,
              children.map(rec)
            )
          case ujson.Arr(collection.mutable.Seq(ujson.Str(t), children@_*)) =>
            tag(t)(children.map(rec).toSeq)
          case x =>
            throw new Error.Delegate("Cannot call manifestXmlJsonml on " + x.getClass)
        }
      }

      rec(Materializer(value)(ev)).render

    },
    builtin("base64", "v"){ (offset, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Base64.getEncoder().encodeToString(value.getBytes)
        case Val.Arr(_, bytes) => Base64.getEncoder().encodeToString(bytes.map(_.force.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (offset, ev, fs, s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (offset, ev, fs, s: String) =>
      Val.Arr(Position(fs.currentFile, offset), Base64.getDecoder().decode(s).map(i => Val.Lazy(Val.Num(Position(fs.currentFile, offset), i))))
    },

    builtin("gzip", "v"){ (offset, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.gzipString(value)
        case Val.Arr(_, bytes) => Platform.gzipBytes(bytes.map(_.force.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot gzip encode " + x.prettyName)
      }
    },

    builtin("xz", "v"){ (offset, ev, fs, v: Val) =>
      v match{
        case Val.Str(_, value) => Platform.xzString(value)
        case Val.Arr(_, bytes) => Platform.xzBytes(bytes.map(_.force.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot xz encode " + x.prettyName)
      }
    },

    builtin("encodeUTF8", "s"){ (offset, ev, fs, s: String) =>
      Val.Arr(Position(fs.currentFile, offset), s.getBytes(UTF_8).map(i => Val.Lazy(Val.Num(Position(fs.currentFile, offset), i & 0xff))))
    },
    builtin("decodeUTF8", "arr"){ (offset, ev, fs, arr: Val.Arr) =>
      new String(arr.value.map(_.force.cast[Val.Num].value.toByte).toArray, UTF_8)
    },

    builtinWithDefaults("uniq", "arr" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      val arr = args("arr")
      val keyF = args("keyF")

      uniqArr(Position(fs.currentFile, offset), ev, arr, keyF)
    },
    builtinWithDefaults("sort", "arr" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      val arr = args("arr")
      val keyF = args("keyF")

      sortArr(Position(fs.currentFile, offset), ev, arr, keyF)
    },

    builtinWithDefaults("set", "arr" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      uniqArr(Position(fs.currentFile, offset), ev, sortArr(Position(fs.currentFile, offset), ev, args("arr"), args("keyF")), args("keyF"))
    },
    builtinWithDefaults("setUnion", "a" -> None, "b" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      val a = args("a") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val concat = Val.Arr(Position(fs.currentFile, offset), a ++ b)
      uniqArr(Position(fs.currentFile, offset), ev, sortArr(Position(fs.currentFile, offset), ev, concat, args("keyF")), args("keyF"))
    },
    builtinWithDefaults("setInter", "a" -> None, "b" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      val a = args("a") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val keyF = args("keyF")
      val out = collection.mutable.Buffer.empty[Val.Lazy]

      for (v <- a) {
        if (keyF.isInstanceOf[Val.False]) {
          val mv = Materializer.apply(v.force)(ev)
          if (b.exists(value => {
            val mValue = Materializer.apply(value.force)(ev)
            mValue == mv
          }) && !out.exists(value => {
            val mValue = Materializer.apply(value.force)(ev)
            mValue == mv
          })) {
            out.append(v)
          }
        } else {
          val keyFFunc = keyF.asInstanceOf[Val.Func]
          val keyFApplyer = Applyer(keyFFunc, ev, null)
          val appliedX = Materializer(keyFApplyer.apply(v))(ev)

          if (b.exists(value => {
            val appliedValue = keyFApplyer.apply(value)
            Materializer(appliedValue)(ev) == appliedX
          }) && !out.exists(value => {
            val mValue = keyFApplyer.apply(value)
            Materializer(mValue)(ev) == appliedX
          })) {
            out.append(v)
          }
        }
      }

      sortArr(Position(fs.currentFile, offset), ev, Val.Arr(Position(fs.currentFile, offset), out.toSeq), keyF)
    },
    builtinWithDefaults("setDiff", "a" -> None, "b" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>

      val a = args("a") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }
      val b = args("b") match {
        case arr: Val.Arr => arr.value
        case str: Val.Str => stringChars(Position(fs.currentFile, offset), str.value).value
        case _ => throw new Error.Delegate("Arguments must be either arrays or strings")
      }

      val keyF = args("keyF")
      val out = collection.mutable.Buffer.empty[Val.Lazy]

      for (v <- a) {
        if (keyF.isInstanceOf[Val.False]) {
          val mv = Materializer.apply(v.force)(ev)
          if (!b.exists(value => {
            val mValue = Materializer.apply(value.force)(ev)
            mValue == mv
          }) && !out.exists(value => {
            val mValue = Materializer.apply(value.force)(ev)
            mValue == mv
          })) {
            out.append(v)
          }
        } else {
          val keyFFunc = keyF.asInstanceOf[Val.Func]
          val keyFApplyer = Applyer(keyFFunc, ev, null)
          val appliedX = Materializer(keyFApplyer.apply(v))(ev)

          if (!b.exists(value => {
            val appliedValue = keyFApplyer.apply(value)
            Materializer(appliedValue)(ev) == appliedX
          }) && !out.exists(value => {
            val mValue = keyFApplyer.apply(value)
            Materializer(mValue)(ev) == appliedX
          })) {
            out.append(v)
          }
        }
      }

      sortArr(Position(fs.currentFile, offset), ev, Val.Arr(Position(fs.currentFile, offset), out.toSeq), keyF)
    },
    builtinWithDefaults("setMember", "x" -> None, "arr" -> None, "keyF" -> Some(Expr.False(0))) { (offset, args, fs, ev) =>
      val keyF = args("keyF")

      if (keyF.isInstanceOf[Val.False]) {
        val ujson.Arr(mArr) = Materializer(args("arr"))(ev)
        val mx = Materializer(args("x"))(ev)
        mArr.contains(mx)
      } else {
        val x = Val.Lazy(args("x"))
        val arr = args("arr").asInstanceOf[Val.Arr].value
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val keyFApplyer = Applyer(keyFFunc, ev, null)
        val appliedX = keyFApplyer.apply(x)
        arr.exists(value => {
          val appliedValue = keyFApplyer.apply(value)
          Materializer(appliedValue)(ev) == Materializer(appliedX)(ev)
        })
      }
    },

    builtin("split", "str", "c"){ (offset, ev, fs, str: String, c: String) =>
      Val.Arr(Position(fs.currentFile, offset), str.split(java.util.regex.Pattern.quote(c), -1).map(s => Val.Lazy(Val.Str(Position(fs.currentFile, offset), s))))
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ (offset, ev, fs, str: String, c: String, maxSplits: Int) =>
      Val.Arr(Position(fs.currentFile, offset), str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => Val.Lazy(Val.Str(Position(fs.currentFile, offset), s))))
    },
    builtin("stringChars", "str"){ (offset, ev, fs, str: String) =>
      stringChars(Position(fs.currentFile, offset), str)
    },
    builtin("parseInt", "str"){ (offset, ev, fs, str: String) =>
      str.toInt
    },
    builtin("parseOctal", "str"){ (offset, ev, fs, str: String) =>
      Integer.parseInt(str, 8)
    },
    builtin("parseHex", "str"){ (offset, ev, fs, str: String) =>
      Integer.parseInt(str, 16)
    },
    builtin("parseJson", "str") { (offset, ev, fs, str: String) =>

      def recursiveTransform(js: ujson.Value): Val = {
        js match {
          case ujson.Null => Val.Null(Position(fs.currentFile, offset))
          case ujson.True => Val.True(Position(fs.currentFile, offset))
          case ujson.False => Val.False(Position(fs.currentFile, offset))
          case ujson.Num(value) => Val.Num(Position(fs.currentFile, offset), value)
          case ujson.Str(value) => Val.Str(Position(fs.currentFile, offset), value)
          case ujson.Arr(values) =>
            val transformedValue: Seq[Val.Lazy] = values.map(v => Val.Lazy(recursiveTransform(v))).toSeq
            Val.Arr(Position(fs.currentFile, offset), transformedValue)
          case ujson.Obj(valueMap) =>
            val transformedValue = mutable.LinkedHashMap() ++ valueMap
              .mapValues { v =>
                Val.Obj.Member(false, Expr.Member.Visibility.Normal, (_, _, _, _) => recursiveTransform(v))
              }
            new Val.Obj(Position(fs.currentFile, offset), transformedValue , (x: Val.Obj) => (), None)
        }
      }
      recursiveTransform(ujson.read(str))
    },
    builtin("md5", "s"){ (offset, ev, fs, s: String) =>
      Platform.md5(s)
    },
    builtin("prune", "x"){ (offset, ev, fs, s: Val) =>
      def filter(x: Val) = x match{
        case c: Val.Arr if c.value.isEmpty => false
        case c: Val.Obj if c.getVisibleKeys().count(_._2 == false) == 0 => false
        case Val.Null(_) => false
        case _ => true
      }
      def rec(x: Val): Val = x match{
        case o: Val.Obj =>
          val bindings = for{
            (k, hidden) <- o.getVisibleKeys()
            if !hidden
            v = rec(o.value(k, -1)(fs, ev))
            if filter(v)
          }yield (k, Val.Obj.Member(false, Visibility.Normal, (_, _, _, _) => v))
          new Val.Obj(Position(fs.currentFile, offset), mutable.LinkedHashMap() ++ bindings, _ => (), None)
        case a: Val.Arr =>
          Val.Arr(Position(fs.currentFile, offset), a.value.map(x => rec(x.force)).filter(filter).map(Val.Lazy(_)))
        case _ => x
      }
      rec(s)
    },

    builtin("asciiUpper", "str"){ (offset, ev, fs, str: String) => str.toUpperCase},
    builtin("asciiLower", "str"){ (offset, ev, fs, str: String) => str.toLowerCase()},
    "trace" -> Val.Func(
      null,
      None,
      Params(Array(("str", None, 0), ("rest", None, 1))),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val Val.Str(_, msg) = scope.bindings(0).get.force
        System.err.println(s"TRACE: $thisFile " + msg)
        scope.bindings(1).get.force
      }
    ),

    "extVar" -> Val.Func(
      null,
      None,
      Params(Array(("x", None, 0))),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val Val.Str(_, x) = scope.bindings(0).get.force
        Materializer.reverse(
          Position(fs.currentFile, outerOffset),
          ev.extVars.getOrElse(
            x,
            throw new Error.Delegate("Unknown extVar: " + x)
          )
        )
      }
    )
  )
  val Std = new Val.Obj(
    null,
    mutable.LinkedHashMap() ++
    functions
      .map{
        case (k, v) =>
          (
            k,
            Val.Obj.Member(
              false,
              Visibility.Hidden,
              (self: Val.Obj, sup: Option[Val.Obj], _, _) => v
            )
          )
      } ++ Seq(
      (
        "thisFile",
        Val.Obj.Member(
          false,
          Visibility.Hidden,
          { (self: Val.Obj, sup: Option[Val.Obj], fs: FileScope, eval: EvalScope) =>
            Val.Str(self.pos, fs.currentFile.relativeToString(eval.wd))
          },
          cached = false
        )
      )
    ),
    _ => (),
    None
  )

  def validate(vs: Array[Val],
               ev: EvalScope,
               fs: FileScope,
               rs: Array[ReadWriter[_]]): Seq[Any] = {
    var i = 0
    val size = vs.size
    val ret = new Array[Any](size)
    while (i < size) {
      val v = vs(i)
      val r = rs(i)
      r.apply(v, ev, fs) match {
        case Left(err) => throw new Error.Delegate("Wrong parameter type: expected " + err + ", got " + v.prettyName)
        case Right(x) => ret(i) = x
      }
      i += 1
    }
    ret
  }

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (Int, EvalScope, FileScope, T1) => R): (String, Val.Func) = builtin0(name, p1){ (offset, vs, ev, fs) =>
    val Seq(v: T1) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]]))
    eval(offset, ev, fs, v)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Int, EvalScope, FileScope, T1, T2) => R): (String, Val.Func) = builtin0(name, p1, p2){ (offset, vs, ev, fs) =>
    val Seq(v1: T1, v2: T2) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]]))
    eval(offset, ev, fs, v1, v2)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Int, EvalScope, FileScope, T1, T2, T3) => R): (String, Val.Func) = builtin0(name, p1, p2, p3){ (offset, vs, ev, fs) =>
    val Seq(v1: T1, v2: T2, v3: T3) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]], implicitly[ReadWriter[T3]]))
    eval(offset, ev, fs, v1, v2, v3)
  }
  def builtin0[R: ReadWriter](name: String, params: String*)(eval: (Int, Array[Val], EvalScope, FileScope) => R) = {
    val paramData = params.zipWithIndex.map{case (k, i) => (k, None, i)}.toArray
    val paramIndices = params.indices.toArray
    name -> Val.Func(
      null,
      None,
      Params(paramData),
      {(scope, thisFile, ev, fs, outerOffset) =>
        implicitly[ReadWriter[R]].write(
          Position(fs.currentFile, outerOffset),
          eval(outerOffset, paramIndices.map(i => scope.bindings(i).get.force), ev, fs)
        )
      }
    )
  }
  /**
    * Helper function that can define a built-in function with default parameters
    *
    * Arguments of the eval function are (args, ev)
    */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Option[Expr])*)
                                        (eval: (Int, Map[String, Val], FileScope, EvalScope) => R): (String, Val.Func) = {
    val indexedParams = params.zipWithIndex.map{case ((k, v), i) => (k, v, i)}.toArray
    val indexedParamKeys = params.zipWithIndex.map{case ((k, v), i) => (k, i)}
    name -> Val.Func(
      null,
      None,
      Params(indexedParams),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val args = indexedParamKeys.map {case (k, i) => k -> scope.bindings(i).get.force }.toMap
        implicitly[ReadWriter[R]].write(Position(fs.currentFile, outerOffset), eval(outerOffset, args, fs, ev))
      },
      { (expr, scope, eval) =>
        eval.visitExpr(expr)(scope, new FileScope(null, Map.empty))
      }
    )
  }

  def scope(size: Int) = {
    new ValScope(
      None, None, None, Array(Val.Lazy(Std)).padTo(size, null)
    )
  }

  def uniqArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    val arrValue = arr match {
      case arr: Val.Arr => arr.value
      case str: Val.Str => stringChars(pos, str.value).value
      case _ => throw new Error.Delegate("Argument must be either array or string")
    }

    val out = collection.mutable.Buffer.empty[Val.Lazy]
    for (v <- arrValue) {
      if (out.isEmpty) {
        out.append(v)
      } else if (keyF.isInstanceOf[Val.False]) {
        val ol = Materializer.apply(out.last.force)(ev)
        val mv = Materializer.apply(v.force)(ev)
        if (ol != mv) {
          out.append(v)
        }
      } else if (!keyF.isInstanceOf[Val.False]) {
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val keyFApplyer = Applyer(keyFFunc, ev, null)

        val o1Key = keyFApplyer.apply(v)
        val o2Key = keyFApplyer.apply(out.last)
        val o1KeyExpr = Materializer.toExpr(Materializer.apply(o1Key)(ev))
        val o2KeyExpr = Materializer.toExpr(Materializer.apply(o2Key)(ev))

        val comparisonExpr = Expr.BinaryOp(0, o1KeyExpr, BinaryOp.`!=`, o2KeyExpr)
        val exprResult = ev.visitExpr(comparisonExpr)(scope(0), new FileScope(null, Map.empty))

        val res = Materializer.apply(exprResult)(ev).asInstanceOf[ujson.Bool]

        if (res.value) {
          out.append(v)
        }
      }
    }

    Val.Arr(pos, out.toSeq)
  }

  def sortArr(pos: Position, ev: EvalScope, arr: Val, keyF: Val) = {
    arr match{
      case Val.Arr(_, vs) =>
        Val.Arr(
          pos,

          if (vs.forall(_.force.isInstanceOf[Val.Str])){
            vs.map(_.force.cast[Val.Str]).sortBy(_.value).map(Val.Lazy(_))
          }else if (vs.forall(_.force.isInstanceOf[Val.Num])) {
            vs.map(_.force.cast[Val.Num]).sortBy(_.value).map(Val.Lazy(_))
          }else if (vs.forall(_.force.isInstanceOf[Val.Obj])){
            if (keyF.isInstanceOf[Val.False]) {
              throw new Error.Delegate("Unable to sort array of objects without key function")
            } else {
              val objs = vs.map(_.force.cast[Val.Obj])

              val keyFFunc = keyF.asInstanceOf[Val.Func]
              val keyFApplyer = Applyer(keyFFunc, ev, null)
              val keys = objs.map((v) => keyFApplyer(Val.Lazy(v)))

              if (keys.forall(_.isInstanceOf[Val.Str])){
                objs.sortBy((v) => keyFApplyer(Val.Lazy(v)).cast[Val.Str].value).map(Val.Lazy(_))
              } else if (keys.forall(_.isInstanceOf[Val.Num])) {
                objs.sortBy((v) => keyFApplyer(Val.Lazy(v)).cast[Val.Num].value).map(Val.Lazy(_))
              } else {
                throw new Error.Delegate("Cannot sort with key values that are " + keys(0).prettyName + "s")
              }
            }
          }else {
            ???
          }
        )
      case Val.Str(pos, s) => Val.Arr(pos, s.sorted.map(c => Val.Lazy(Val.Str(pos, c.toString))))
      case x => throw new Error.Delegate("Cannot sort " + x.prettyName)
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    var offset = 0
    val output = str.toSeq.sliding(1).toList
    Val.Arr(pos, output.map(s => Val.Lazy(Val.Str(pos, s.toString()))).toSeq)
  }
  
  def getVisibleKeys(ev: EvalScope, v1: Val.Obj): Seq[String] = {
    val keys = v1.getVisibleKeys()
      .collect{case (k, false) => k}
      .toSeq
    
    maybeSortKeys(ev, keys)
  }
  
  def getAllKeys(ev: EvalScope, v1: Val.Obj): Seq[String] = {
    val keys = v1.getVisibleKeys()
      .collect{case (k, _) => k}
      .toSeq
    
    maybeSortKeys(ev, keys)
  }
  
  def maybeSortKeys(ev: EvalScope, keys: Seq[String]): Seq[String] = {
    if(ev.preserveOrder) {
      keys
    } else {
      keys.sorted
    }
  }
  
  def getObjValuesFromKeys(offset: Int, ev: EvalScope, fs: FileScope, v1: Val.Obj, keys: Seq[String]): Val.Arr = {
    Val.Arr(Position(fs.currentFile, offset), keys.map { k =>
      Val.Lazy(v1.value(k, -1)(fs, ev))
    })
  }
}
