package sjsonnet

import java.io.StringWriter
import java.util.Base64

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{False, Params}

import scala.collection.mutable.ArrayBuffer
import scala.collection.compat._
import com.google.re2j.{Matcher, Pattern, RE2}
import ujson.Value
import util.control.Breaks._

/**
  * The Jsonnet standard library, `std`, with each builtin function implemented
  * in Scala code. Uses `builtin` and other helpers to handle the common wrapper
  * logic automatically
  */
object Std {
  val functions: Seq[(String, Val.Func)] = Seq(
    builtin("assertEqual", "a", "b"){ (ev, fs, v1: Val, v2: Val) =>
      val x1 = Materializer(v1)(ev)
      val x2 = Materializer(v2)(ev)
      if (x1 == x2) true
      else throw new Error.Delegate("assertEqual failed: " + x1 + " != " + x2)
    },
    builtin("toString", "a"){ (ev, fs, v1: Val) =>
      v1 match{
        case Val.Str(s) => s
        case v => Materializer.stringify(v)(ev)
      }
    },
    builtin("codepoint", "str"){ (ev, fs, v1: Val) =>
      v1.cast[Val.Str].value.charAt(0).toInt
    },
    builtin("length", "x"){ (ev, fs, v1: Val) =>
      v1 match{
        case Val.Str(s) => s.length
        case Val.Arr(s) => s.length
        case o: Val.Obj => o.getVisibleKeys().count(!_._2)
        case o: Val.Func => o.params.args.length
        case _ => throw new Error.Delegate("Cannot get length of " + v1.prettyName)
      }
    },
    builtin("objectHas", "o", "f"){ (ev, fs, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2) == Some(false)
    },
    builtin("objectHasAll", "o", "f"){ (ev, fs, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2).isDefined
    },
    builtin("objectFields", "o"){ (ev, fs, v1: Val.Obj) =>
      Val.Arr(
        v1.getVisibleKeys()
          .collect{case (k, false) => k}
          .toSeq
          .sorted
          .map(k => Val.Lazy(Val.Str(k)))
      )
    },
    builtin("objectFieldsAll", "o"){ (ev, fs, v1: Val.Obj) =>
      Val.Arr(
        v1.getVisibleKeys()
          .collect{case (k, _) => k}
          .toSeq
          .sorted
          .map(k => Val.Lazy(Val.Str(k)))
      )
    },
    builtin("type", "x"){ (ev, fs, v1: Val) =>
      v1 match{
        case Val.True | Val.False => "boolean"
        case Val.Null => "null"
        case _: Val.Obj => "object"
        case _: Val.Arr => "array"
        case _: Val.Func => "function"
        case _: Val.Num => "number"
        case _: Val.Str => "string"
      }
    },
    builtin("lines", "arr"){ (ev, fs, v1: Val.Arr) =>
      v1.value.map(_.force).foreach{
        case _: Val.Str | Val.Null => // donothing
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
    builtin("format", "str", "vals"){ (ev, fs, v1: String, v2: Val) =>
      Format.format(v1, v2, -1)(fs, ev)
    },
    builtin("foldl", "func", "arr", "init"){ (ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value){
        val c = current
        current = func.apply(Val.Lazy(c), item)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ (ev, fs, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value.reverse){
        val c = current
        current = func.apply(item, Val.Lazy(c))
      }
      current
    },
    builtin("range", "from", "to"){ (ev, fs, from: Int, to: Int) =>
      Val.Arr(
        (from to to).map(i => Val.Lazy(Val.Num(i)))
      )
    },
    builtin("mergePatch", "target", "patch"){ (ev, fs, target: Val, patch: Val) =>
      def rec(l: ujson.Value, r: ujson.Value): ujson.Value = {
        (l, r) match{
          case (l0, r: ujson.Obj) =>
            val l = l0 match{
              case l: ujson.Obj => l
              case _ => ujson.Obj()
            }
            for((k, v) <- r.value){
              if (v == ujson.Null) l.value.remove(k)
              else if (l.value.contains(k)) l(k) = rec(l(k), r(k))
              else l(k) = rec(ujson.Obj(), r(k))
            }
            l
          case (_, _) => r
        }
      }
      Materializer.reverse(rec(Materializer(target)(ev), Materializer(patch)(ev)))
    },
    builtin("sqrt", "x"){ (ev, fs, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b"){ (ev, fs, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b"){ (ev, fs, a: Double, b: Double) =>
      math.min(a, b)
    },
    builtin("mod", "a", "b"){ (ev, fs, a: Int, b: Int) =>
      a % b
    },

    builtin("makeArray", "sz", "func"){ (ev, fs, sz: Int, func: Applyer) =>
      Val.Arr(
        (0 until sz).map(i =>
          Val.Lazy(func.apply(Val.Lazy(Val.Num(i))))
        )
      )
    },

    builtin("pow", "x", "n"){ (ev, fs, x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (ev, fs, x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (ev, fs, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (ev, fs, x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (ev, fs, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (ev, fs, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (ev, fs, x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (ev, fs, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (ev, fs, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (ev, fs, x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (ev, fs, x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (ev, fs, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (ev, fs, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    builtin("isString", "v"){ (ev, fs, v: Val) =>
      v.isInstanceOf[Val.Str]
    },
    builtin("isBoolean", "v"){ (ev, fs, v: Val) =>
      v == Val.True || v == Val.False
    },
    builtin("isNumber", "v"){ (ev, fs, v: Val) =>
      v.isInstanceOf[Val.Num]
    },
    builtin("isObject", "v"){ (ev, fs, v: Val) =>
      v.isInstanceOf[Val.Obj]
    },
    builtin("isArray", "v"){ (ev, fs, v: Val) =>
      v.isInstanceOf[Val.Arr]
    },
    builtin("isFunction", "v"){ (ev, fs, v: Val) =>
      v.isInstanceOf[Val.Func]
    },
    builtin("count", "arr", "x"){ (ev, fs, arr: Val.Arr, x: Val) =>
      val res =  arr.value.count{i =>
        Materializer(i.force)(ev) == Materializer(x)(ev)
      }
      res
    },
    builtin("filter", "func", "arr"){ (ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.filter{ i =>
          func.apply(i) == Val.True
        }
      )
    },
    builtin("map", "func", "arr"){ (ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.map{ i =>
          Val.Lazy(func.apply(i))
        }
      )
    },
    builtin("mapWithKey", "func", "obj"){ (ev, fs, func: Applyer, obj: Val.Obj) =>
      val allKeys = obj.getVisibleKeys()
      new Val.Obj(
        allKeys.map{ k =>
          k._1 -> (Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _, _) =>
            func.apply(
              Val.Lazy(Val.Str(k._1)),
              Val.Lazy(obj.value(k._1, -1)(fs,ev))
            )
          ))
        }.toMap,
        _ => (),
        None
      )
    },
    builtin("mapWithIndex", "func", "arr"){ (ev, fs, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.zipWithIndex.map{ case (x, i) =>
          Val.Lazy(func.apply(Val.Lazy(Val.Num(i)), x))
        }
      )
    },
    builtin("filterMap", "filter_func", "map_func", "arr"){ (ev, fs, filter_func: Applyer, map_func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.flatMap { i =>
          val x = i.force
          if (filter_func.apply(Val.Lazy(x)) != Val.True) None
          else Some(Val.Lazy(map_func.apply(Val.Lazy(x))))
        }
      )
    },
    builtin("find", "value","arr"){ (ev, fs, value: Val, arr: Val.Arr) =>
      Val.Arr(
        for (
          (v, i) <- arr.value.zipWithIndex
          if Materializer(v.force)(ev) == Materializer(value)(ev)
        ) yield Val.Lazy(Val.Num(i))
      )
    },
    builtin("findSubstr", "pat", "str") { (ev, fs, pat: String, str: String) =>
      if (pat.length == 0) Val.Arr(Seq())
      else {
        val indices = ArrayBuffer[Int]()
        var matchIndex = str.indexOf(pat)
        while (0 <= matchIndex && matchIndex < str.length) {
          indices.append(matchIndex)
          matchIndex = str.indexOf(pat, matchIndex + 1)
        }
        Val.Arr(indices.map(x => Val.Lazy(Val.Num(x))).toSeq)
      }
    },
    builtin("substr", "s", "from", "len"){ (ev, fs, s: String, from: Int, len: Int) =>
      val safeOffset = math.min(from, s.length)
      val safeLength = math.min(len, s.length - safeOffset)
      s.substring(safeOffset, safeOffset + safeLength)
    },
    builtin("startsWith", "a", "b"){ (ev, fs, a: String, b: String) =>
      a.startsWith(b)
    },
    builtin("endsWith", "a", "b"){ (ev, fs, a: String, b: String) =>
      a.endsWith(b)
    },
    builtin("char", "n"){ (ev, fs, n: Double) =>
      n.toInt.toChar.toString
    },

    builtin("strReplace", "str", "from", "to"){ (ev, fs, str: String, from: String, to: String) =>
      str.replace(from, to)
    },
    builtin("join", "sep", "arr"){ (ev, fs, sep: Val, arr: Val.Arr) =>
      val res: Val = sep match{
        case Val.Str(s) =>
          Val.Str(
            arr.value
              .map(_.force)
              .filter(_ != Val.Null)
              .map{
                case Val.Str(x) => x
                case x => throw new Error.Delegate("Cannot join " + x.prettyName)
              }
              .mkString(s)
          )
        case Val.Arr(sep) =>
          val out = collection.mutable.Buffer.empty[Val.Lazy]
          for(x <- arr.value){
            x.force match{
              case Val.Null => // do nothing
              case Val.Arr(v) =>
                if (out.nonEmpty) out.appendAll(sep)
                out.appendAll(v)
              case x => throw new Error.Delegate("Cannot join " + x.prettyName)
            }
          }
          Val.Arr(out.toSeq)
        case x => throw new Error.Delegate("Cannot join " + x.prettyName)
      }
      res
    },
    builtin("flattenArrays", "arrs"){ (ev, fs, arrs: Val.Arr) =>
      val out = collection.mutable.Buffer.empty[Val.Lazy]
      for(x <- arrs.value){
        x.force match{
          case Val.Null => // do nothing
          case Val.Arr(v) => out.appendAll(v)
          case x => throw new Error.Delegate("Cannot call flattenArrays on " + x)
        }
      }
      Val.Arr(out.toSeq)
    },
    builtin("manifestIni", "v"){ (ev, fs, v: Val) =>
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
    builtin("escapeStringJson", "str"){ (ev, fs, str: String) =>
      val out = new StringWriter()
      ujson.Renderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (ev, fs, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (ev, fs, str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (ev, fs, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    builtin("manifestJson", "v"){ (ev, fs, v: Val) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = 4))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtin("manifestJsonEx", "value", "indent"){ (ev, fs, v: Val, i: String) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJsonEx
      Materializer
        .apply0(v, new ujson.StringRenderer(indent = i.length))(ev)
        .toString
        .replaceAll("\n[ ]+\n", "\n\n")
    },
    builtinWithDefaults("manifestYamlDoc",
                        "v" -> None,
                        "indent_array_in_object" -> Some(Expr.False(0))){ (args, ev) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
          case Val.False => false
          case Val.True => true
          case _ => throw Error.Delegate("indent_array_in_object has to be a boolean, got" + v.getClass)
        }
      Materializer.apply0(
        v,
        new YamlRenderer(indentArrayInObject = indentArrayInObject)
      )(ev).toString
    },
    builtinWithDefaults("manifestYamlStream",
                        "v" -> None,
                        "indent_array_in_object" -> Some(Expr.False(0))){ (args, ev) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
        case Val.False => false
        case Val.True => true
        case _ => throw Error.Delegate("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      v match {
        case Val.Arr(values) => values
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
    builtin("manifestPythonVars", "v"){ (ev, fs, v: Val.Obj) =>
      Materializer(v)(ev).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (ev, fs, value: Val) =>
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
    builtin("base64", "v"){ (ev, fs, v: Val) =>
      v match{
        case Val.Str(value) => Base64.getEncoder().encodeToString(value.getBytes)
        case Val.Arr(bytes) => Base64.getEncoder().encodeToString(bytes.map(_.force.cast[Val.Num].value.toByte).toArray)
        case x => throw new Error.Delegate("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (ev, fs, s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (ev, fs, s: String) =>
      Val.Arr(Base64.getDecoder().decode(s).map(i => Val.Lazy(Val.Num(i))))
    },
    builtin("sort", "arr"){ (ev, fs, arr: Val) =>
      arr match{
        case Val.Arr(vs) =>
          Val.Arr(

            if (vs.forall(_.force.isInstanceOf[Val.Str])){
              vs.map(_.force.cast[Val.Str]).sortBy(_.value).map(Val.Lazy(_))
            }else if (vs.forall(_.force.isInstanceOf[Val.Num])){
              vs.map(_.force.cast[Val.Num]).sortBy(_.value).map(Val.Lazy(_))
            }else {
              ???
            }
          )
        case Val.Str(s) => Val.Arr(s.sorted.map(c => Val.Lazy(Val.Str(c.toString))))
        case x => throw new Error.Delegate("Cannot sort " + x.prettyName)
      }
    },
    builtin("uniq", "arr"){ (ev, fs, arr: Val.Arr) =>
      val ujson.Arr(vs) = Materializer(arr)(ev)
      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Val.Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("set", "arr"){ (ev, fs, arr: Val.Arr) =>
      val ujson.Arr(vs0) = Materializer(arr)(ev)
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new Error.Delegate("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Val.Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setUnion", "a", "b"){ (ev, fs, a: Val.Arr, b: Val.Arr) =>

      val ujson.Arr(vs1) = Materializer(a)(ev)
      val ujson.Arr(vs2) = Materializer(b)(ev)
      val vs0 = vs1 ++ vs2
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new Error.Delegate("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Val.Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setInter", "a", "b"){ (ev, fs, a: Val, b: Val.Arr) =>
      val vs1 = Materializer(a)(ev) match{
        case ujson.Arr(vs1) => vs1
        case x => Seq(x)
      }
      val ujson.Arr(vs2) = Materializer(b)(ev)


      val vs0 = vs1.to(collection.mutable.LinkedHashSet)
        .intersect(vs2.to(collection.mutable.LinkedHashSet))
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new Error.Delegate("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Val.Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setDiff", "a", "b"){ (ev, fs, a: Val.Arr, b: Val.Arr) =>
      val ujson.Arr(vs1) = Materializer(a)(ev)
      val ujson.Arr(vs2) = Materializer(b)(ev)


      val vs0 = vs1.to(collection.mutable.LinkedHashSet)
        .diff(vs2.to(collection.mutable.LinkedHashSet))
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new Error.Delegate("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Val.Lazy(Materializer.reverse(v))).toSeq)

    },

    builtin("setMember", "x", "arr"){ (ev, fs, x: Val, arr: Val.Arr) =>
      val vs1 = Materializer(x)(ev)
      val ujson.Arr(vs2) = Materializer(arr)(ev)
      vs2.contains(vs1)
    },


    builtinWithDefaults("setMember", "x" -> None, "arr" -> None, "keyF" ->
      Some(Expr.False(0))
    ) { (args, ev) =>
      val x = args("x")
      val arr = args("arr")
      val keyF = args("keyF")

      val vs1 = Materializer(x)(ev)
      val ujson.Arr(vs2) = Materializer(arr)(ev)

//      System.out.println("******* X is " + x)
//      System.out.println("******* ARR is " + arr)
//      System.out.println("******* keyF is " + keyF)
//
      if (keyF == Val.False) {
        vs2.contains(vs1)
      } else {
        var found = false;
        val keyFFunc = keyF.asInstanceOf[Val.Func]
        val keyFApplyer = Applyer(keyFFunc, ev, null)
        val appliedX = keyFApplyer.apply(Val.Lazy(x))

        breakable {
          vs2.foreach(value => {
            val appliedValue = keyFApplyer.apply(Val.Lazy(Materializer.reverse(value)))
            //System.out.println("Value is " + appliedValue)
            if (appliedValue == appliedX) {
              found = true
              break
            }
          })
        }
        found
      }
    },

/*
    builtin("setMember", "x", "arr"){ (ev, fs, x: Val, arr: Val.Arr) =>
      val vs1 = Materializer(x)(ev)
      val ujson.Arr(vs2) = Materializer(arr)(ev)
      vs2.contains(vs1)
    },
*/
    builtin("split", "str", "c"){ (ev, fs, str: String, c: String) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), -1).map(s => Val.Lazy(Val.Str(s))))
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ (ev, fs, str: String, c: String, maxSplits: Int) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => Val.Lazy(Val.Str(s))))
    },
    builtin("stringChars", "str"){ (ev, fs, str: String) =>

      var offset = 0
      val output = collection.mutable.Buffer.empty[String]
      while (offset < str.length) {
        val codepoint = str.codePointAt(offset)
        output.append(new String(Character.toChars(codepoint)))
        offset += Character.charCount(codepoint)
      }
      Val.Arr(output.map(s => Val.Lazy(Val.Str(s))).toSeq)

    },
    builtin("parseInt", "str"){ (ev, fs, str: String) =>
      str.toInt
    },
    builtin("parseOctal", "str"){ (ev, fs, str: String) =>
      Integer.parseInt(str, 8)
    },
    builtin("parseHex", "str"){ (ev, fs, str: String) =>
      Integer.parseInt(str, 16)
    },
    builtin("parseJson", "str") { (ev, fs, str: String) =>

      def recursiveTransform(js: ujson.Value): Val = {
        js match {
          case ujson.Null => Val.Null
          case ujson.True => Val.True
          case ujson.False => Val.False
          case ujson.Num(value) => Val.Num(value)
          case ujson.Str(value) => Val.Str(value)
          case ujson.Arr(values) =>
            val transformedValue: Seq[Val.Lazy] = values.map(v => Val.Lazy(recursiveTransform(v))).toSeq
            Val.Arr(transformedValue)
          case ujson.Obj(valueMap) =>
            val transformedValue = valueMap
              .mapValues { v =>
                Val.Obj.Member(false, Expr.Member.Visibility.Normal, (_, _, _, _) => recursiveTransform(v))
              }.toMap
            new Val.Obj(transformedValue , (x: Val.Obj) => (), None)
        }
      }
      recursiveTransform(ujson.read(str))
    },
    builtin("md5", "s"){ (ev, fs, s: String) =>
      Platform.md5(s)
    },
    builtin("prune", "x"){ (ev, fs, s: Val) =>
      def filter(x: Val) = x match{
        case c: Val.Arr if c.value.isEmpty => false
        case c: Val.Obj if c.getVisibleKeys().count(_._2 == false) == 0 => false
        case Val.Null => false
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
          new Val.Obj(bindings.toMap, _ => (), None)
        case a: Val.Arr =>
          Val.Arr(a.value.map(x => rec(x.force)).filter(filter).map(Val.Lazy(_)))
        case _ => x
      }
      rec(s)
    },

    builtin("asciiUpper", "str"){ (ev, fs, str: String) => str.toUpperCase},
    builtin("asciiLower", "str"){ (ev, fs, str: String) => str.toLowerCase()},
    "trace" -> Val.Func(
      None,
      Params(Array(("str", None, 0), ("rest", None, 1))),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val Val.Str(msg) = scope.bindings(0).get.force
        System.err.println(s"TRACE: $thisFile " + msg)
        scope.bindings(1).get.force
      }
    ),

    //Regex functions as described in this PR: https://github.com/google/jsonnet/pull/665
    builtin("regexFullMatch", "pattern", "str"){ (ev, fs, pattern: String, str: String) =>
      Platform.patternMatches(pattern, str)
    },
    builtin("regexPartialMatch", "pattern", "str"){ (ev, fs, pattern: String, str: String) =>
      Platform.patternFind(pattern, str)
    },
    builtin("regexQuoteMeta","str"){ (ev, fs, str: String) =>
      Platform.patternQuote(str)
    },
    builtin("regexReplace","str", "pattern", "to"){ (ev, fs, str: String, pattern: String, to: String) =>
      Platform.patternReplaceFirst(pattern, str, to)
    },
    builtin("regexGlobalReplace","str", "pattern", "to"){ (ev, fs, str: String, pattern: String, to: String) =>
      Platform.patternReplaceAll(pattern, str, to)
    },
    //////////////////////////////////////////////////////////////

    "extVar" -> Val.Func(
      None,
      Params(Array(("x", None, 0))),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val Val.Str(x) = scope.bindings(0).get.force
        Materializer.reverse(
          ev.extVars.getOrElse(
            x,
            throw new Error.Delegate("Unknown extVar: " + x)
          )
        )
      }
    )
  )
  val Std = new Val.Obj(
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
      }
      .toMap ++ Seq(
      (
        "thisFile",
        Val.Obj.Member(
          false,
          Visibility.Hidden,
          { (self: Val.Obj, sup: Option[Val.Obj], fs: FileScope, eval: EvalScope) =>
            Val.Str(fs.currentFile.relativeToString(eval.wd))
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
               rs: Array[ReadWriter[_]]) = {
    for(i <- vs.indices) yield {
      val v = vs(i)
      val r = rs(i)
      r.apply(v, ev, fs) match {
        case Left(err) => throw new Error.Delegate("Wrong parameter type: expected " + err + ", got " + v.prettyName)
        case Right(x) => x
      }
    }
  }

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (EvalScope, FileScope, T1) => R): (String, Val.Func) = builtin0(name, p1){ (vs, ev, fs) =>
    val Seq(v: T1) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]]))
    eval(ev, fs, v)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (EvalScope, FileScope, T1, T2) => R): (String, Val.Func) = builtin0(name, p1, p2){ (vs, ev, fs) =>
    val Seq(v1: T1, v2: T2) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]]))
    eval(ev, fs, v1, v2)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (EvalScope, FileScope, T1, T2, T3) => R): (String, Val.Func) = builtin0(name, p1, p2, p3){ (vs, ev, fs) =>
    val Seq(v1: T1, v2: T2, v3: T3) = validate(vs, ev, fs, Array(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]], implicitly[ReadWriter[T3]]))
    eval(ev, fs, v1, v2, v3)
  }
  def builtin0[R: ReadWriter](name: String, params: String*)(eval: (Array[Val], EvalScope, FileScope) => R) = {
    val paramData = params.zipWithIndex.map{case (k, i) => (k, None, i)}.toArray
    val paramIndices = params.indices.toArray
    name -> Val.Func(
      None,
      Params(paramData),
      {(scope, thisFile, ev, fs, outerOffset) =>
        implicitly[ReadWriter[R]].write(
          eval(paramIndices.map(i => scope.bindings(i).get.force), ev, fs)
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
                                        (eval: (Map[String, Val], EvalScope) => R): (String, Val.Func) = {
    val indexedParams = params.zipWithIndex.map{case ((k, v), i) => (k, v, i)}.toArray
    val indexedParamKeys = params.zipWithIndex.map{case ((k, v), i) => (k, i)}
    name -> Val.Func(
      None,
      Params(indexedParams),
      { (scope, thisFile, ev, fs, outerOffset) =>
        val args = indexedParamKeys.map {case (k, i) => k -> scope.bindings(i).get.force }.toMap
        implicitly[ReadWriter[R]].write(eval(args, ev))
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


  val paramData = Array("x").zipWithIndex.map{case (k, i) => (k, None, i)}.toArray
  val defaultFunc = Val.Func(
    None,
    Params(paramData),
    {(scope, thisFile, ev, fs, outerOffset) =>
      scope.bindings(0).get.force
    }
  )

}
