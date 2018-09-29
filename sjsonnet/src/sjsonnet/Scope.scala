package sjsonnet

import java.io.StringWriter
import java.util.Base64

import ammonite.ops.Path
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params


object Scope{
  def builtin(name: String, params: String*)(eval: Seq[Val] => Val) = {
    name -> Val.Func(
      empty,
      Params(params.map(_ -> None)),
      {scope => eval(params.map(scope.bindings(_).get.force))}
    )
  }
  val functions = Seq[(String, Val.Func)](
    builtin("assertEqual", "a", "b"){ case Seq(v1, v2) =>
      val x1 = Materializer(v1)
      val x2 = Materializer(v2)
      if (x1 == x2) Val.True
      else throw new DelegateError("assertEqual failed: " + x1 + " != " + x2)
    },
    builtin("toString", "a"){ case Seq(v1) =>
      v1 match{
        case Val.Str(s) => Val.Str(s)
        case v =>
          Val.Str(Materializer.apply(v).transform(new Renderer()).toString)
      }
    },
    builtin("codepoint", "str"){ case Seq(v1) =>
      Val.Num(v1.asInstanceOf[Val.Str].value.charAt(0).toInt)
    },
    builtin("length", "x"){ case Seq(v1) =>
      Val.Num(
        v1 match{
          case Val.Str(s) => s.length
          case Val.Arr(s) => s.length
          case o: Val.Obj => o.getVisibleKeys().count(!_._2)
          case o: Val.Func => o.params.args.length
        }
      )
    },
    builtin("objectHas", "o", "f"){ case Seq(v1: Val.Obj, v2: Val.Str) =>
      Val.bool(v1.getVisibleKeys().get(v2.value) == Some(false))
    },
    builtin("objectHasAll", "o", "f"){ case Seq(v1: Val.Obj, v2: Val.Str) =>
      Val.bool(v1.getVisibleKeys().get(v2.value).isDefined)
    },
    builtin("objectFields", "o"){ case Seq(v1: Val.Obj) =>
      Val.Arr(
        v1.asInstanceOf[Val.Obj]
          .getVisibleKeys()
          .collect{case (k, false) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("objectFieldsAll", "o"){ case Seq(v1: Val.Obj) =>
      Val.Arr(
        v1.asInstanceOf[Val.Obj]
          .getVisibleKeys()
          .collect{case (k, _) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("type", "x"){ case Seq(v1) =>
      Val.Str(
        v1 match{
          case Val.True | Val.False => "boolean"
          case Val.Null => "null"
          case _: Val.Obj => "object"
          case _: Val.Arr => "array"
          case _: Val.Func => "function"
          case _: Val.Num => "number"
          case _: Val.Str => "string"
        }
      )
    },
    builtin("lines", "arr"){ case Seq(v1: Val.Arr) =>
      Val.Str(
        Materializer.apply(v1).asInstanceOf[ujson.Js.Arr]
          .value
          .filter(_ != ujson.Js.Null)
          .map{case ujson.Js.Str(s) => s + "\n"}
          .mkString
      )
    },
    builtin("format", "str", "vals"){ case Seq(v1: Val.Str, v2) =>
      val formatStr = v1.value
      Val.Str(Format.format(formatStr, v2, ammonite.ops.pwd / "(unknown)", -1))
    },
    builtin("foldl", "func", "arr", "init"){ case Seq(func: Val.Func, arr: Val.Arr, init) =>
      var current = init
      for(item <- arr.value){
        val c = current
        current = func.apply(Seq((None, Lazy(c)), (None, item)), -1)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ case Seq(func: Val.Func, arr: Val.Arr, init) =>
      var current = init
      for(item <- arr.value.reverse){
        val c = current
        current = func.apply(Seq((None, item), (None, Lazy(c))), -1)
      }
      current
    },
    builtin("range", "from", "to"){ case Seq(from: Val.Num, to: Val.Num) =>
      Val.Arr(
        (from.asInstanceOf[Val.Num].value.toInt to
          to.asInstanceOf[Val.Num].value.toInt)
          .map(i => Lazy(Val.Num(i)))
      )
    },
    builtin("mergePatch", "target", "patch"){ case Seq(target, patch) =>
      def rec(l: ujson.Js, r: ujson.Js): ujson.Js = {
        (l, r) match{
          case (l0, r: ujson.Js.Obj) =>
            val l = l0 match{
              case l: ujson.Js.Obj => l
              case _ => ujson.Js.Obj()
            }
            for((k, v) <- r.value){
              if (v == ujson.Js.Null) l.value.remove(k)
              else if (l.value.contains(k)) l(k) = rec(l(k), r(k))
              else l(k) = rec(ujson.Js.Obj(), r(k))
            }
            l
          case (_, _) => r
        }
      }
      Materializer.reverse(rec(Materializer(target), Materializer(patch)))
    },
    builtin("sqrt", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.sqrt(x.asInstanceOf[Val.Num].value))
    },

    builtin("makeArray", "sz", "func"){ case Seq(sz: Val.Num, func: Val.Func) =>
      Val.Arr(
        (0 until sz.asInstanceOf[Val.Num].value.toInt).map(i =>
          Lazy(func.asInstanceOf[Val.Func].apply(Seq(None -> Lazy(Val.Num(i))), -1))
        )
      )
    },

    builtin("pow", "x", "n"){ case Seq(x: Val.Num, n: Val.Num) =>
      Val.Num(math.pow(x.value, n.value))
    },

    builtin("floor", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.floor(x.value))
    },
    builtin("ceil", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.ceil(x.value))
    },
    builtin("abs", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.abs(x.value))
    },
    builtin("sin", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.sin(x.value))
    },
    builtin("cos", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.cos(x.value))
    },
    builtin("tan", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.tan(x.value))
    },

    builtin("asin", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.asin(x.value))
    },
    builtin("acos", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.acos(x.value))
    },
    builtin("atan", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.atan(x.value))
    },
    builtin("log", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.log(x.value))
    },
    builtin("exp", "x"){ case Seq(x: Val.Num) =>
      Val.Num(math.exp(x.value))
    },
    builtin("mantissa", "x"){ case Seq(x: Val.Num) =>
      val value = x.value
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      Val.Num(mantissa)
    },
    builtin("exponent", "x"){ case Seq(x: Val.Num) =>
      val value = x.value
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      Val.Num(exponent)
    },
    builtin("isString", "v"){ case Seq(v) =>
      Val.bool(v.isInstanceOf[Val.Str])
    },
    builtin("isBoolean", "v"){ case Seq(v) =>
      Val.bool(v == Val.True || v == Val.False)
    },
    builtin("isNumber", "v"){ case Seq(v) =>
      Val.bool(v.isInstanceOf[Val.Num])
    },
    builtin("isObject", "v"){ case Seq(v) =>
      Val.bool(v.isInstanceOf[Val.Obj])
    },
    builtin("isArray", "v"){ case Seq(v) =>
      Val.bool(v.isInstanceOf[Val.Arr])
    },
    builtin("isFunction", "v"){ case Seq(v) =>
      Val.bool(v.isInstanceOf[Val.Func])
    },
    builtin("count", "arr", "x"){ case Seq(arr: Val.Arr, x) =>
      val res =  arr.value.count{i =>
        Materializer(i.force) == Materializer(x)
      }
      Val.Num(res)
    },
    builtin("filter", "func", "arr"){ case Seq(func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.filter{ i =>
          func.apply(Seq(None -> i), -1) == Val.True
        }
      )
    },
    builtin("map", "func", "arr"){ case Seq(func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.map{ i =>
          Lazy(func.apply(Seq(None -> i), -1))
        }
      )
    },
    builtin("mapWithKey", "func", "obj"){ case Seq(func: Val.Func, obj: Val.Obj) =>
      val allKeys = obj.getVisibleKeys()
      Val.Obj(
        allKeys.map{ k =>
          k._1 -> (Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(
            func.asInstanceOf[Val.Func].apply(
              Seq(None -> Lazy(Val.Str(k._1)), None -> obj.value(k._1, ammonite.ops.pwd / "(Unknown)", -1)),
              -1
            )
          )))
        }.toMap,
        _ => (),
        None
      )
    },
    builtin("mapWithIndex", "func", "arr"){ case Seq(func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.zipWithIndex.map{ case (i, i2) =>
          Lazy(func.apply(Seq(None -> i, None -> Lazy(Val.Num(i2))), -1))
        }
      )
    },
    builtin("filterMap", "filter_func", "map_func", "arr"){ case Seq(filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.flatMap { i =>
          val x = i.force
          if (filter_func.apply(Seq(None -> Lazy(x)), -1) != Val.True) None
          else Some(Lazy(map_func.apply(Seq(None -> Lazy(x)), -1)))
        }
      )
    },
    builtin("substr", "s", "from", "len"){ case Seq(s: Val.Str, from: Val.Num, len: Val.Num) =>
      Val.Str(s.value.substring(
        from.value.toInt,
        len.value.toInt + 1
      ))
    },
    builtin("startsWith", "a", "b"){ case Seq(a: Val.Str, b: Val.Str) =>
      Val.bool(a.value.startsWith(b.value))
    },
    builtin("endsWith", "a", "b"){ case Seq(a: Val.Str, b: Val.Str) =>
      Val.bool(a.value.endsWith(b.value))
    },
    builtin("char", "n"){ case Seq(n: Val.Num) =>
      Val.Str(n.value.toInt.toChar.toString)
    },

    builtin("strReplace", "str", "from", "to"){ case Seq(str: Val.Str, from: Val.Str, to: Val.Str) =>
      Val.Str(str.value.replace(from.value, to.value))
    },
    builtin("join", "sep", "arr"){ case Seq(sep, arr) =>
      sep match{
        case Val.Str(s) =>
          Val.Str(arr.asInstanceOf[Val.Arr].value.map(_.force).filter(_ != Val.Null).map{case Val.Str(x) => x}.mkString(s))
        case Val.Arr(sep) =>
          val out = collection.mutable.Buffer.empty[Lazy]
          for(x <- arr.asInstanceOf[Val.Arr].value){
            x.force match{
              case Val.Null => // do nothing
              case Val.Arr(v) =>
                if (out.nonEmpty) out.appendAll(sep)
                out.appendAll(v)
            }
          }
          Val.Arr(out)
      }

    },
    builtin("flattenArrays", "arrs"){ case Seq(arrs: Val.Arr) =>
      val out = collection.mutable.Buffer.empty[Lazy]
      for(x <- arrs.value){
        x.force match{
          case Val.Null => // do nothing
          case Val.Arr(v) => out.appendAll(v)
        }
      }
      Val.Arr(out)
    },
    builtin("manifestIni", "v"){ case Seq(v) =>
      val materialized = Materializer(v)
      def sect(x: ujson.Js.Obj) = {
        x.value.flatMap{
          case (k, ujson.Js.Str(v)) => Seq(k + " = " + v)
          case (k, ujson.Js.Arr(vs)) => vs.map{case ujson.Js.Str(v) => k + " = " + v}
        }
      }
      val lines = materialized.obj.get("main").fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Js.Obj])) ++
        materialized.obj.get("sections").fold(Iterable[String]())(x =>
          x.obj.flatMap{case (k, v) => Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Js.Obj])}
        )
      Val.Str(lines.flatMap(Seq(_, "\n")).mkString)
    },
    builtin("escapeStringJson", "str"){ case Seq(str: Val.Str) =>
      val v = str.value
      val out = new StringWriter()
      ujson.Renderer.escape(out, v, unicode = true)
      Val.Str(out.toString)
    },
    builtin("escapeStringBash", "str"){ case Seq(str: Val.Str) =>
      val v = str.value
      Val.Str("'" + v.replace("'", """'"'"'""") + "'")
    },
    builtin("escapeStringDollars", "str"){ case Seq(str: Val.Str) =>
      val v = str.value
      Val.Str(v.replace("$", "$$"))
    },
    builtin("manifestPython", "v"){ case Seq(v) =>
      Val.Str(Materializer(v).transform(new PythonRenderer()).toString)
    },
    builtin("manifestJson", "v"){ case Seq(v) =>
      Val.Str(Materializer(v).render(indent = 4))
    },
    builtin("manifestPythonVars", "v"){ case Seq(v: Val.Obj) =>
      Val.Str(
        Materializer(v).obj
          .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
          .mkString
      )
    },
    builtin("manifestXmlJsonml", "value"){ case Seq(value) =>
      import scalatags.Text.all.{value => _, _}


      def rec(v: ujson.Js): Frag = {
        v match {
          case ujson.Js.Str(s) => s
          case ujson.Js.Arr(Seq(ujson.Js.Str(t), attrs: ujson.Js.Obj, children@_*)) =>
            tag(t)(
              attrs.value.map { case (k, ujson.Js.Str(v)) => attr(k) := v }.toSeq,
              children.map(rec)
            )
          case ujson.Js.Arr(Seq(ujson.Js.Str(t), children@_*)) =>
            tag(t)(children.map(rec))
        }
      }

      Val.Str(rec(Materializer(value)).render)

    },
    builtin("base64", "v"){ case Seq(v) =>
      Val.Str(
        v match{
          case Val.Str(value) => Base64.getEncoder().encodeToString(value.getBytes)
          case Val.Arr(bytes) => Base64.getEncoder().encodeToString(bytes.map(_.force.asInstanceOf[Val.Num].value.toByte).toArray)
        }
      )
    },

    builtin("base64Decode", "s"){ case Seq(s: Val.Str) =>
      Val.Str(
        new String(Base64.getDecoder().decode(s.value))
      )
    },
    builtin("base64DecodeBytes", "s"){ case Seq(s: Val.Str) =>
      Val.Arr(Base64.getDecoder().decode(s.value).map(i => Lazy(Val.Num(i))))
    },
    builtin("sort", "arr"){ case Seq(arr: Val.Arr) =>

      val vs = arr.value
      Val.Arr(

        if (vs.forall(_.force.isInstanceOf[Val.Str])){
          vs.map(_.force.asInstanceOf[Val.Str]).sortBy(_.value).map(Lazy(_))
        }else if (vs.forall(_.force.isInstanceOf[Val.Num])){
          vs.map(_.force.asInstanceOf[Val.Num]).sortBy(_.value).map(Lazy(_))
        }else {
          ???
        }
      )
    },
    builtin("uniq", "arr"){ case Seq(arr: Val.Arr) =>
      val ujson.Js.Arr(vs) = Materializer(arr)
      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))
    },
    builtin("set", "arr"){ case Seq(arr: Val.Arr) =>
      val ujson.Js.Arr(vs0) = Materializer(arr)
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
          vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
          vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
        }else ???

      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))
    },
    builtin("setUnion", "a", "b"){ case Seq(a: Val.Arr, b: Val.Arr) =>

      val ujson.Js.Arr(vs1) = Materializer(a)
      val ujson.Js.Arr(vs2) = Materializer(b)
      val vs0 = vs1 ++ vs2
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
          vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
          vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
        }else ???

      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))
    },
    builtin("setInter", "a", "b"){ case Seq(a, b: Val.Arr) =>
      val vs1 = Materializer(a) match{
        case ujson.Js.Arr(vs1) => vs1
        case x => Seq(x)
      }
      val ujson.Js.Arr(vs2) = Materializer(b)


      val vs0 = vs1.to[collection.mutable.LinkedHashSet]
        .intersect(vs2.to[collection.mutable.LinkedHashSet])
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
          vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
          vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
        }else ???

      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))
    },
    builtin("setDiff", "a", "b"){ case Seq(a: Val.Arr, b: Val.Arr) =>
      val ujson.Js.Arr(vs1) = Materializer(a)
      val ujson.Js.Arr(vs2) = Materializer(b)


      val vs0 = vs1.to[collection.mutable.LinkedHashSet]
        .diff(vs2.to[collection.mutable.LinkedHashSet])
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
          vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
          vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
        }else ???

      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))

    },
    builtin("setMember", "x", "arr"){ case Seq(x, arr: Val.Arr) =>
      val vs1 = Materializer(x)
      val ujson.Js.Arr(vs2) = Materializer(arr)
      Val.bool(vs2.contains(vs1))
    },
    builtin("split", "str", "c"){ case Seq(str: Val.Str, c: Val.Str) =>
      Val.Arr(str.value.split(java.util.regex.Pattern.quote(c.value), -1).map(s => Lazy(Val.Str(s))))
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ case Seq(str: Val.Str, c: Val.Str, maxSplits: Val.Num) =>
      Val.Arr(str.value.split(java.util.regex.Pattern.quote(c.value), maxSplits.value.toInt + 1).map(s => Lazy(Val.Str(s))))
    },
    builtin("stringChars", "str"){ case Seq(str: Val.Str) =>

      var offset = 0
      val output = collection.mutable.Buffer.empty[String]
      while (offset < str.value.length) {
        val codepoint = str.value.codePointAt(offset)
        output.append(new String(Character.toChars(codepoint)))
        offset += Character.charCount(codepoint)
      }
      Val.Arr(output.map(s => Lazy(Val.Str(s))))

    },
    builtin("parseInt", "str"){ case Seq(str: Val.Str) =>
      Val.Num(str.value.toInt)
    },
    builtin("parseOctal", "str"){ case Seq(str: Val.Str) =>
      Val.Num(Integer.parseInt(str.value, 8))
    },
    builtin("parseHex", "str"){ case Seq(str: Val.Str) =>
      Val.Num(Integer.parseInt(str.value, 16))
    },
  )
  val Std = Val.Obj(
    functions
      .map{case (k, v) => (k, Val.Obj.Member(false, Visibility.Hidden, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(v)))}
      .toMap,
    _ => (),
    None
  )

  def empty = new Scope(None, None, None, Map.empty, ammonite.ops.pwd / "(memory)", List(), None)
  def standard(p: Path, s: List[Path]) = new Scope(None, None, None, Map("std" -> Lazy(Scope.Std)), p, s, None)
}

case class Scope(dollar0: Option[Val.Obj],
                 self0: Option[Val.Obj],
                 super0: Option[Val.Obj],
                 bindings0: Map[String, Lazy],
                 fileName: Path,
                 searchRoots: List[Path],
                 delegate: Option[Scope]){
  def dollar = dollar0.get
  def self = self0.get
  val bindingCache = collection.mutable.Map.empty[String, Option[Lazy]]
  def bindings(k: String): Option[Lazy] = bindingCache.getOrElseUpdate(
    k,
    bindings0.get(k).orElse(delegate.flatMap(_.bindings(k)))
  )
  def ++(traversableOnce: TraversableOnce[(String, (Val.Obj, Option[Val.Obj]) => Lazy)]) = {
    new Scope(
      dollar0,
      self0,
      super0,
      traversableOnce.map{case (k, v) => (k, v.apply(self0.getOrElse(null), super0))}.toMap,
      fileName,
      searchRoots,
      Some(this)
    )
  }
}