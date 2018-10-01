package sjsonnet

import java.io.StringWriter
import java.util.Base64

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params
import sjsonnet.Scope.empty

object Std {
  sealed trait ReadWriter[T]{
    def apply(t: Val): Either[String, T]
    def write(t: T): Val
  }
  object ReadWriter{
    implicit object StringRead extends ReadWriter[String]{
      def apply(t: Val) = t match{
        case Val.Str(s) => Right(s)
        case _ => Left("String")
      }
      def write(t: String) = Val.Str(t)
    }
    implicit object BooleanRead extends ReadWriter[Boolean]{
      def apply(t: Val) = t match{
        case Val.True => Right(true)
        case Val.False => Right(false)
        case _ => Left("Boolean")
      }
      def write(t: Boolean) = Val.bool(t)
    }
    implicit object IntRead extends ReadWriter[Int]{
      def apply(t: Val) = t match{
        case Val.Num(s) => Right(s.toInt)
        case _ => Left("Int")
      }
      def write(t: Int) = Val.Num(t)
    }
    implicit object DoubleRead extends ReadWriter[Double]{
      def apply(t: Val) = t match{
        case Val.Num(s) => Right(s)
        case _ => Left("Number")
      }
      def write(t: Double) = Val.Num(t)
    }
    implicit object ValRead extends ReadWriter[Val]{
      def apply(t: Val) = Right(t)
      def write(t: Val) = t
    }
    implicit object ObjRead extends ReadWriter[Val.Obj]{
      def apply(t: Val) = t match{
        case v: Val.Obj => Right(v)
        case _ => Left("Object")
      }
      def write(t: Val.Obj) = t
    }
    implicit object ArrRead extends ReadWriter[Val.Arr]{
      def apply(t: Val) = t match{
        case v: Val.Arr => Right(v)
        case _ => Left("Array")
      }
      def write(t: Val.Arr) = t
    }
    implicit object FuncRead extends ReadWriter[Val.Func]{
      def apply(t: Val) = t match{
        case v: Val.Func => Right(v)
        case _ => Left("Function")
      }
      def write(t: Val.Func) = t
    }
  }

  def validate(vs: Seq[Val], rs: Seq[ReadWriter[_]]) = {
    for((v, r) <- vs.zip(rs)) yield r.apply(v) match{
      case Left(err) => throw new DelegateError("Wrong parameter type: expected " + err + ", got " + v.prettyName)
      case Right(x) => x
    }
  }

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                             (eval: T1 => R): (String, Val.Func) = builtin0(name, p1){ vs =>
    val Seq(v: T1) = validate(vs, Seq(implicitly[ReadWriter[T1]]))
    eval(v)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                             (eval: (T1, T2) => R): (String, Val.Func) = builtin0(name, p1, p2){ vs =>
    val Seq(v1: T1, v2: T2) = validate(vs, Seq(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]]))
    eval(v1, v2)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                             (eval: (T1, T2, T3) => R): (String, Val.Func) = builtin0(name, p1, p2, p3){ vs =>
    val Seq(v1: T1, v2: T2, v3: T3) = validate(vs, Seq(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]], implicitly[ReadWriter[T3]]))
    eval(v1, v2, v3)
  }
  def builtin0[R: ReadWriter](name: String, params: String*)(eval: Seq[Val] => R) = {
    name -> Val.Func(
      empty,
      Params(params.map(_ -> None)),
      {scope => implicitly[ReadWriter[R]].write(eval(params.map(scope.bindings(_).get.force)))}
    )
  }
  val functions = Seq[(String, Val.Func)](
    builtin("assertEqual", "a", "b"){ (v1: Val, v2: Val) =>
      val x1 = Materializer(v1)
      val x2 = Materializer(v2)
      if (x1 == x2) true
      else throw new DelegateError("assertEqual failed: " + x1 + " != " + x2)
    },
    builtin("toString", "a"){ (v1: Val) =>
      v1 match{
        case Val.Str(s) => s
        case v =>
          Materializer.apply(v).transform(new Renderer()).toString
      }
    },
    builtin("codepoint", "str"){ (v1: Val) =>
      v1.asInstanceOf[Val.Str].value.charAt(0).toInt
    },
    builtin("length", "x"){ (v1: Val) =>
      v1 match{
        case Val.Str(s) => s.length
        case Val.Arr(s) => s.length
        case o: Val.Obj => o.getVisibleKeys().count(!_._2)
        case o: Val.Func => o.params.args.length
      }
    },
    builtin("objectHas", "o", "f"){ (v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2) == Some(false)
    },
    builtin("objectHasAll", "o", "f"){ (v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2).isDefined
    },
    builtin("objectFields", "o"){ (v1: Val.Obj) =>
      Val.Arr(
        v1.asInstanceOf[Val.Obj]
          .getVisibleKeys()
          .collect{case (k, false) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("objectFieldsAll", "o"){ (v1: Val.Obj) =>
      Val.Arr(
        v1.asInstanceOf[Val.Obj]
          .getVisibleKeys()
          .collect{case (k, _) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("type", "x"){ (v1: Val) =>
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
    builtin("lines", "arr"){ (v1: Val.Arr) =>
      Materializer.apply(v1).asInstanceOf[ujson.Js.Arr]
        .value
        .filter(_ != ujson.Js.Null)
        .map{case ujson.Js.Str(s) => s + "\n"}
        .mkString
    },
    builtin("format", "str", "vals"){ (v1: String, v2: Val) =>
      Format.format(v1, v2, ammonite.ops.pwd / "(unknown)", -1)
    },
    builtin("foldl", "func", "arr", "init"){ (func: Val.Func, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value){
        val c = current
        current = func.apply(Seq((None, Lazy(c)), (None, item)), -1)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ (func: Val.Func, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value.reverse){
        val c = current
        current = func.apply(Seq((None, item), (None, Lazy(c))), -1)
      }
      current
    },
    builtin("range", "from", "to"){ (from: Int, to: Int) =>
      Val.Arr(
        (from to to).map(i => Lazy(Val.Num(i)))
      )
    },
    builtin("mergePatch", "target", "patch"){ (target: Val, patch: Val) =>
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
    builtin("sqrt", "x"){ (x: Double) =>
      math.sqrt(x)
    },

    builtin("makeArray", "sz", "func"){ (sz: Int, func: Val.Func) =>
      Val.Arr(
        (0 until sz).map(i =>
          Lazy(func.apply(Seq(None -> Lazy(Val.Num(i))), -1))
        )
      )
    },

    builtin("pow", "x", "n"){ (x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    builtin("isString", "v"){ (v: Val) =>
      v.isInstanceOf[Val.Str]
    },
    builtin("isBoolean", "v"){ (v: Val) =>
      v == Val.True || v == Val.False
    },
    builtin("isNumber", "v"){ (v: Val) =>
      v.isInstanceOf[Val.Num]
    },
    builtin("isObject", "v"){ (v: Val) =>
      v.isInstanceOf[Val.Obj]
    },
    builtin("isArray", "v"){ (v: Val) =>
      v.isInstanceOf[Val.Arr]
    },
    builtin("isFunction", "v"){ (v: Val) =>
      v.isInstanceOf[Val.Func]
    },
    builtin("count", "arr", "x"){ (arr: Val.Arr, x: Val) =>
      val res =  arr.value.count{i =>
        Materializer(i.force) == Materializer(x)
      }
      res
    },
    builtin("filter", "func", "arr"){ (func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.filter{ i =>
          func.apply(Seq(None -> i), -1) == Val.True
        }
      )
    },
    builtin("map", "func", "arr"){ (func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.map{ i =>
          Lazy(func.apply(Seq(None -> i), -1))
        }
      )
    },
    builtin("mapWithKey", "func", "obj"){ (func: Val.Func, obj: Val.Obj) =>
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
    builtin("mapWithIndex", "func", "arr"){ (func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.zipWithIndex.map{ case (i, i2) =>
          Lazy(func.apply(Seq(None -> i, None -> Lazy(Val.Num(i2))), -1))
        }
      )
    },
    builtin("filterMap", "filter_func", "map_func", "arr"){ (filter_func: Val.Func, map_func: Val.Func, arr: Val.Arr) =>
      Val.Arr(
        arr.value.flatMap { i =>
          val x = i.force
          if (filter_func.apply(Seq(None -> Lazy(x)), -1) != Val.True) None
          else Some(Lazy(map_func.apply(Seq(None -> Lazy(x)), -1)))
        }
      )
    },
    builtin("substr", "s", "from", "len"){ (s: String, from: Int, len: Int) =>
      s.substring(from, len + 1)
    },
    builtin("startsWith", "a", "b"){ (a: String, b: String) =>
      a.startsWith(b)
    },
    builtin("endsWith", "a", "b"){ (a: String, b: String) =>
      a.endsWith(b)
    },
    builtin("char", "n"){ (n: Double) =>
      n.toInt.toChar.toString
    },

    builtin("strReplace", "str", "from", "to"){ (str: String, from: String, to: String) =>
      str.replace(from, to)
    },
    builtin("join", "sep", "arr"){ (sep: Val, arr: Val.Arr) =>
      val res: Val = sep match{
        case Val.Str(s) =>
          Val.Str(arr.value.map(_.force).filter(_ != Val.Null).map{case Val.Str(x) => x}.mkString(s))
        case Val.Arr(sep) =>
          val out = collection.mutable.Buffer.empty[Lazy]
          for(x <- arr.value){
            x.force match{
              case Val.Null => // do nothing
              case Val.Arr(v) =>
                if (out.nonEmpty) out.appendAll(sep)
                out.appendAll(v)
            }
          }
          Val.Arr(out)
      }
      res
    },
    builtin("flattenArrays", "arrs"){ (arrs: Val.Arr) =>
      val out = collection.mutable.Buffer.empty[Lazy]
      for(x <- arrs.value){
        x.force match{
          case Val.Null => // do nothing
          case Val.Arr(v) => out.appendAll(v)
        }
      }
      Val.Arr(out)
    },
    builtin("manifestIni", "v"){ (v: Val) =>
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
      lines.flatMap(Seq(_, "\n")).mkString
    },
    builtin("escapeStringJson", "str"){ (str: String) =>
      val out = new StringWriter()
      ujson.Renderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (v: Val) =>
      Materializer(v).transform(new PythonRenderer()).toString
    },
    builtin("manifestJson", "v"){ (v: Val) =>
      Materializer(v).render(indent = 4)
    },
    builtin("manifestPythonVars", "v"){ (v: Val.Obj) =>
      Materializer(v).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (value: Val) =>
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

      rec(Materializer(value)).render

    },
    builtin("base64", "v"){ (v: Val) =>
      v match{
        case Val.Str(value) => Base64.getEncoder().encodeToString(value.getBytes)
        case Val.Arr(bytes) => Base64.getEncoder().encodeToString(bytes.map(_.force.asInstanceOf[Val.Num].value.toByte).toArray)
      }
    },

    builtin("base64Decode", "s"){ (s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (s: String) =>
      Val.Arr(Base64.getDecoder().decode(s).map(i => Lazy(Val.Num(i))))
    },
    builtin("sort", "arr"){ (arr: Val.Arr) =>

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
    builtin("uniq", "arr"){ (arr: Val.Arr) =>
      val ujson.Js.Arr(vs) = Materializer(arr)
      val out = collection.mutable.Buffer.empty[ujson.Js]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))))
    },
    builtin("set", "arr"){ (arr: Val.Arr) =>
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
    builtin("setUnion", "a", "b"){ (a: Val.Arr, b: Val.Arr) =>

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
    builtin("setInter", "a", "b"){ (a: Val, b: Val.Arr) =>
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
    builtin("setDiff", "a", "b"){ (a: Val.Arr, b: Val.Arr) =>
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
    builtin("setMember", "x", "arr"){ (x: Val, arr: Val.Arr) =>
      val vs1 = Materializer(x)
      val ujson.Js.Arr(vs2) = Materializer(arr)
      vs2.contains(vs1)
    },
    builtin("split", "str", "c"){ (str: String, c: String) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), -1).map(s => Lazy(Val.Str(s))))
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ (str: String, c: String, maxSplits: Int) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => Lazy(Val.Str(s))))
    },
    builtin("stringChars", "str"){ (str: String) =>

      var offset = 0
      val output = collection.mutable.Buffer.empty[String]
      while (offset < str.length) {
        val codepoint = str.codePointAt(offset)
        output.append(new String(Character.toChars(codepoint)))
        offset += Character.charCount(codepoint)
      }
      Val.Arr(output.map(s => Lazy(Val.Str(s))))

    },
    builtin("parseInt", "str"){ (str: String) =>
      str.toInt
    },
    builtin("parseOctal", "str"){ (str: String) =>
      Integer.parseInt(str, 8)
    },
    builtin("parseHex", "str"){ (str: String) =>
      Integer.parseInt(str, 16)
    },
    builtin("trace", "str", "rest"){ (str: String, rest: Val) =>
      println("TRACE: " + str)
      rest
    },
  )
  val Std = Val.Obj(
    functions
      .map{case (k, v) => (k, Val.Obj.Member(false, Visibility.Hidden, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(v)))}
      .toMap,
    _ => (),
    None
  )
}
