package sjsonnet

import java.io.StringWriter
import java.util.Base64

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params
import sjsonnet.Scope.empty

import scala.collection.mutable.ArrayBuffer
import scala.collection.compat._

object Std {
  sealed trait ReadWriter[T]{
    def apply(t: Val, evaluator: EvaluatorApi): Either[String, T]
    def write(t: T): Val
  }
  object ReadWriter{
    implicit object StringRead extends ReadWriter[String]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case Val.Str(s) => Right(s)
        case _ => Left("String")
      }
      def write(t: String) = Val.Str(t)
    }
    implicit object BooleanRead extends ReadWriter[Boolean]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case Val.True => Right(true)
        case Val.False => Right(false)
        case _ => Left("Boolean")
      }
      def write(t: Boolean) = Val.bool(t)
    }
    implicit object IntRead extends ReadWriter[Int]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case Val.Num(s) => Right(s.toInt)
        case _ => Left("Int")
      }
      def write(t: Int) = Val.Num(t)
    }
    implicit object DoubleRead extends ReadWriter[Double]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case Val.Num(s) => Right(s)
        case _ => Left("Number")
      }
      def write(t: Double) = Val.Num(t)
    }
    implicit object ValRead extends ReadWriter[Val]{
      def apply(t: Val, evaluator: EvaluatorApi) = Right(t)
      def write(t: Val) = t
    }
    implicit object ObjRead extends ReadWriter[Val.Obj]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case v: Val.Obj => Right(v)
        case _ => Left("Object")
      }
      def write(t: Val.Obj) = t
    }
    implicit object ArrRead extends ReadWriter[Val.Arr]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case v: Val.Arr => Right(v)
        case _ => Left("Array")
      }
      def write(t: Val.Arr) = t
    }
    implicit object FuncRead extends ReadWriter[Val.Func]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case v: Val.Func => Right(v)
        case _ => Left("Function")
      }
      def write(t: Val.Func) = t
    }

    implicit object ApplyerRead extends ReadWriter[Applyer]{
      def apply(t: Val, evaluator: EvaluatorApi) = t match{
        case v: Val.Func => Right(Applyer(v, evaluator))
        case _ => Left("Function")
      }
      def write(t: Applyer) = t.f
    }
  }
  case class Applyer(f: Val.Func, evaluator: EvaluatorApi){
    def apply(args: Lazy*) = {
      f.apply(args.map((None, _)), "(memory)", evaluator, -1, evaluator.wd / "(memory)")
    }
  }

  def validate(vs: Seq[Val],
               evaluator: EvaluatorApi,
               rs: Seq[ReadWriter[_]]) = {
    for((v, r) <- vs.zip(rs)) yield r.apply(v, evaluator) match{
      case Left(err) => throw new DelegateError("Wrong parameter type: expected " + err + ", got " + v.prettyName)
      case Right(x) => x
    }
  }

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                             (eval:  (EvaluatorApi, T1) => R): (String, Val.Func) = builtin0(name, p1){ (vs, evaluator) =>
    val Seq(v: T1) = validate(vs, evaluator, Seq(implicitly[ReadWriter[T1]]))
    eval(evaluator, v)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                             (eval:  (EvaluatorApi, T1, T2) => R): (String, Val.Func) = builtin0(name, p1, p2){ (vs, evaluator) =>
    val Seq(v1: T1, v2: T2) = validate(vs, evaluator, Seq(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]]))
    eval(evaluator, v1, v2)
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                             (eval:  (EvaluatorApi, T1, T2, T3) => R): (String, Val.Func) = builtin0(name, p1, p2, p3){ (vs, evaluator) =>
    val Seq(v1: T1, v2: T2, v3: T3) = validate(vs, evaluator, Seq(implicitly[ReadWriter[T1]], implicitly[ReadWriter[T2]], implicitly[ReadWriter[T3]]))
    eval(evaluator, v1, v2, v3)
  }
  def builtin0[R: ReadWriter](name: String, params: String*)(eval: (Seq[Val], EvaluatorApi) => R) = {
    name -> Val.Func(
      None,
      Params(params.map(_ -> None)),
      {(scope, thisFile, evaluator, outerOffset) => implicitly[ReadWriter[R]].write(eval(params.map(scope.bindings(_).get.force), evaluator))}
    )
  }
  /**
    * Helper function that can define a built-in function with default parameters
    *
    * Arguments of the eval function are (args, evaluator)
    */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Option[Expr])*)(eval: (Map[String, Val], EvaluatorApi) => R): (String, Val.Func) = {
    name -> Val.Func(
      None,
      Params(params),
      { (scope, thisFile, evaluator, outerOffset) =>
        val args = params.map {case (k, v) => k -> scope.bindings(k).get.force }.toMap
        implicitly[ReadWriter[R]].write(eval(args, evaluator))
      },
      { (expr, scope) =>
        new Evaluator(
          scala.collection.mutable.Map(),
          scope,
          Map(),
          null,
          (_, _) => None
        ).visitExpr(expr, scope)
      }
    )
  }
  val functions: Seq[(String, Val.Func)] = Seq(
    builtin("assertEqual", "a", "b"){ (evaluator, v1: Val, v2: Val) =>
      val x1 = Materializer(v1, evaluator)
      val x2 = Materializer(v2, evaluator)
      if (x1 == x2) true
      else throw new DelegateError("assertEqual failed: " + x1 + " != " + x2)
    },
    builtin("toString", "a"){ (evaluator, v1: Val) =>
      v1 match{
        case Val.Str(s) => s
        case v =>
          Materializer.apply(v, evaluator).transform(new Renderer()).toString
      }
    },
    builtin("codepoint", "str"){ (evaluator, v1: Val) =>
      v1.cast[Val.Str].value.charAt(0).toInt
    },
    builtin("length", "x"){ (evaluator, v1: Val) =>
      v1 match{
        case Val.Str(s) => s.length
        case Val.Arr(s) => s.length
        case o: Val.Obj => o.getVisibleKeys().count(!_._2)
        case o: Val.Func => o.params.args.length
        case _ => throw new DelegateError("Cannot get length of " + v1.prettyName)
      }
    },
    builtin("objectHas", "o", "f"){ (evaluator, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2) == Some(false)
    },
    builtin("objectHasAll", "o", "f"){ (evaluator, v1: Val.Obj, v2: String) =>
      v1.getVisibleKeys().get(v2).isDefined
    },
    builtin("objectFields", "o"){ (evaluator, v1: Val.Obj) =>
      Val.Arr(
        v1.getVisibleKeys()
          .collect{case (k, false) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("objectFieldsAll", "o"){ (evaluator, v1: Val.Obj) =>
      Val.Arr(
        v1.getVisibleKeys()
          .collect{case (k, _) => k}
          .toSeq
          .sorted
          .map(k => Lazy(Val.Str(k)))
      )
    },
    builtin("type", "x"){ (evaluator, v1: Val) =>
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
    builtin("lines", "arr"){ (evaluator, v1: Val.Arr) =>
      v1.value.map(_.force).foreach{
        case _: Val.Str | Val.Null => // donothing
        case x => throw new DelegateError("Cannot call .lines on " + x.prettyName)
      }
      Materializer.apply(v1, evaluator).asInstanceOf[ujson.Arr]
        .value
        .filter(_ != ujson.Null)
        .map{
          case ujson.Str(s) => s + "\n"
          case _ => ??? /* we ensure it's all strings above */
        }
        .mkString
    },
    builtin("format", "str", "vals"){ (evaluator, v1: String, v2: Val) =>
      Format.format(v1, v2, evaluator.memoryScope, -1, evaluator)
    },
    builtin("foldl", "func", "arr", "init"){ (evaluator, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value){
        val c = current
        current = func.apply(Lazy(c), item)
      }
      current
    },
    builtin("foldr", "func", "arr", "init"){ (evaluator, func: Applyer, arr: Val.Arr, init: Val) =>
      var current = init
      for(item <- arr.value.reverse){
        val c = current
        current = func.apply(item, Lazy(c))
      }
      current
    },
    builtin("range", "from", "to"){ (evaluator, from: Int, to: Int) =>
      Val.Arr(
        (from to to).map(i => Lazy(Val.Num(i)))
      )
    },
    builtin("mergePatch", "target", "patch"){ (evaluator, target: Val, patch: Val) =>
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
      Materializer.reverse(rec(Materializer(target, evaluator), Materializer(patch, evaluator)))
    },
    builtin("sqrt", "x"){ (evaluator, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b"){ (evaluator, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b"){ (evaluator, a: Double, b: Double) =>
      math.min(a, b)
    },
    builtin("mod", "a", "b"){ (evaluator, a: Int, b: Int) =>
      a % b
    },

    builtin("makeArray", "sz", "func"){ (evaluator, sz: Int, func: Applyer) =>
      Val.Arr(
        (0 until sz).map(i =>
          Lazy(func.apply(Lazy(Val.Num(i))))
        )
      )
    },

    builtin("pow", "x", "n"){ (evaluator, x: Double, n: Double) =>
      math.pow(x, n)
    },

    builtin("floor", "x"){ (evaluator, x: Double) =>
      math.floor(x)
    },
    builtin("ceil", "x"){ (evaluator, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "x"){ (evaluator, x: Double) =>
      math.abs(x)
    },
    builtin("sin", "x"){ (evaluator, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x"){ (evaluator, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x"){ (evaluator, x: Double) =>
      math.tan(x)
    },

    builtin("asin", "x"){ (evaluator, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x"){ (evaluator, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x"){ (evaluator, x: Double) =>
      math.atan(x)
    },
    builtin("log", "x"){ (evaluator, x: Double) =>
      math.log(x)
    },
    builtin("exp", "x"){ (evaluator, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x"){ (evaluator, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      mantissa
    },
    builtin("exponent", "x"){ (evaluator, x: Double) =>
      val value = x
      val exponent = (Math.log(value) / Math.log(2)).toInt + 1
      val mantissa = value * Math.pow(2.0, -exponent)
      exponent
    },
    builtin("isString", "v"){ (evaluator, v: Val) =>
      v.isInstanceOf[Val.Str]
    },
    builtin("isBoolean", "v"){ (evaluator, v: Val) =>
      v == Val.True || v == Val.False
    },
    builtin("isNumber", "v"){ (evaluator, v: Val) =>
      v.isInstanceOf[Val.Num]
    },
    builtin("isObject", "v"){ (evaluator, v: Val) =>
      v.isInstanceOf[Val.Obj]
    },
    builtin("isArray", "v"){ (evaluator, v: Val) =>
      v.isInstanceOf[Val.Arr]
    },
    builtin("isFunction", "v"){ (evaluator, v: Val) =>
      v.isInstanceOf[Val.Func]
    },
    builtin("count", "arr", "x"){ (evaluator, arr: Val.Arr, x: Val) =>
      val res =  arr.value.count{i =>
        Materializer(i.force, evaluator) == Materializer(x, evaluator)
      }
      res
    },
    builtin("filter", "func", "arr"){ (evaluator, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.filter{ i =>
          func.apply(i) == Val.True
        }
      )
    },
    builtin("map", "func", "arr"){ (evaluator, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.map{ i =>
          Lazy(func.apply(i))
        }
      )
    },
    builtin("mapWithKey", "func", "obj"){ (evaluator, func: Applyer, obj: Val.Obj) =>
      val allKeys = obj.getVisibleKeys()
      Val.Obj(
        allKeys.map{ k =>
          k._1 -> (Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _) => Lazy(
            func.apply(
              Lazy(Val.Str(k._1)),
              obj.value(k._1, evaluator.memoryScope, -1, evaluator)
            )
          )))
        }.toMap,
        _ => (),
        None
      )
    },
    builtin("mapWithIndex", "func", "arr"){ (evaluator, func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.zipWithIndex.map{ case (x, i) =>
          Lazy(func.apply(Lazy(Val.Num(i)), x))
        }
      )
    },
    builtin("filterMap", "filter_func", "map_func", "arr"){ (evaluator, filter_func: Applyer, map_func: Applyer, arr: Val.Arr) =>
      Val.Arr(
        arr.value.flatMap { i =>
          val x = i.force
          if (filter_func.apply(Lazy(x)) != Val.True) None
          else Some(Lazy(map_func.apply(Lazy(x))))
        }
      )
    },
    builtin("find", "value","arr"){ (evaluator, value: Val, arr: Val.Arr) =>
      Val.Arr(
        for (
          (v, i) <- arr.value.zipWithIndex
          if Materializer(v.force, evaluator) == Materializer(value, evaluator)
        ) yield Lazy(Val.Num(i))
      )
    },
    builtin("findSubstr", "pat", "str") { (evaluator, pat: String, str: String) =>
      if (pat.length == 0) Val.Arr(Seq())
      else {
        val indices = ArrayBuffer[Int]()
        var matchIndex = str.indexOf(pat)
        while (0 <= matchIndex && matchIndex < str.length) {
          indices.append(matchIndex)
          matchIndex = str.indexOf(pat, matchIndex + 1)
        }
        Val.Arr(indices.map(x => Lazy(Val.Num(x))).toSeq)
      }
    },
    builtin("substr", "s", "from", "len"){ (evaluator, s: String, from: Int, len: Int) =>
      val safeOffset = math.min(from, s.length)
      val safeLength = math.min(len, s.length - safeOffset)
      s.substring(safeOffset, safeOffset + safeLength)
    },
    builtin("startsWith", "a", "b"){ (evaluator, a: String, b: String) =>
      a.startsWith(b)
    },
    builtin("endsWith", "a", "b"){ (evaluator, a: String, b: String) =>
      a.endsWith(b)
    },
    builtin("char", "n"){ (evaluator, n: Double) =>
      n.toInt.toChar.toString
    },

    builtin("strReplace", "str", "from", "to"){ (evaluator, str: String, from: String, to: String) =>
      str.replace(from, to)
    },
    builtin("join", "sep", "arr"){ (evaluator, sep: Val, arr: Val.Arr) =>
      val res: Val = sep match{
        case Val.Str(s) =>
          Val.Str(
            arr.value
              .map(_.force)
              .filter(_ != Val.Null)
              .map{
                case Val.Str(x) => x
                case x => throw new DelegateError("Cannot join " + x.prettyName)
              }
              .mkString(s)
          )
        case Val.Arr(sep) =>
          val out = collection.mutable.Buffer.empty[Lazy]
          for(x <- arr.value){
            x.force match{
              case Val.Null => // do nothing
              case Val.Arr(v) =>
                if (out.nonEmpty) out.appendAll(sep)
                out.appendAll(v)
              case x => throw new DelegateError("Cannot join " + x.prettyName)
            }
          }
          Val.Arr(out.toSeq)
        case x => throw new DelegateError("Cannot join " + x.prettyName)
      }
      res
    },
    builtin("flattenArrays", "arrs"){ (evaluator, arrs: Val.Arr) =>
      val out = collection.mutable.Buffer.empty[Lazy]
      for(x <- arrs.value){
        x.force match{
          case Val.Null => // do nothing
          case Val.Arr(v) => out.appendAll(v)
          case x => throw new DelegateError("Cannot call flattenArrays on " + x)
        }
      }
      Val.Arr(out.toSeq)
    },
    builtin("manifestIni", "v"){ (evaluator, v: Val) =>
      val materialized = Materializer(v, evaluator)
      def sect(x: ujson.Obj) = {
        x.value.flatMap{
          case (k, ujson.Str(v)) => Seq(k + " = " + v)
          case (k, ujson.Arr(vs)) =>
            vs.map{
              case ujson.Str(v) => k + " = " + v
              case x => throw new DelegateError("Cannot call manifestIni on " + x.getClass)
            }
          case (k, x) => throw new DelegateError("Cannot call manifestIni on " + x.getClass)
        }
      }
      val lines = materialized.obj.get("main").fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Obj])) ++
        materialized.obj.get("sections").fold(Iterable[String]())(x =>
          x.obj.flatMap{case (k, v) => Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Obj])}
        )
      lines.flatMap(Seq(_, "\n")).mkString
    },
    builtin("escapeStringJson", "str"){ (evaluator, str: String) =>
      val out = new StringWriter()
      ujson.Renderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringBash", "str"){ (evaluator, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str"){ (evaluator, str: String) =>
      str.replace("$", "$$")
    },
    builtin("manifestPython", "v"){ (evaluator, v: Val) =>
      Materializer(v, evaluator).transform(new PythonRenderer()).toString
    },
    builtin("manifestJson", "v"){ (evaluator, v: Val) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
      Materializer(v, evaluator).render(indent = 4).replaceAll("\n[ ]+\n", "\n\n")
    },
    builtin("manifestJsonEx", "value", "indent"){ (evaluator, v: Val, i: String) =>
      // account for rendering differences of whitespaces in ujson and jsonnet manifestJsonEx
      Materializer(v, evaluator).render(indent = i.length).replaceAll("\n[ ]+\n", "\n\n")
    },
    builtinWithDefaults("manifestYamlDoc", "v" -> None, "indent_array_in_object" -> Some(Expr.False(0))){ (args, evaluator) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
          case Val.False => false
          case Val.True => true
          case _ => throw DelegateError("indent_array_in_object has to be a boolean, got" + v.getClass)
        }
      Materializer(v, evaluator).transform(new YamlRenderer(indentArrayInObject = indentArrayInObject)).toString
    },
    builtinWithDefaults("manifestYamlStream", "v" -> None, "indent_array_in_object" -> Some(Expr.False(0))){ (args, evaluator) =>
      val v = args("v")
      val indentArrayInObject = args("indent_array_in_object")  match {
        case Val.False => false
        case Val.True => true
        case _ => throw DelegateError("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      v match {
        case Val.Arr(values) => values
          .map { item => Materializer(item.force, evaluator).transform(new YamlRenderer(indentArrayInObject = indentArrayInObject)).toString() }
          .mkString("---\n", "\n---\n", "\n...\n")
        case _ => throw new DelegateError("manifestYamlStream only takes arrays, got " + v.getClass)
      }
    },
    builtin("manifestPythonVars", "v"){ (evaluator, v: Val.Obj) =>
      Materializer(v, evaluator).obj
        .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
        .mkString
    },
    builtin("manifestXmlJsonml", "value"){ (evaluator, value: Val) =>
      import scalatags.Text.all.{value => _, _}


      def rec(v: ujson.Value): Frag = {
        v match {
          case ujson.Str(s) => s
          case ujson.Arr(collection.mutable.Seq(ujson.Str(t), attrs: ujson.Obj, children@_*)) =>
            tag(t)(
              attrs.value.map {
                case (k, ujson.Str(v)) => attr(k) := v
                case (k, v) => throw new DelegateError("Cannot call manifestXmlJsonml on " + v.getClass)
              }.toSeq,
              children.map(rec)
            )
          case ujson.Arr(collection.mutable.Seq(ujson.Str(t), children@_*)) =>
            tag(t)(children.map(rec).toSeq)
          case x =>
            throw new DelegateError("Cannot call manifestXmlJsonml on " + x.getClass)
        }
      }

      rec(Materializer(value, evaluator)).render

    },
    builtin("base64", "v"){ (evaluator, v: Val) =>
      v match{
        case Val.Str(value) => Base64.getEncoder().encodeToString(value.getBytes)
        case Val.Arr(bytes) => Base64.getEncoder().encodeToString(bytes.map(_.force.cast[Val.Num].value.toByte).toArray)
        case x => throw new DelegateError("Cannot base64 encode " + x.prettyName)
      }
    },

    builtin("base64Decode", "s"){ (evaluator, s: String) =>
      new String(Base64.getDecoder().decode(s))
    },
    builtin("base64DecodeBytes", "s"){ (evaluator, s: String) =>
      Val.Arr(Base64.getDecoder().decode(s).map(i => Lazy(Val.Num(i))))
    },
    builtin("sort", "arr"){ (evaluator, arr: Val) =>
      arr match{
        case Val.Arr(vs) =>
          Val.Arr(

            if (vs.forall(_.force.isInstanceOf[Val.Str])){
              vs.map(_.force.cast[Val.Str]).sortBy(_.value).map(Lazy(_))
            }else if (vs.forall(_.force.isInstanceOf[Val.Num])){
              vs.map(_.force.cast[Val.Num]).sortBy(_.value).map(Lazy(_))
            }else {
              ???
            }
          )
        case Val.Str(s) => Val.Arr(s.sorted.map(c => Lazy(Val.Str(c.toString))))
        case x => throw new DelegateError("Cannot sort " + x.prettyName)
      }
    },
    builtin("uniq", "arr"){ (evaluator, arr: Val.Arr) =>
      val ujson.Arr(vs) = Materializer(arr, evaluator)
      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("set", "arr"){ (evaluator, arr: Val.Arr) =>
      val ujson.Arr(vs0) = Materializer(arr, evaluator)
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new DelegateError("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setUnion", "a", "b"){ (evaluator, a: Val.Arr, b: Val.Arr) =>

      val ujson.Arr(vs1) = Materializer(a, evaluator)
      val ujson.Arr(vs2) = Materializer(b, evaluator)
      val vs0 = vs1 ++ vs2
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new DelegateError("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setInter", "a", "b"){ (evaluator, a: Val, b: Val.Arr) =>
      val vs1 = Materializer(a, evaluator) match{
        case ujson.Arr(vs1) => vs1
        case x => Seq(x)
      }
      val ujson.Arr(vs2) = Materializer(b, evaluator)


      val vs0 = vs1.to(collection.mutable.LinkedHashSet)
        .intersect(vs2.to(collection.mutable.LinkedHashSet))
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new DelegateError("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))).toSeq)
    },
    builtin("setDiff", "a", "b"){ (evaluator, a: Val.Arr, b: Val.Arr) =>
      val ujson.Arr(vs1) = Materializer(a, evaluator)
      val ujson.Arr(vs2) = Materializer(b, evaluator)


      val vs0 = vs1.to(collection.mutable.LinkedHashSet)
        .diff(vs2.to(collection.mutable.LinkedHashSet))
        .toSeq
      val vs =
        if (vs0.forall(_.isInstanceOf[ujson.Str])){
          vs0.map(_.asInstanceOf[ujson.Str]).sortBy(_.value)
        }else if (vs0.forall(_.isInstanceOf[ujson.Num])){
          vs0.map(_.asInstanceOf[ujson.Num]).sortBy(_.value)
        }else {
          throw new DelegateError("Every element of the input must be of the same type, string or number")
        }

      val out = collection.mutable.Buffer.empty[ujson.Value]
      for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

      Val.Arr(out.map(v => Lazy(Materializer.reverse(v))).toSeq)

    },
    builtin("setMember", "x", "arr"){ (evaluator, x: Val, arr: Val.Arr) =>
      val vs1 = Materializer(x, evaluator)
      val ujson.Arr(vs2) = Materializer(arr, evaluator)
      vs2.contains(vs1)
    },
    builtin("split", "str", "c"){ (evaluator, str: String, c: String) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), -1).map(s => Lazy(Val.Str(s))))
    },
    builtin("splitLimit", "str", "c", "maxSplits"){ (evaluator, str: String, c: String, maxSplits: Int) =>
      Val.Arr(str.split(java.util.regex.Pattern.quote(c), maxSplits + 1).map(s => Lazy(Val.Str(s))))
    },
    builtin("stringChars", "str"){ (evaluator, str: String) =>

      var offset = 0
      val output = collection.mutable.Buffer.empty[String]
      while (offset < str.length) {
        val codepoint = str.codePointAt(offset)
        output.append(new String(Character.toChars(codepoint)))
        offset += Character.charCount(codepoint)
      }
      Val.Arr(output.map(s => Lazy(Val.Str(s))).toSeq)

    },
    builtin("parseInt", "str"){ (evaluator, str: String) =>
      str.toInt
    },
    builtin("parseOctal", "str"){ (evaluator, str: String) =>
      Integer.parseInt(str, 8)
    },
    builtin("parseHex", "str"){ (evaluator, str: String) =>
      Integer.parseInt(str, 16)
    },
    builtin("parseJson", "str") { (evaluator, str: String) =>

      def recursiveTransform(js: ujson.Value): Val = {
        js match {
          case ujson.Null => Val.Null
          case ujson.True => Val.True
          case ujson.False => Val.False
          case ujson.Num(value) => Val.Num(value)
          case ujson.Str(value) => Val.Str(value)
          case ujson.Arr(values) =>
            val transformedValue: Seq[Lazy] = values.map(v => Lazy(recursiveTransform(v))).toSeq
            Val.Arr(transformedValue)
          case ujson.Obj(valueMap) =>
            val transformedValue = valueMap
              .mapValues { v =>
                Val.Obj.Member(false, Expr.Member.Visibility.Normal, (_, _ ,_) => Lazy(recursiveTransform(v)))
              }.toMap
            Val.Obj(transformedValue , (x: Val.Obj) => (), None)
        }
      }
      recursiveTransform(ujson.read(str))
    },
    builtin("md5", "s"){ (evaluator, s: String) =>
      Platform.md5(s)
    },
    builtin("prune", "x"){ (evaluator, s: Val) =>
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
            v = rec(o.value(k, evaluator.memoryScope, -1, evaluator).force)
            if filter(v)
          }yield (k, Val.Obj.Member(false, Visibility.Normal, (_, _, _) => Lazy(v)))
          Val.Obj(bindings.toMap, _ => (), None)
        case a: Val.Arr =>
          Val.Arr(a.value.map(x => rec(x.force)).filter(filter).map(Lazy(_)))
        case _ => x
      }
      rec(s)
    },

    builtin("asciiUpper", "str"){ (evaluator, str: String) => str.toUpperCase},
    builtin("asciiLower", "str"){ (evaluator, str: String) => str.toLowerCase()},
    "trace" -> Val.Func(
      None,
      Params(Seq("str" -> None, "rest" -> None)),
      { (scope, thisFile, evaluator, outerOffset) =>
        val Val.Str(msg) = scope.bindings("str").get.force
        System.err.println(s"TRACE: $thisFile " + msg)
        scope.bindings("rest").get.force
      }
    ),
    "extVar" -> Val.Func(
      None,
      Params(Seq("x" -> None)),
      { (scope, thisFile, evaluator, outerOffset) =>
        val Val.Str(x) = scope.bindings("x").get.force
        Materializer.reverse(
          evaluator.extVars.getOrElse(
            x,
            throw new DelegateError("Unknown extVar: " + x)
          )
        )
      }
    )
  )
  val Std = Val.Obj(
    functions
      .map{
        case (k, v) =>
          (
            k,
            Val.Obj.Member(
              false,
              Visibility.Hidden,
              (self: Val.Obj, sup: Option[Val.Obj], _) => Lazy(v)
            )
          )
      }
      .toMap ++ Seq(
      (
        "thisFile",
        Val.Obj.Member(
          false,
          Visibility.Hidden,
          { (self: Val.Obj, sup: Option[Val.Obj], scope: ScopeApi) =>
            Lazy(Val.Str(scope.currentFile.relativeToString(scope.currentRoot)))
          },
          cached = false
        )
      )
    ),
    _ => (),
    None
  )
}
