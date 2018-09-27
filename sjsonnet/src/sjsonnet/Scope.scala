package sjsonnet

import java.io.StringWriter
import java.util.Base64

import ammonite.ops.Path


object Scope{
  val Empty = new Scope(None, None, None, Map.empty, ammonite.ops.pwd)
  val functions = Seq[(String, Val.Func)](
    "assertEqual" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        val x1 = Materializer(v1.calc)
        val x2 = Materializer(v2.calc)
        if (x1 == x2) Val.True
        else throw new Exception("assertEqual failed: " + x1 + " != " + x2)
    }),
    "toString" -> Val.Func(1, {case Seq((None, v1)) =>
        v1.calc match{
          case Val.Str(s) => Val.Str(s)
          case v =>
            Val.Str(Materializer.apply(v).transform(new Renderer()).toString)
        }
    }),
    "codepoint" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Num(v1.calc.asInstanceOf[Val.Str].value.charAt(0).toInt)
    }),
    "length" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Num(
          v1.calc match{
            case Val.Str(s) => s.length
            case Val.Arr(s) => s.length
            case o: Val.Obj => o.getVisibleKeys().count(!_._2)
            case o: Val.Func => o.length
          }
        )
    }),
    "objectHas" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        if (v1.calc.asInstanceOf[Val.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Val.Str].value) == Some(false)){
          Val.True
        } else Val.False
    }),
    "objectHasAll" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        if (v1.calc.asInstanceOf[Val.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Val.Str].value).isDefined){
          Val.True
        } else Val.False
    }),
    "objectFields" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Arr(
          v1.calc.asInstanceOf[Val.Obj]
            .getVisibleKeys()
            .collect{case (k, false) => Ref(Val.Str(k))}
            .toSeq
        )
    }),
    "objectFieldsAll" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Arr(
          v1.calc.asInstanceOf[Val.Obj]
            .getVisibleKeys()
            .collect{case (k, _) => Ref(Val.Str(k))}
            .toSeq
        )
    }),
    "type" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Str(
          v1.calc match{
            case Val.True | Val.False => "boolean"
            case Val.Null => "null"
            case _: Val.Obj => "object"
            case _: Val.Arr => "array"
            case _: Val.Func => "function"
            case _: Val.Num => "number"
            case _: Val.Str => "string"
          }
        )
    }),
    "lines" -> Val.Func(1, {case Seq((None, v1)) =>
        Val.Str(
          Materializer.apply(v1.calc).asInstanceOf[ujson.Js.Arr]
            .value
            .filter(_ != ujson.Js.Null)
            .map{case ujson.Js.Str(s) => s + "\n"}
            .mkString
        )
    }),
    "format" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        val formatStr = v1.calc.asInstanceOf[Val.Str].value

        val items = Materializer(v2.calc) match{
          case x: ujson.Js.Arr => x.value
          case x => Seq(x)
        }
        Val.Str(Format.format(formatStr, items))
    }),
    "foldl" -> Val.Func(3, {case Seq((None, func), (None, cases), (None, start)) =>
        var current = start.calc
        for(item <- cases.calc.asInstanceOf[Val.Arr].value){
          val c = current
          current = func.calc.asInstanceOf[Val.Func].value(Seq((None, Ref(c)), (None, item)))
        }
        current
    }),
    "foldr" -> Val.Func(3, {case Seq((None, func), (None, cases), (None, start)) =>
        var current = start.calc
        for(item <- cases.calc.asInstanceOf[Val.Arr].value.reverse){
          val c = current
          current = func.calc.asInstanceOf[Val.Func].value(Seq((None, item), (None, Ref(c))))
        }
        current
    }),
    "range" -> Val.Func(2, {case Seq((None, start), (None, end)) =>
        Val.Arr(
          (start.calc.asInstanceOf[Val.Num].value.toInt to
            end.calc.asInstanceOf[Val.Num].value.toInt)
            .map(i => Ref(Val.Num(i))))
    }),
    "mergePatch" -> Val.Func(2, {case Seq((None, base), (None, patch)) =>
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
        Materializer.reverse(rec(Materializer(base.calc), Materializer(patch.calc)))
    }),
    "sqrt" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.sqrt(n.calc.asInstanceOf[Val.Num].value))
    }),
    "makeArray" -> Val.Func(2, {case Seq((None, n), (None, func)) =>
        Val.Arr(
          (0 until n.calc.asInstanceOf[Val.Num].value.toInt).map(i =>
            Ref(func.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(Val.Num(i)))))
          )
        )
    }),
    "pow" -> Val.Func(2, {case Seq((None, n), (None, m)) =>
        Val.Num(math.pow(n.calc.asInstanceOf[Val.Num].value, m.calc.asInstanceOf[Val.Num].value))
    }),
    "floor" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.floor(n.calc.asInstanceOf[Val.Num].value))
    }),
    "ceil" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.ceil(n.calc.asInstanceOf[Val.Num].value))
    }),
    "abs" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.abs(n.calc.asInstanceOf[Val.Num].value))
    }),
    "sin" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.sin(n.calc.asInstanceOf[Val.Num].value))
    }),
    "cos" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.cos(n.calc.asInstanceOf[Val.Num].value))
    }),
    "tan" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.tan(n.calc.asInstanceOf[Val.Num].value))
    }),
    "asin" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.asin(n.calc.asInstanceOf[Val.Num].value))
    }),
    "acos" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.acos(n.calc.asInstanceOf[Val.Num].value))
    }),
    "atan" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.atan(n.calc.asInstanceOf[Val.Num].value))
    }),
    "log" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.log(n.calc.asInstanceOf[Val.Num].value))
    }),
    "exp" -> Val.Func(1, {case Seq((None, n)) =>
        Val.Num(math.exp(n.calc.asInstanceOf[Val.Num].value))
    }),
    "mantissa" -> Val.Func(1, {case Seq((None, n)) =>
        val value = n.calc.asInstanceOf[Val.Num].value
        val exponent = (Math.log(value) / Math.log(2)).toInt + 1
        val mantissa = value * Math.pow(2.0, -exponent)
        Val.Num(mantissa)
    }),
    "exponent" -> Val.Func(1, {case Seq((None, n)) =>
        val value = n.calc.asInstanceOf[Val.Num].value
        val exponent = (Math.log(value) / Math.log(2)).toInt + 1
        val mantissa = value * Math.pow(2.0, -exponent)
        Val.Num(exponent)
    }),
    "isString" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc.isInstanceOf[Val.Str]) Val.True else Val.False
    }),
    "isBoolean" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc == Val.True || n.calc == Val.False) Val.True else Val.False
    }),
    "isNumber" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc.isInstanceOf[Val.Num]) Val.True else Val.False
    }),
    "isObject" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc.isInstanceOf[Val.Obj]) Val.True else Val.False
    }),
    "isArray" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc.isInstanceOf[Val.Arr]) Val.True else Val.False
    }),
    "isFunction" -> Val.Func(1, {case Seq((None, n)) =>
        if (n.calc.isInstanceOf[Val.Func]) Val.True else Val.False
    }),
    "count" -> Val.Func(2, {case Seq((None, arr), (None, v)) =>
        val items = arr.calc.asInstanceOf[Val.Arr].value
        val res = items.count{i =>
          Materializer(i.calc) == Materializer(v.calc)
        }
        Val.Num(res)
    }),
    "filter" -> Val.Func(2, {case Seq((None, f), (None, arr)) =>
        Val.Arr(
          arr.calc.asInstanceOf[Val.Arr].value.filter{i =>
            f.calc.asInstanceOf[Val.Func].value(Seq(None -> i)) == Val.True
          }
        )
    }),
    "map" -> Val.Func(2, {case Seq((None, f), (None, arr)) =>
        Val.Arr(
          arr.calc.asInstanceOf[Val.Arr].value.map{i =>
            Ref(f.calc.asInstanceOf[Val.Func].value(Seq(None -> i)))
          }
        )
    }),
    "mapWithKey" -> Val.Func(2, {case Seq((None, f), (None, arr)) =>
        val allKeys = arr.calc.asInstanceOf[Val.Obj].getVisibleKeys()
        Val.Obj(
          allKeys.map{ k =>
            k._1 -> ((false, ":", (self: Val.Obj, sup: Option[Val.Obj]) => Ref(
              f.calc.asInstanceOf[Val.Func].value(
                Seq(None -> Ref(Val.Str(k._1)), None -> arr.calc.asInstanceOf[Val.Obj].value(k._1))
              )
            )))
          }.toMap,
          None
        )
    }),
    "mapWithIndex" -> Val.Func(2, {case Seq((None, f), (None, arr)) =>
        Val.Arr(
          arr.calc.asInstanceOf[Val.Arr].value.zipWithIndex.map{ case (i, i2) =>
            Ref(f.calc.asInstanceOf[Val.Func].value(Seq(None -> i, None -> Ref(Val.Num(i2)))))
          }
        )
    }),
    "filterMap" -> Val.Func(2, {case Seq((None, f1), (None, f2), (None, arr)) =>
        Val.Arr(
          arr.calc.asInstanceOf[Val.Arr].value.flatMap { i =>
            val x = i.calc
            if (f1.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(x))) != Val.True) None
            else Some(Ref(f2.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(x)))))
          }
        )
    }),
    "substr" -> Val.Func(2, {case Seq((None, s), (None, start), (None, end)) =>
        Val.Str(s.calc.asInstanceOf[Val.Str].value.substring(
          start.calc.asInstanceOf[Val.Num].value.toInt,
          end.calc.asInstanceOf[Val.Num].value.toInt + 1
        ))
    }),
    "startsWith" -> Val.Func(2, {case Seq((None, s), (None, snip)) =>
        if (s.calc.asInstanceOf[Val.Str].value.startsWith(snip.calc.asInstanceOf[Val.Str].value))
          Val.True
        else
          Val.False
    }),
    "endsWith" -> Val.Func(2, {case Seq((None, s), (None, snip)) =>
        if (s.calc.asInstanceOf[Val.Str].value.endsWith(snip.calc.asInstanceOf[Val.Str].value))
          Val.True
        else
          Val.False
    }),
    "char" -> Val.Func(2, {case Seq((None, c)) =>
        Val.Str(c.calc.asInstanceOf[Val.Num].value.toInt.toChar.toString)
    }),
    "strReplace" -> Val.Func(2, {case Seq((None, s), (None, a), (None, b)) =>
        Val.Str(
          s.calc.asInstanceOf[Val.Str].value.replace(
            a.calc.asInstanceOf[Val.Str].value,
            b.calc.asInstanceOf[Val.Str].value
          )
        )
    }),
    "join" -> Val.Func(2, {case Seq((None, sep), (None, xs)) =>
        sep.calc match{
          case Val.Str(s) =>
            Val.Str(xs.calc.asInstanceOf[Val.Arr].value.map(_.calc).filter(_ != Val.Null).map{case Val.Str(x) => x}.mkString(s))
          case Val.Arr(sep) =>
            val out = collection.mutable.Buffer.empty[Ref]
            for(x <- xs.calc.asInstanceOf[Val.Arr].value){
              x.calc match{
                case Val.Null => // do nothing
                case Val.Arr(v) =>
                  if (out.nonEmpty) out.appendAll(sep)
                  out.appendAll(v)
              }
            }
            Val.Arr(out)
        }
    }),
    "flattenArrays" -> Val.Func(1, {case Seq((None, xs)) =>
        val out = collection.mutable.Buffer.empty[Ref]
        for(x <- xs.calc.asInstanceOf[Val.Arr].value){
          x.calc match{
            case Val.Null => // do nothing
            case Val.Arr(v) => out.appendAll(v)
          }
        }
        Val.Arr(out)
    }),
    "manifestIni" -> Val.Func(1, {case Seq((None, v0)) =>
        val v = Materializer(v0.calc)
        def sect(x: ujson.Js.Obj) = {
          x.value.flatMap{
            case (k, ujson.Js.Str(v)) => Seq(k + " = " + v)
            case (k, ujson.Js.Arr(vs)) => vs.map{case ujson.Js.Str(v) => k + " = " + v}
          }
        }
        val lines = v.obj.get("main").fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Js.Obj])) ++
          v.obj.get("sections").fold(Iterable[String]())(x =>
            x.obj.flatMap{case (k, v) => Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Js.Obj])}
          )
        Val.Str(lines.flatMap(Seq(_, "\n")).mkString)
    }),
    "escapeStringJson" -> Val.Func(1, {case Seq((None, v0)) =>
        val v = v0.calc.asInstanceOf[Val.Str].value
        val out = new StringWriter()
        ujson.Renderer.escape(out, v, unicode = true)
        Val.Str(out.toString)
    }),
    "escapeStringBash" -> Val.Func(1, {case Seq((None, v0)) =>
        val v = v0.calc.asInstanceOf[Val.Str].value
        Val.Str("'" + v.replace("'", """'"'"'""") + "'")
    }),
    "escapeStringDollars" -> Val.Func(1, {case Seq((None, v0)) =>
        val v = v0.calc.asInstanceOf[Val.Str].value
        Val.Str(v.replace("$", "$$"))
    }),
    "manifestPython" -> Val.Func(1, {case Seq((None, v0)) =>
        Val.Str(Materializer(v0.calc).transform(new PythonRenderer()).toString)
    }),
    "manifestPythonVars" -> Val.Func(1, {case Seq((None, v0)) =>
        Val.Str(
          Materializer(v0.calc).obj
            .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
            .mkString
        )
    }),
    "manifestXmlJsonml" -> Val.Func(1, {case Seq((None, v0)) =>
        import scalatags.Text.all._


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

        Val.Str(rec(Materializer(v0.calc)).render)
    }),

    "base64" -> Val.Func(1, {case Seq((None, v0)) =>
        Val.Str(
          v0.calc match{
            case Val.Str(value) => Base64.getEncoder().encodeToString(value.getBytes)
            case Val.Arr(bytes) => Base64.getEncoder().encodeToString(bytes.map(_.calc.asInstanceOf[Val.Num].value.toByte).toArray)
          }
        )
    }),
    "base64Decode" -> Val.Func(1, {case Seq((None, v0)) =>
        Val.Str(
          v0.calc match{
            case Val.Str(value) => new String(Base64.getDecoder().decode(value))
          }
        )
    }),
    "base64DecodeBytes" -> Val.Func(1, {case Seq((None, v0)) =>
        v0.calc match{
          case Val.Str(value) =>
            Val.Arr(Base64.getDecoder().decode(value).map(i => Ref(Val.Num(i))))
        }
    }),
    "sort" -> Val.Func(1, {case Seq((None, v0)) =>
        val Val.Arr(vs0) = v0.calc
        val vs = vs0.map(_.calc)
        Val.Arr(

          if (vs.forall(_.isInstanceOf[Val.Str])){
            vs.map(_.asInstanceOf[Val.Str]).sortBy(_.value).map(Ref(_))
          }else if (vs.forall(_.isInstanceOf[Val.Num])){
            vs.map(_.asInstanceOf[Val.Num]).sortBy(_.value).map(Ref(_))
          }else {
            ???
          }
        )
    }),
    "uniq" -> Val.Func(1, {case Seq((None, v0)) =>
        val ujson.Js.Arr(vs) = Materializer(v0.calc)
        val out = collection.mutable.Buffer.empty[ujson.Js]
        for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

        Val.Arr(out.map(v => Ref(Materializer.reverse(v))))
    }),
    "set" -> Val.Func(1, {case Seq((None, v0)) =>
        val ujson.Js.Arr(vs0) = Materializer(v0.calc)
        val vs =
          if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
            vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
          }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
            vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
          }else ???

        val out = collection.mutable.Buffer.empty[ujson.Js]
        for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

        Val.Arr(out.map(v => Ref(Materializer.reverse(v))))
    }),
    "setUnion" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>

        val ujson.Js.Arr(vs1) = Materializer(v1.calc)
        val ujson.Js.Arr(vs2) = Materializer(v2.calc)
        val vs0 = vs1 ++ vs2
        val vs =
          if (vs0.forall(_.isInstanceOf[ujson.Js.Str])){
            vs0.map(_.asInstanceOf[ujson.Js.Str]).sortBy(_.value)
          }else if (vs0.forall(_.isInstanceOf[ujson.Js.Num])){
            vs0.map(_.asInstanceOf[ujson.Js.Num]).sortBy(_.value)
          }else ???

        val out = collection.mutable.Buffer.empty[ujson.Js]
        for(v <- vs) if (out.isEmpty || out.last != v) out.append(v)

        Val.Arr(out.map(v => Ref(Materializer.reverse(v))))
    }),
    "setInter" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        val seen = collection.mutable.LinkedHashSet.empty[ujson.Js.Value]
        val ujson.Js.Arr(vs1) = Materializer(v1.calc)
        val ujson.Js.Arr(vs2) = Materializer(v2.calc)


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

        Val.Arr(out.map(v => Ref(Materializer.reverse(v))))
    }),
    "setDiff" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        val seen = collection.mutable.LinkedHashSet.empty[ujson.Js.Value]
        val ujson.Js.Arr(vs1) = Materializer(v1.calc)
        val ujson.Js.Arr(vs2) = Materializer(v2.calc)


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

        Val.Arr(out.map(v => Ref(Materializer.reverse(v))))
    }),
    "setMember" -> Val.Func(2, {case Seq((None, v1), (None, v2)) =>
        val seen = collection.mutable.LinkedHashSet.empty[ujson.Js.Value]
        val vs1 = Materializer(v1.calc)
        val ujson.Js.Arr(vs2) = Materializer(v2.calc)
        if(vs2.contains(vs1)) Val.True else Val.False
    }),
  )
  val Std = Val.Obj(
    functions
      .map{case (k, v) => (k, (false, "::", (self: Val.Obj, sup: Option[Val.Obj]) => Ref(v)))}
      .toMap,
    None
  )
}

case class Scope(val dollar0: Option[Val.Obj],
                 val self0: Option[Val.Obj],
                 val super0: Option[Val.Obj],
                 val bindings0: Map[String, Ref],
                 val cwd: Path){
  def dollar = dollar0.get
  def self = self0.get
  val bindingCache = collection.mutable.Map.empty[String, Ref]
  def bindings(k: String) = bindingCache.getOrElseUpdate(k, bindings0(k))
  def ++(traversableOnce: TraversableOnce[(String, (Val.Obj, Option[Val.Obj]) => Ref)]) = {
    new Scope(
      dollar0,
      self0,
      super0,
      bindings0 ++ traversableOnce.map{case (k, v) => (k, v.apply(self0.getOrElse(null), super0))},
      cwd
    )
  }
}