package sjsonnet

import java.io.StringWriter

import ammonite.ops.Path


object Scope{
  val Empty = new Scope(None, None, None, Map.empty, ammonite.ops.pwd)
  val Std = Val.Obj(
    Map(
      "assertEqual" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, v1), (None, v2)) =>
          val x1 = Materializer(v1.calc)
          val x2 = Materializer(v2.calc)
          if (x1 == x2) Val.True
          else throw new Exception("assertEqual failed: " + x1 + " != " + x2)
        }))
      )),
      "toString" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          v1.calc match{
            case Val.Str(s) => Val.Str(s)
            case v =>
              Val.Str(Materializer.apply(v).transform(new Renderer()).toString)
          }
        }))
      )),
      "codepoint" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          Val.Num(v1.calc.asInstanceOf[Val.Str].value.charAt(0).toInt)
        }))
      )),
      "length" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          Val.Num(
            v1.calc match{
              case Val.Str(s) => s.length
              case Val.Arr(s) => s.length
              case o: Val.Obj => o.getVisibleKeys().count(!_._2)
              case o: Val.Func => o.length
            }
          )
        }))
      )),
      "objectHas" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, v1), (None, v2)) =>
          if (v1.calc.asInstanceOf[Val.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Val.Str].value) == Some(false)){
            Val.True
          } else Val.False
        }))
      )),
      "objectHasAll" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, v1), (None, v2)) =>
          if (v1.calc.asInstanceOf[Val.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Val.Str].value).isDefined){
            Val.True
          } else Val.False
        }))
      )),
      "objectFields" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          Val.Arr(
            v1.calc.asInstanceOf[Val.Obj]
              .getVisibleKeys()
              .collect{case (k, false) => Ref(Val.Str(k))}
              .toSeq
          )
        }))
      )),
      "objectFieldsAll" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          Val.Arr(
            v1.calc.asInstanceOf[Val.Obj]
              .getVisibleKeys()
              .collect{case (k, _) => Ref(Val.Str(k))}
              .toSeq
          )
        }))
      )),
      "type" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
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
        }))
      )),
      "lines" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v1)) =>
          Val.Str(
            Materializer.apply(v1.calc).asInstanceOf[ujson.Js.Arr]
              .value
              .filter(_ != ujson.Js.Null)
              .map{case ujson.Js.Str(s) => s + "\n"}
              .mkString
          )
        }))
      )),
      "format" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, v1), (None, v2)) =>
          val formatStr = v1.calc.asInstanceOf[Val.Str].value

          val items = Materializer(v2.calc) match{
            case x: ujson.Js.Arr => x.value
            case x => Seq(x)
          }
          Val.Str(Format.format(formatStr, items))
        }))
      )),
      "foldl" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(3, {case Seq((None, func), (None, cases), (None, start)) =>
          var current = start.calc
          for(item <- cases.calc.asInstanceOf[Val.Arr].value){
            val c = current
            current = func.calc.asInstanceOf[Val.Func].value(Seq((None, Ref(c)), (None, item)))
          }
          current
        }))
      )),
      "foldr" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(3, {case Seq((None, func), (None, cases), (None, start)) =>
          var current = start.calc
          for(item <- cases.calc.asInstanceOf[Val.Arr].value.reverse){
            val c = current
            current = func.calc.asInstanceOf[Val.Func].value(Seq((None, item), (None, Ref(c))))
          }
          current
        }))
      )),
      "range" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, start), (None, end)) =>
          Val.Arr(
            (start.calc.asInstanceOf[Val.Num].value.toInt to
             end.calc.asInstanceOf[Val.Num].value.toInt)
              .map(i => Ref(Val.Num(i))))
        }))
      )),
      "mergePatch" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, base), (None, patch)) =>
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
        }))
      )),
      "sqrt" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.sqrt(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "makeArray" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, n), (None, func)) =>
          Val.Arr(
            (0 until n.calc.asInstanceOf[Val.Num].value.toInt).map(i =>
              Ref(func.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(Val.Num(i)))))
            )
          )
        }))
      )),
      "pow" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, n), (None, m)) =>
          Val.Num(math.pow(n.calc.asInstanceOf[Val.Num].value, m.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "floor" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.floor(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "ceil" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.ceil(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "abs" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.abs(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "sin" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.sin(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "cos" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.cos(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "tan" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.tan(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "asin" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.asin(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "acos" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.acos(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "atan" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.atan(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "log" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.log(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "exp" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          Val.Num(math.exp(n.calc.asInstanceOf[Val.Num].value))
        }))
      )),
      "mantissa" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          val value = n.calc.asInstanceOf[Val.Num].value
          val exponent = (Math.log(value) / Math.log(2)).toInt + 1
          val mantissa = value * Math.pow(2.0, -exponent)
          Val.Num(mantissa)
        }))
      )),
      "exponent" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          val value = n.calc.asInstanceOf[Val.Num].value
          val exponent = (Math.log(value) / Math.log(2)).toInt + 1
          val mantissa = value * Math.pow(2.0, -exponent)
          Val.Num(exponent)
        }))
      )),
      "isString" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc.isInstanceOf[Val.Str]) Val.True else Val.False
        }))
      )),
      "isBoolean" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc == Val.True || n.calc == Val.False) Val.True else Val.False
        }))
      )),
      "isNumber" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc.isInstanceOf[Val.Num]) Val.True else Val.False
        }))
      )),
      "isObject" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc.isInstanceOf[Val.Obj]) Val.True else Val.False
        }))
      )),
      "isArray" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc.isInstanceOf[Val.Arr]) Val.True else Val.False
        }))
      )),
      "isFunction" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, n)) =>
          if (n.calc.isInstanceOf[Val.Func]) Val.True else Val.False
        }))
      )),
      "count" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, arr), (None, v)) =>
          val items = arr.calc.asInstanceOf[Val.Arr].value
          val res = items.count{i =>
            Materializer(i.calc) == Materializer(v.calc)
          }
          Val.Num(res)
        }))
      )),
      "filter" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, f), (None, arr)) =>
          Val.Arr(
            arr.calc.asInstanceOf[Val.Arr].value.filter{i =>
              f.calc.asInstanceOf[Val.Func].value(Seq(None -> i)) == Val.True
            }
          )
        }))
      )),
      "map" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, f), (None, arr)) =>
          Val.Arr(
            arr.calc.asInstanceOf[Val.Arr].value.map{i =>
              Ref(f.calc.asInstanceOf[Val.Func].value(Seq(None -> i)))
            }
          )
        }))
      )),
      "mapWithKey" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, f), (None, arr)) =>
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
        }))
      )),
      "mapWithIndex" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, f), (None, arr)) =>
          Val.Arr(
            arr.calc.asInstanceOf[Val.Arr].value.zipWithIndex.map{ case (i, i2) =>
              Ref(f.calc.asInstanceOf[Val.Func].value(Seq(None -> i, None -> Ref(Val.Num(i2)))))
            }
          )
        }))
      )),
      "filterMap" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, f1), (None, f2), (None, arr)) =>
          Val.Arr(
            arr.calc.asInstanceOf[Val.Arr].value.flatMap { i =>
              val x = i.calc
              if (f1.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(x))) != Val.True) None
              else Some(Ref(f2.calc.asInstanceOf[Val.Func].value(Seq(None -> Ref(x)))))
            }
          )
        }))
      )),
      "substr" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, s), (None, start), (None, end)) =>
          Val.Str(s.calc.asInstanceOf[Val.Str].value.substring(
            start.calc.asInstanceOf[Val.Num].value.toInt,
            end.calc.asInstanceOf[Val.Num].value.toInt + 1
          ))
        }))
      )),
      "startsWith" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, s), (None, snip)) =>
          if (s.calc.asInstanceOf[Val.Str].value.startsWith(snip.calc.asInstanceOf[Val.Str].value))
            Val.True
          else
            Val.False
        }))
      )),
      "endsWith" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, s), (None, snip)) =>
          if (s.calc.asInstanceOf[Val.Str].value.endsWith(snip.calc.asInstanceOf[Val.Str].value))
            Val.True
          else
            Val.False
        }))
      )),
      "char" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, c)) =>
          Val.Str(c.calc.asInstanceOf[Val.Num].value.toInt.toChar.toString)
        }))
      )),
      "strReplace" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, s), (None, a), (None, b)) =>
          Val.Str(
            s.calc.asInstanceOf[Val.Str].value.replace(
              a.calc.asInstanceOf[Val.Str].value,
              b.calc.asInstanceOf[Val.Str].value
            )
          )
        }))
      )),
      "join" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(2, {case Seq((None, sep), (None, xs)) =>
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
        }))
      )),
      "flattenArrays" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, xs)) =>
          val out = collection.mutable.Buffer.empty[Ref]
          for(x <- xs.calc.asInstanceOf[Val.Arr].value){
            x.calc match{
              case Val.Null => // do nothing
              case Val.Arr(v) => out.appendAll(v)
            }
          }
          Val.Arr(out)
        }))
      )),
      "manifestIni" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
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
        }))
      )),
      "escapeStringJson" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
          val v = v0.calc.asInstanceOf[Val.Str].value
          val out = new StringWriter()
          ujson.Renderer.escape(out, v, unicode = true)
          Val.Str(out.toString)
        }))
      )),
      "escapeStringBash" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
          val v = v0.calc.asInstanceOf[Val.Str].value
          Val.Str("'" + v.replace("'", """'"'"'""") + "'")
        }))
      )),
      "escapeStringDollars" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
          val v = v0.calc.asInstanceOf[Val.Str].value
          Val.Str(v.replace("$", "$$"))
        }))
      )),
      "manifestPython" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
          Val.Str(Materializer(v0.calc).transform(new PythonRenderer()).toString)
        }))
      )),
      "manifestPythonVars" -> ((
        false,
        "::",
        (self: Val.Obj, sup: Option[Val.Obj]) => Ref(Val.Func(1, {case Seq((None, v0)) =>
          Val.Str(
            Materializer(v0.calc).obj
              .map{case (k, v) => k + " = " + v.transform(new PythonRenderer()).toString + "\n"}
              .mkString
          )
        }))
      )),
    ),
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