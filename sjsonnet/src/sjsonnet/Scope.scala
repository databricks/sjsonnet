package sjsonnet

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
          Val.Str(Materializer.apply(v1.calc).transform(new Renderer()).toString)
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
            current = func.calc.asInstanceOf[Val.Func].value(Seq((None, Ref(current)), (None, item)))
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
      ))
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