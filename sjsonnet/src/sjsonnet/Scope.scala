package sjsonnet


object Scope{

  val Std = Value.Obj(
    Map(
      "assertEqual" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
          val x1 = Materializer(v1.calc)
          val x2 = Materializer(v2.calc)
          if (x1 == x2) Value.True
          else throw new Exception("assertEqual failed: " + x1 + " != " + x2)
        })
      )),
      "toString" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Str(Materializer.apply(v1.calc).transform(new Renderer()).toString)
        })
      )),
      "codepoint" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Num(v1.calc.asInstanceOf[Value.Str].value.charAt(0).toInt)
        })
      )),
      "length" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Num(
            v1.calc match{
              case Value.Str(s) => s.length
              case Value.Arr(s) => s.length
              case o: Value.Obj => o.getVisibleKeys().count(!_._2)
            }
          )
        })
      )),
      "objectHas" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
          if (v1.calc.asInstanceOf[Value.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Value.Str].value) == Some(false)){
            Value.True
          } else Value.False
        })
      )),
      "objectHasAll" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
          if (v1.calc.asInstanceOf[Value.Obj].getVisibleKeys().get(v2.calc.asInstanceOf[Value.Str].value).isDefined){
            Value.True
          } else Value.False
        })
      )),
      "objectFields" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Arr(
            v1.calc.asInstanceOf[Value.Obj]
              .getVisibleKeys()
              .collect{case (k, false) => Ref(Value.Str(k))}
              .toSeq
          )
        })
      )),
      "objectFieldsAll" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Arr(
            v1.calc.asInstanceOf[Value.Obj]
              .getVisibleKeys()
              .collect{case (k, _) => Ref(Value.Str(k))}
              .toSeq
          )
        })
      )),
      "type" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Str(
            v1.calc match{
              case Value.True | Value.False => "boolean"
              case Value.Null => "null"
              case _: Value.Obj => "object"
              case _: Value.Arr => "array"
              case _: Value.Func => "function"
              case _: Value.Num => "number"
              case _: Value.Str => "string"
            }
          )
        })
      )),
      "lines" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1)) =>
          Value.Str(
            Materializer.apply(v1.calc).asInstanceOf[ujson.Js.Arr]
              .value
              .map{case ujson.Js.Str(s) => s + "\n"}
              .mkString
          )
        })
      )),
      "format" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
          val formatStr = v1.calc.asInstanceOf[Value.Str].value

          val items = Materializer(v2.calc) match{
            case x: ujson.Js.Arr => x.value
            case x => Seq(x)
          }
          Value.Str(Format.format(formatStr, items))
        })
      )),
      "foldl" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, func), (None, cases), (None, start)) =>
          var current = start.calc
          for(item <- cases.calc.asInstanceOf[Value.Arr].value){
            current = func.calc.asInstanceOf[Value.Func].value(Seq((None, Ref(current)), (None, item)))
          }
          current
        })
      )),
      "range" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, start), (None, end)) =>
          Value.Arr(
            (start.calc.asInstanceOf[Value.Num].value.toInt to
             end.calc.asInstanceOf[Value.Num].value.toInt)
              .map(i => Ref(Value.Num(i))))
        })
      )),
      "mergePatch" -> ((
        false,
        "::",
        (self: Value.Obj, sup: Option[Value.Obj]) => Ref(Value.Func{case Seq((None, base), (None, patch)) =>
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
        })
      ))
    ),
    None
  )
}

class Scope(val dollar0: Option[Value.Obj],
            val self0: Option[Value.Obj],
            val super0: Option[Value.Obj],
            val bindings0: Map[String, Ref]){
  def dollar = dollar0.get
  def self = self0.get
  val bindingCache = collection.mutable.Map.empty[String, Ref]
  def bindings(k: String) = bindingCache.getOrElseUpdate(k, bindings0(k))
  def ++(traversableOnce: TraversableOnce[(String, (Value.Obj, Option[Value.Obj]) => Ref)]) = {
    new Scope(
      dollar0,
      self0,
      super0,
      bindings0 ++ traversableOnce.map{case (k, v) => (k, v.apply(self0.getOrElse(null), super0))}
    )
  }
}