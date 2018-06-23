package sjsonnet
import Expr._
object Evaluator {

  object Scope{

    val Std = Value.Obj(
      Map(
        "assertEqual" -> ((
          false,
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
            val x1 = Materializer(v1.calc)
            val x2 = Materializer(v2.calc)
            if (x1 == x2) Value.True
            else throw new Exception("assertEqual failed: " + x1 + " != " + x2)
          })
        )),
        "toString" -> ((
          false,
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1)) =>
            Value.Str(Materializer.apply(v1.calc).transform(new Renderer()).toString)
          })
        )),
        "codepoint" -> ((
          false,
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1)) =>
            Value.Num(v1.calc.asInstanceOf[Value.Str].value.charAt(0).toInt)
          })
        )),
        "length" -> ((
          false,
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1)) =>
            Value.Num(v1.calc.asInstanceOf[Value.Str].value.length)
          })
        )),
        "lines" -> ((
          false,
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1)) =>
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
          (self: Value.Obj) => Ref(Value.Func{case Seq((None, v1), (None, v2)) =>
            val formatStr = v1.calc.asInstanceOf[Value.Str].value

            val items = Materializer(v2.calc) match{
              case x: ujson.Js.Arr => x.value
              case x => Seq(x)
            }
            Value.Str(Format.format(formatStr, items))
          })
        ))
      ),
      None
    )
  }

  class Scope(val dollar0: Option[Value.Obj],
              val self0: Option[Value.Obj],
              val bindings0: Map[String, Ref]){
    def dollar = dollar0.get
    def self = self0.get
    val bindingCache = collection.mutable.Map.empty[String, Ref]
    def bindings(k: String) = bindingCache.getOrElseUpdate(k, bindings0(k))
    def ++(traversableOnce: TraversableOnce[(String, Value.Obj => Ref)]) = {
      new Scope(dollar0, self0, bindings0 ++ traversableOnce.map{case (k, v) => (k, v.apply(self0.getOrElse(null)))})
    }
  }

  def visitExpr(expr: Expr, scope: => Scope): Value = expr match{
    case Null => Value.Null
    case Parened(inner) => visitExpr(inner, scope)
    case True => Value.True
    case False => Value.False
    case Self => scope.self
    case Super => scope.self.`super`.get
    case $ => scope.dollar
    case Str(value) => Value.Str(value)
    case Num(value) => Value.Num(value)
    case Id(value) => scope.bindings(value).force(scope.dollar0)

    case Arr(value) => Value.Arr(value.map(v => Ref(visitExpr(v, scope))))
    case Obj(value) => visitObjBody(value, scope)

    case UnaryOp(op, value) => (op, visitExpr(value, scope)) match{
      case ("-", Value.Num(v)) => Value.Num(-v)
      case ("+", Value.Num(v)) => Value.Num(v)
      case ("~", Value.Num(v)) => Value.Num(~v.toLong)
      case ("!", Value.True) => Value.False
      case ("!", Value.False) => Value.True
    }

    case BinaryOp(lhs, op, rhs) => {
      op match{
        case "&&" | "||" =>
          (visitExpr(lhs, scope), op) match {
            case (Value.False, "&&") => Value.False
            case (Value.True, "||") => Value.True
            case _ => visitExpr(rhs, scope)

          }
        case _ =>
          (visitExpr(lhs, scope), op, visitExpr(rhs, scope)) match{
            case (Value.Num(l), "*", Value.Num(r)) => Value.Num(l * r)
            case (Value.Num(l), "/", Value.Num(r)) => Value.Num(l / r)
            case (Value.Num(l), "%", Value.Num(r)) => Value.Num(l % r)
            case (Value.Num(l), "+", Value.Num(r)) => Value.Num(l + r)
            case (Value.Str(l), "%", r) => Value.Str(Format.format(l, Materializer.apply(r)))
            case (Value.Str(l), "+", Value.Str(r)) => Value.Str(l + r)
            case (Value.Str(l), "<", Value.Str(r)) => if (l < r) Value.True else Value.False
            case (Value.Str(l), ">", Value.Str(r)) => if (l > r) Value.True else Value.False
            case (Value.Str(l), "<=", Value.Str(r)) => if (l <= r) Value.True else Value.False
            case (Value.Str(l), ">=", Value.Str(r)) => if (l >= r) Value.True else Value.False
            case (Value.Str(l), "+", r) => Value.Str(l + Materializer.apply(r).transform(new Renderer()).toString)
            case (l, "+", Value.Str(r)) => Value.Str(Materializer.apply(l).transform(new Renderer()).toString + r)
            case (Value.Num(l), "-", Value.Num(r)) => Value.Num(l - r)
            case (Value.Num(l), "<<", Value.Num(r)) => Value.Num(l.toLong << r.toLong)
            case (Value.Num(l), ">>", Value.Num(r)) => Value.Num(l.toLong >> r.toLong)
            case (Value.Num(l), "<", Value.Num(r)) => if (l < r) Value.True else Value.False
            case (Value.Num(l), ">", Value.Num(r)) => if (l > r) Value.True else Value.False
            case (Value.Num(l), "<=", Value.Num(r)) => if (l <= r) Value.True else Value.False
            case (Value.Num(l), ">=", Value.Num(r)) => if (l >= r) Value.True else Value.False
            case (l, "==", r) => if (Materializer(l) == Materializer(r)) Value.True else Value.False
            case (l, "!=", r) => if (Materializer(l) != Materializer(r)) Value.True else Value.False
            case (Value.Str(l), "in", Value.Obj(r, _)) => if (r.contains(l)) Value.True else Value.False
            case (Value.Num(l), "&", Value.Num(r)) => Value.Num(l.toLong & r.toLong)
            case (Value.Num(l), "^", Value.Num(r)) => Value.Num(l.toLong ^ r.toLong)
            case (Value.Num(l), "|", Value.Num(r)) => Value.Num(l.toLong | r.toLong)
            case (l: Value.Obj, "+", r: Value.Obj) => mergeObjects(l, r)
            case (Value.Arr(l), "+", Value.Arr(r)) => Value.Arr(l ++ r)
          }
        }
    }
    case AssertExpr(Member.AssertStmt(value, msg), returned) =>
      if (visitExpr(value, scope) != Value.True) {
        throw new Exception(msg.fold("")(visitExpr(_, scope).asInstanceOf[Value.Str].value))
      }
      visitExpr(returned, scope)

    case LocalExpr(bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings, self => newScope)
      visitExpr(returned, newScope)

//    case Import(value) => expr
//    case ImportStr(value) => expr
    case Error(value) => throw new Exception(visitExpr(value, scope).asInstanceOf[Value.Str].value)
    case Apply(value, Args(args)) =>
      visitExpr(value, scope).asInstanceOf[Value.Func].value(args.map{case (k, v) => (k, Ref(visitExpr(v, scope)))})

    case Select(value, name) =>
      val self = visitExpr(value, scope).asInstanceOf[Value.Obj]

      self.value(name).force(scope.dollar0)

    case Lookup(value, index) =>
      val res = (visitExpr(value, scope), visitExpr(index, scope)) match{
        case (v: Value.Arr, i: Value.Num) => v.value(i.value.toInt)
        case (v: Value.Str, i: Value.Num) => Ref(Value.Str(new String(Array(v.value(i.value.toInt)))))
        case (v: Value.Obj, i: Value.Str) => v.value(i.value)
      }
      res.force(scope.dollar0)
//    case Slice(value, start, end, stride) => Slice(visitExpr(value), start.map(visitExpr), end.map(visitExpr), stride.map(visitExpr))
    case Function(params, body) => visitMethod(scope, body, params)
    case IfElse(cond, then, else0) =>
      visitExpr(cond, scope) match{
        case Value.True => visitExpr(then, scope)
        case Value.False => else0.fold[Value](Value.Null)(visitExpr(_, scope))
      }
    case Comp(value, first, rest) =>
      Value.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Ref(visitExpr(value, s))))
    case ObjExtend(value, ext) => {
      val original = visitExpr(value, scope).asInstanceOf[Value.Obj]
      val extension = visitObjBody(ext, scope)
      mergeObjects(original, extension)
    }
  }
  def mergeObjects(lhs: Value.Obj, rhs: Value.Obj) = {
    def rec(current: Value.Obj): Value.Obj = {
      current.`super` match{
        case None => Value.Obj(current.value0, Some(lhs))
        case Some(x) => Value.Obj(current.value0, Some(rec(x)))
      }
    }
    rec(rhs)
  }


  def visitFieldName(fieldName: FieldName, scope: => Scope) = {
    fieldName match{
      case FieldName.Fixed(s) => s
      case FieldName.Dyn(k) => visitExpr(k, scope).asInstanceOf[Value.Str].value
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, argSpec: Params) = {
    Value.Func { args =>
      lazy val newScope: Scope =
        scope ++
        argSpec.args.collect{case (k, Some(default)) => (k, (self: Value.Obj) => Ref(visitExpr(default, newScope)))} ++
        args.zipWithIndex.map{
          case ((Some(name), v), _) => (name, (self: Value.Obj) => v)
          case ((None, v), i) => (argSpec.args(i)._1, (self: Value.Obj) => v)
        }
      visitExpr(rhs, newScope)
    }
  }
  def visitBindings(bindings: Seq[Bind], scope: Value.Obj => Scope) = {

    bindings.collect{
      case Bind(fieldName, None, rhs) =>
        (fieldName, (self: Value.Obj) => Ref(visitExpr(rhs, scope(self))))
      case Bind(fieldName, Some(argSpec), rhs) =>
        (fieldName, (self: Value.Obj) => Ref(visitMethod(scope(self), rhs, argSpec)))
    }
  }
  def visitObjBody(b: ObjBody, scope: => Scope): Value.Obj = b match{
    case ObjBody.MemberList(value) =>
      def makeNewScope(self: => Value.Obj): Scope = new Scope(
        scope.dollar0.orElse(Some(self)),
        Some(self),
        scope.bindings0 ++ newBindings.map{case (k, v) => (k, v.apply(self))}
      )


      lazy val newBindings = visitBindings(value.collect{case Member.BindStmt(b) => b}, self => makeNewScope(self))

      lazy val newSelf = Value.Obj(
        value.flatMap {
          case Member.Field(fieldName, plus, None, sep, rhs) =>
            Some(visitFieldName(fieldName, scope) -> (plus, (self: Value.Obj) => {
              Ref(visitExpr(rhs, makeNewScope(self)))
            }))
          case Member.Field(fieldName, false, Some(argSpec), sep, rhs) =>
            Some(visitFieldName(fieldName, scope) -> (false, (self: Value.Obj) =>
              Ref(visitMethod(makeNewScope(self), rhs, argSpec))
            ))

          case _: Member.BindStmt => None
          case Member.AssertStmt(value, msg) => None
        }.toMap,
        None
      )
      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      lazy val compScope: Scope = new Scope(
        scope.dollar0,
        scope.self0,
        scope.bindings0 ++ newBindings.map{case (k, v) => (k, v.apply(scope.self0.getOrElse(null)))}
      )

      lazy val newScope: Scope = new Scope(
        scope.dollar0.orElse(Some(newSelf)),
        Some(newSelf),
        scope.bindings0 ++ newBindings.map{case (k, v) => (k, v.apply(scope.self0.getOrElse(null)))}
      )

      lazy val newBindings = visitBindings(
        preLocals.collect{ case Member.BindStmt(b) => b},
        self => newScope
      )

      lazy val newSelf = Value.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .map(s =>
            visitExpr(key, s).asInstanceOf[Value.Str].value -> (false, (self: Value.Obj) => Ref(visitExpr(value, s)))
          )
          .toMap,
        None
      )

      newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Seq[Scope]): Seq[Scope] = f match{
    case ForSpec(name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr, s).asInstanceOf[Value.Arr].value
        } yield s ++ Seq(name -> ((self: Value.Obj) => e))
      )
    case IfSpec(expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr, _) == Value.True))
    case Nil => scopes
  }
}

