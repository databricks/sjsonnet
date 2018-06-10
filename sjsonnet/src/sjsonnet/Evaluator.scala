package sjsonnet
import Expr._
object Evaluator {
  class Scope(val dollar0: Option[Value.Obj],
              val self0: Option[Value.Obj],
              val bindings: Map[String, Ref]){
    def dollar = dollar0.get
    def self = self0.get
    def ++(traversableOnce: TraversableOnce[(String, Ref)]) = {
      new Scope(dollar0, self0, bindings ++ traversableOnce)
    }
  }

  def visitExpr(expr: Expr, scope: => Scope): Value = expr match{
    case Null => Value.Null
    case Parened(inner) => visitExpr(inner, scope)
    case True => Value.True
    case False => Value.False
    case Self => scope.dollar
    case Super => ???
    case $ => scope.dollar
    case Str(value) => Value.Str(value)
    case Num(value) => Value.Num(value)
    case Id(value) => scope.bindings(value).force(scope.dollar0)
    case Arr(value) => Value.Arr(value.map(v => Ref(visitExpr(v, scope))))
    case Obj(value) => visitObjBody(value, scope)

    case UnaryOp(op, value) => (op, visitExpr(value, scope)) match{
      case ("-", Value.Num(v)) => Value.Num(-v)
      case ("+", Value.Num(v)) => Value.Num(-v)
      case ("~", Value.Num(v)) => Value.Num(~v.toLong)
      case ("!", Value.True) => Value.False
      case ("!", Value.False) => Value.True
    }

    case BinaryOp(lhs, op, rhs) => (visitExpr(lhs, scope), op, visitExpr(rhs, scope)) match{
      case (Value.Num(l), "*", Value.Num(r)) => Value.Num(l * r)
      case (Value.Num(l), "/", Value.Num(r)) => Value.Num(l / r)
      case (Value.Num(l), "%", Value.Num(r)) => Value.Num(l % r)
      case (Value.Num(l), "+", Value.Num(r)) => Value.Num(l + r)
      case (Value.Str(l), "+", Value.Str(r)) => Value.Str(l + r)
      case (Value.Num(l), "-", Value.Num(r)) => Value.Num(l - r)
      case (Value.Num(l), "<<", Value.Num(r)) => Value.Num(l.toLong << r.toLong)
      case (Value.Num(l), ">>", Value.Num(r)) => Value.Num(l.toLong >> r.toLong)
      case (Value.Num(l), "<", Value.Num(r)) => if (l < r) Value.True else Value.False
      case (Value.Num(l), ">", Value.Num(r)) => if (l > r) Value.True else Value.False
      case (Value.Num(l), "<=", Value.Num(r)) => if (l <= r) Value.True else Value.False
      case (Value.Num(l), ">=", Value.Num(r)) => if (l >= r) Value.True else Value.False
      case (l, "==", r) => if (l == r) Value.True else Value.False
      case (Value.Str(l), "in", Value.Obj(r)) => if (r.contains(l)) Value.True else Value.False
      case (Value.Num(l), "&", Value.Num(r)) => Value.Num(l.toLong & r.toLong)
      case (Value.Num(l), "^", Value.Num(r)) => Value.Num(l.toLong ^ r.toLong)
      case (Value.Num(l), "|", Value.Num(r)) => Value.Num(l.toLong | r.toLong)
      case (Value.True, "&&", Value.True) => Value.True
      case (_, "&&", _) => Value.True
      case (Value.False, "||", Value.False) => Value.False
      case (_, "||", _) => Value.True
      case (Value.Obj(l), "+", Value.Obj(r)) => Value.Obj(l ++ r)
    }

    case AssertExpr(Member.AssertStmt(value, msg), returned) =>
      if (visitExpr(value, scope) != Value.True) {
        throw new Exception(msg.fold("")(visitExpr(_, scope).asInstanceOf[Value.Str].value))
      }
      visitExpr(returned, scope)

    case LocalExpr(bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings, newScope)
      visitExpr(returned, newScope)

//    case Import(value) => expr
//    case ImportStr(value) => expr
//    case Error(value) => Error(visitExpr(value))
    case Apply(value, Args(args)) =>
      visitExpr(value, scope).asInstanceOf[Value.Func].value(args.map{case (k, v) => (k, Ref(visitExpr(v, scope)))})

    case Select(value, name) => visitExpr(value, scope).asInstanceOf[Value.Obj].value(name).force(scope.dollar0)
    case Lookup(value, index) =>
      val res = (visitExpr(value, scope), visitExpr(index, scope)) match{
        case (v: Value.Arr, i: Value.Num) => v.value(i.value.toInt)
        case (v: Value.Obj, i: Value.Str) => v.value(i.value)
      }
      res.force(scope.dollar0)
//    case Slice(value, start, end, stride) => Slice(visitExpr(value), start.map(visitExpr), end.map(visitExpr), stride.map(visitExpr))
    case Function(params, body) => visitMethod(scope, body, params)
    case IfElse(cond, then, else0) =>
      visitExpr(cond, scope) match{
        case Value.True => visitExpr(then, scope)
        case Value.False => visitExpr(else0.get, scope)
      }
//    case Comp(value, first, rest) => Comp(visitExpr(value), visitForSpec(first), rest.map(visitCompSpec))
//    case ObjExtend(value, ext) => ObjExtend(visitExpr(value), ext)
  }


  def visitFieldName(fieldName: FieldName, scope: => Scope) = {
    fieldName match{
      case FieldName.Fixed(s) => s
      case FieldName.Dyn(k) => visitExpr(k, scope).asInstanceOf[Value.Str].value
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, argSpec: Params) = {
    Value.Func { args =>
      val newScope =
        scope ++
        argSpec.args.collect{case (k, Some(default)) => (k, Ref(visitExpr(default, scope)))} ++
        args.zipWithIndex.map{
          case ((Some(name), v), _) => (name, v)
          case ((None, v), i) => (argSpec.args(i)._1, v)
        }
      visitExpr(rhs, newScope)
    }
  }
  def visitBindings(bindings: Seq[Bind], scope: => Scope) = {
    bindings.collect{
      case Bind(fieldName, None, rhs) =>
        (fieldName, Ref(visitExpr(rhs, scope)))
      case Bind(fieldName, Some(argSpec), rhs) =>
        (fieldName, Ref(visitMethod(scope, rhs, argSpec)))
    }
  }
  def visitObjBody(b: ObjBody, scope: => Scope): Value.Obj = b match{
    case ObjBody.MemberList(value) =>

      lazy val newScope: Scope = new Scope(scope.dollar0.orElse(Some(newSelf)), Some(newSelf), scope.bindings ++ newBindings)

      lazy val newBindings = visitBindings(value.collect{case Member.BindStmt(b) => b}, newScope)

      lazy val newSelf = Value.Obj(value.flatMap {
        case Member.Field(fieldName, plus, None, sep, rhs) =>
          Some(visitFieldName(fieldName, scope) -> Ref(visitExpr(rhs, newScope)))
        case Member.Field(fieldName, false, Some(argSpec), sep, rhs) =>
          Some(visitFieldName(fieldName, scope) -> Ref(visitMethod(newScope, rhs, argSpec)))

        case _: Member.BindStmt => None
        case Member.AssertStmt(value, msg) => None
      }.toMap)

      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      lazy val newScope: Scope = new Scope(scope.dollar0.orElse(Some(newSelf)), Some(newSelf), scope.bindings ++ newBindings)

      lazy val newBindings = visitBindings(preLocals.collect{ case Member.BindStmt(b) => b}, newScope)

      lazy val newSelf = Value.Obj(
        visitComp(first :: rest.toList, Seq(newScope))
          .map(s => visitExpr(key, s).asInstanceOf[Value.Str].value -> Ref(visitExpr(value, s)))
          .toMap
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
        } yield s ++ Seq(name -> e)
      )
    case IfSpec(expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr, _) == Value.True))
    case Nil => scopes
  }
}
