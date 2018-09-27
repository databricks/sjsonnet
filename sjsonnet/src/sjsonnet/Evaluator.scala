package sjsonnet
import Expr._
import ammonite.ops.{Path, RelPath}
object Evaluator extends Evaluator(Scope.Empty)
class Evaluator(originalScope: Scope) {
  val imports = collection.mutable.Map.empty[Path, Val]
  val importStrs = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr, scope: => Scope): Val = expr match{
    case Null(offset) => Val.Null
    case Parened(offset, inner) => visitExpr(inner, scope)
    case True(offset) => Val.True
    case False(offset) => Val.False
    case Self(offset) => scope.self

    case Select(_, Super(offset), name) =>
      scope.super0.get.value(name, self = scope.self).calc

    case Lookup(_, Super(offset), index) =>
      val key = visitExpr(index, scope).asInstanceOf[Val.Str]
      scope.super0.get.value(key.value).calc

    case BinaryOp(_, lhs, "in", Super(offset)) =>
      val key = visitExpr(lhs, scope).asInstanceOf[Val.Str]
      if (scope.super0.get.value0.contains(key.value)) Val.True else Val.False

    case $(offset) => scope.dollar
    case Str(offset, value) => Val.Str(value)
    case Num(offset, value) => Val.Num(value)
    case Id(offset, value) => scope.bindings(value).force(scope.dollar0)

    case Arr(offset, value) => Val.Arr(value.map(v => Ref(visitExpr(v, scope))))
    case Obj(offset, value) => visitObjBody(value, scope)

    case UnaryOp(offset, op, value) => (op, visitExpr(value, scope)) match{
      case ("-", Val.Num(v)) => Val.Num(-v)
      case ("+", Val.Num(v)) => Val.Num(v)
      case ("~", Val.Num(v)) => Val.Num(~v.toLong)
      case ("!", Val.True) => Val.False
      case ("!", Val.False) => Val.True
    }

    case BinaryOp(offset, lhs, op, rhs) => {
      op match{
        case "&&" | "||" =>
          (visitExpr(lhs, scope), op) match {
            case (Val.False, "&&") => Val.False
            case (Val.True, "||") => Val.True
            case _ => visitExpr(rhs, scope)

          }
        case _ =>
          (visitExpr(lhs, scope), op, visitExpr(rhs, scope)) match{
            case (Val.Num(l), "*", Val.Num(r)) => Val.Num(l * r)
            case (Val.Num(l), "/", Val.Num(r)) => Val.Num(l / r)
            case (Val.Num(l), "%", Val.Num(r)) => Val.Num(l % r)
            case (Val.Num(l), "+", Val.Num(r)) => Val.Num(l + r)
            case (Val.Str(l), "%", r) => Val.Str(Format.format(l, Materializer.apply(r)))
            case (Val.Str(l), "+", Val.Str(r)) => Val.Str(l + r)
            case (Val.Str(l), "<", Val.Str(r)) => if (l < r) Val.True else Val.False
            case (Val.Str(l), ">", Val.Str(r)) => if (l > r) Val.True else Val.False
            case (Val.Str(l), "<=", Val.Str(r)) => if (l <= r) Val.True else Val.False
            case (Val.Str(l), ">=", Val.Str(r)) => if (l >= r) Val.True else Val.False
            case (Val.Str(l), "+", r) => Val.Str(l + Materializer.apply(r).transform(new Renderer()).toString)
            case (l, "+", Val.Str(r)) => Val.Str(Materializer.apply(l).transform(new Renderer()).toString + r)
            case (Val.Num(l), "-", Val.Num(r)) => Val.Num(l - r)
            case (Val.Num(l), "<<", Val.Num(r)) => Val.Num(l.toLong << r.toLong)
            case (Val.Num(l), ">>", Val.Num(r)) => Val.Num(l.toLong >> r.toLong)
            case (Val.Num(l), "<", Val.Num(r)) => if (l < r) Val.True else Val.False
            case (Val.Num(l), ">", Val.Num(r)) => if (l > r) Val.True else Val.False
            case (Val.Num(l), "<=", Val.Num(r)) => if (l <= r) Val.True else Val.False
            case (Val.Num(l), ">=", Val.Num(r)) => if (l >= r) Val.True else Val.False
            case (l, "==", r) => if (Materializer(l) == Materializer(r)) Val.True else Val.False
            case (l, "!=", r) => if (Materializer(l) != Materializer(r)) Val.True else Val.False
            case (Val.Str(l), "in", Val.Obj(r, _)) => if (r.contains(l)) Val.True else Val.False
            case (Val.Num(l), "&", Val.Num(r)) => Val.Num(l.toLong & r.toLong)
            case (Val.Num(l), "^", Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
            case (Val.Num(l), "|", Val.Num(r)) => Val.Num(l.toLong | r.toLong)
            case (l: Val.Obj, "+", r: Val.Obj) => mergeObjects(l, r)
            case (Val.Arr(l), "+", Val.Arr(r)) => Val.Arr(l ++ r)
          }
        }
    }
    case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
      if (visitExpr(value, scope) != Val.True) {
        throw new Exception(msg.fold("")(visitExpr(_, scope).asInstanceOf[Val.Str].value))
      }
      visitExpr(returned, scope)

    case LocalExpr(offset, bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings, (self, sup) => newScope)
      visitExpr(returned, newScope)

    case Import(offset, value) =>

      val p = scope.cwd / RelPath(value)
      imports.getOrElseUpdate(
        p,
        visitExpr(
          Parser.expr.parse(ammonite.ops.read(p)).get.value,
          originalScope.copy(cwd = p / ammonite.ops.up)
        )
      )


    case ImportStr(offset, value) =>
      val p = scope.cwd / RelPath(value)
      Val.Str(
        importStrs.getOrElseUpdate(
          p,
          ammonite.ops.read(p)
        )
      )
    case Error(offset, value) => throw new Exception(visitExpr(value, scope).asInstanceOf[Val.Str].value)
    case Apply(offset, value, Args(args)) =>
      visitExpr(value, scope).asInstanceOf[Val.Func].value(args.map{case (k, v) => (k, Ref(visitExpr(v, scope)))})

    case Select(offset, value, name) =>
      val self = visitExpr(value, scope).asInstanceOf[Val.Obj]
      self.value(name).force(scope.dollar0)

    case Lookup(offset, value, index) =>
      val res = (visitExpr(value, scope), visitExpr(index, scope)) match{
        case (v: Val.Arr, i: Val.Num) => v.value(i.value.toInt)
        case (v: Val.Str, i: Val.Num) => Ref(Val.Str(new String(Array(v.value(i.value.toInt)))))
        case (v: Val.Obj, i: Val.Str) => v.value(i.value)
      }
      res.force(scope.dollar0)
    case Slice(offset, value, start, end, stride) =>
      visitExpr(value, scope) match{
        case Val.Arr(a) =>

          val range = start.fold(0)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) until end.fold(a.length)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) by stride.fold(1)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt)
          Val.Arr(range.dropWhile(_ < 0).takeWhile(_ < a.length).map(a))
        case Val.Str(s) =>
          val range = start.fold(0)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) until end.fold(s.length)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) by stride.fold(1)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt)
          Val.Str(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).mkString)
      }
    case Function(offset, params, body) => visitMethod(scope, body, params)
    case IfElse(offset, cond, then, else0) =>
      visitExpr(cond, scope) match{
        case Val.True => visitExpr(then, scope)
        case Val.False => else0.fold[Val](Val.Null)(visitExpr(_, scope))
      }
    case Comp(offset, value, first, rest) =>
      Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Ref(visitExpr(value, s))))
    case ObjExtend(offset, value, ext) => {
      val original = visitExpr(value, scope).asInstanceOf[Val.Obj]
      val extension = visitObjBody(ext, scope)
      mergeObjects(original, extension)
    }
  }
  def mergeObjects(lhs: Val.Obj, rhs: Val.Obj) = {
    def rec(current: Val.Obj): Val.Obj = {
      current.`super` match{
        case None => Val.Obj(current.value0, Some(lhs))
        case Some(x) => Val.Obj(current.value0, Some(rec(x)))
      }
    }
    rec(rhs)
  }


  def visitFieldName(fieldName: FieldName, scope: => Scope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k, scope) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
      }
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, argSpec: Params) = {
    Val.Func (
      argSpec.args.length,
      { args =>
        lazy val newScope: Scope =
          scope ++
          argSpec.args.collect{
            case (k, Some(default)) => (k, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitExpr(default, newScope)))
          } ++
          args.zipWithIndex.map{
            case ((Some(name), v), _) => (name, (self: Val.Obj, sup: Option[Val.Obj]) => v)
            case ((None, v), i) => (argSpec.args(i)._1, (self: Val.Obj, sup: Option[Val.Obj]) => v)
          }
        visitExpr(rhs, newScope)
      }
    )
  }
  def visitBindings(bindings: Seq[Bind], scope: (Val.Obj, Option[Val.Obj]) => Scope) = {

    bindings.collect{
      case Bind(fieldName, None, rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitExpr(rhs, scope(self, sup))))
      case Bind(fieldName, Some(argSpec), rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitMethod(scope(self, sup), rhs, argSpec)))
    }
  }
  def visitObjBody(b: ObjBody, scope: => Scope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      def makeNewScope(self: => Val.Obj, sup: => Option[Val.Obj]): Scope = new Scope(
        scope.dollar0.orElse(Some(self)),
        Some(self),
        sup,
        scope.bindings0 ++ newBindings.map{case (k, v) => (k, v.apply(self, sup))},
        scope.cwd
      )


      lazy val newBindings = visitBindings(
        value.collect{case Member.BindStmt(b) => b},
        (self, sup) => makeNewScope(self, sup)
      )

      lazy val newSelf = Val.Obj(
        value.flatMap {
          case Member.Field(fieldName, plus, None, sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> (plus, sep, (self: Val.Obj, sup: Option[Val.Obj]) => {
              Ref(visitExpr(rhs, makeNewScope(self, sup)))
            }))
          case Member.Field(fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> (false, sep, (self: Val.Obj, sup: Option[Val.Obj]) =>
              Ref(visitMethod(makeNewScope(self, sup), rhs, argSpec))
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
        None,
        scope.bindings0,
        scope.cwd
      )

      lazy val newScope: Scope = new Scope(
        scope.dollar0.orElse(Some(newSelf)),
        Some(newSelf),
        None,
        scope.bindings0 ++ newBindings.map{case (k, v) => (k, v.apply(scope.self0.getOrElse(null), None))},
        scope.cwd
      )

      lazy val newBindings = visitBindings(
        (preLocals ++ postLocals).collect{ case Member.BindStmt(b) => b},
        (self, sup) => newScope
      )

      lazy val newSelf = Val.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .flatMap(s =>
            visitExpr(key, s) match{
              case Val.Str(k) =>
                Some(k -> ((false, ":", (self: Val.Obj, sup: Option[Val.Obj]) =>
                  Ref(visitExpr(
                    value,
                    s.copy(self0 = Some(self), dollar0 = Some(s.dollar0.getOrElse(self))) ++
                    newBindings
                  ))
                )))
              case Val.Null => None
            }

          )
          .toMap,
        None
      )

      newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Seq[Scope]): Seq[Scope] = f match{
    case ForSpec(offset, name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr, s).asInstanceOf[Val.Arr].value
        } yield s ++ Seq(name -> ((self: Val.Obj, sup: Option[Val.Obj]) => e))
      )
    case IfSpec(offset, expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr, _) == Val.True))
    case Nil => scopes
  }
}

