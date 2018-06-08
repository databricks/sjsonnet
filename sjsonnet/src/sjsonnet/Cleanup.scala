package sjsonnet
import Expr._
object Cleanup {
  val precedenceTable = Seq(
    Seq("*", "/", "%"),
    Seq("+", "-"),
    Seq("<<", ">>"),
    Seq("<", ">", "<=", ">=", "in"),
    Seq("==", "!="),
    Seq("&"),
    Seq("^"),
    Seq("|"),
    Seq("&&"),
    Seq("||"),
  )
  val precedence = precedenceTable
    .reverse
    .zipWithIndex
    .flatMap{case (ops, idx) => ops.map(_ -> idx)}
    .toMap

  def visitExpr(expr: Expr): Expr = expr match{
    case Null => expr
    case True => expr
    case False => expr
    case Self => expr
    case Super => expr
    case $ => expr
    case Str(value) => expr
    case Num(value) => expr
    case Id(value) => expr
    case Arr(value) => Arr(value.map(visitExpr))
    case Obj(value) => Obj(visitObjBody(value))

    case UnaryOp(op, value) => UnaryOp(op, visitExpr(value))

    case BinaryOp(lhs, op, rhs) => ???

    case AssertExpr(Member.AssertStmt(value, msg), returned) =>
      AssertExpr(Member.AssertStmt(visitExpr(value), msg.map(visitExpr)), visitExpr(returned))

    case LocalExpr(bindings, returned) =>
      LocalExpr(bindings.map(b => visitBind(b.name, b.args, b.rhs)), visitExpr(returned))
    case Import(value) => expr
    case ImportStr(value) => expr
    case Error(value) => Error(visitExpr(value))
    case Apply(value, Args(args)) => Apply(visitExpr(value), Args(args.map{case (k, v) => (k, visitExpr(v))}))
    case Select(value, name) => Select(visitExpr(value), name)
    case Lookup(value, index) => Lookup(visitExpr(value), visitExpr(index))
    case Slice(value, start, end, stride) => Slice(visitExpr(value), start.map(visitExpr), end.map(visitExpr), stride.map(visitExpr))
    case Function(params, body) => Function(visitParams(params.args), visitExpr(body))
    case IfElse(cond, then, else0) => IfElse(visitExpr(cond), visitExpr(then), else0.map(visitExpr))
    case Comp(value, first, rest) => Comp(visitExpr(value), visitForSpec(first), rest.map(visitCompSpec))
    case ObjExtend(value, ext) => ObjExtend(visitExpr(value), ext)
  }

  def visitForSpec(c: ForSpec) = ForSpec(c.name, visitExpr(c.cond))
  def visitCompSpec(c: CompSpec) = c match{
    case f: ForSpec => visitForSpec(f)
    case c: IfSpec => IfSpec(visitExpr(c.cond))
  }
  def visitBind(name: String, args: Option[Params], rhs: Expr) = {
    Bind(name, args.map(p => visitParams(p.args)), visitExpr(rhs))
  }
  def visitParams(args: Seq[(String, Option[Expr])]) = Params(args.map{case (k, v) => (k, v.map(visitExpr))})
  def visitObjBody(b: ObjBody) = b match{
    case ObjBody.MemberList(value) =>
      ObjBody.MemberList(value.map{
        case Member.Field(fieldName, plus, args, sep, rhs) =>
          Member.Field(
            fieldName match{
              case FieldName.Dyn(e) => FieldName.Dyn(visitExpr(e))
              case FieldName.Fixed(s) => FieldName.Fixed(s)
            },
            plus,
            visitParams(args.args),
            sep,
            visitExpr(rhs)
          )
        case Member.BindStmt(b) => Member.BindStmt(visitBind(b.name, b.args, b.rhs))
        case Member.AssertStmt(value, msg) => Member.AssertStmt(visitExpr(value), msg.map(visitExpr))
      })
    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest)
  }
}
