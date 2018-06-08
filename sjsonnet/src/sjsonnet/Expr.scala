package sjsonnet

sealed trait Expr
object Expr{
  case object Null extends Expr
  case object True extends Expr
  case object False extends Expr
  case object Self extends Expr
  case object Super extends Expr
  case object $ extends Expr

  case class Str(value: String) extends Expr
  case class Num(value: Double) extends Expr
  case class Id(value: String) extends Expr
  case class Arr(value: Seq[Expr]) extends Expr
  case class Obj(value: ObjBody) extends Expr

  sealed trait FieldName

  object FieldName{
    case class Fixed(value: String) extends FieldName
    case class Dyn(expr: Expr) extends FieldName
  }
  sealed trait Member

  object Member{
    case class Field(fieldName: FieldName,
                     plus: Boolean,
                     args: Params,
                     sep: String,
                     rhs: Expr) extends Member
    case class BindStmt(value: Bind) extends Member
    case class AssertStmt(value: Expr, msg: Option[Expr]) extends Member
  }


  case class Parened(value: Expr) extends Expr
  case class Params(args: Seq[(String, Option[Expr])])
  case class Args(args: Seq[(Option[String], Expr)])

  case class UnaryOp(op: String, value: Expr) extends Expr
  case class BinaryOp(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class AssertExpr(asserted: Member.AssertStmt, returned: Expr) extends Expr
  case class LocalExpr(bindings: Seq[Bind], returned: Expr) extends Expr

  case class Bind(name: String, args: Option[Params], rhs: Expr)
  case class Import(value: String) extends Expr
  case class ImportStr(value: String) extends Expr
  case class Error(value: Expr) extends Expr
  case class Apply(value: Expr, args: Args) extends Expr
  case class Select(value: Expr, name: String) extends Expr
  case class Lookup(value: Expr, index: Expr) extends Expr
  case class Slice(value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr
  case class Function(params: Params, body: Expr) extends Expr
  case class IfElse(cond: Expr, then: Expr, `else`: Option[Expr]) extends Expr

  sealed trait CompSpec extends Expr
  case class IfSpec(cond: Expr) extends CompSpec
  case class ForSpec(name: String, cond: Expr) extends CompSpec

  case class Comp(value: Expr, first: ForSpec, rest: Seq[CompSpec]) extends Expr
  case class ObjExtend(base: Expr, ext: ObjBody) extends Expr

  sealed trait ObjBody
  object ObjBody{
    case class MemberList(value: Seq[Member]) extends ObjBody
    case class ObjComp(preLocals: Seq[Member.BindStmt],
                       key: Expr,
                       value: Expr,
                       postLocals: Seq[Member.BindStmt],
                       first: ForSpec,
                       rest: Seq[CompSpec]) extends ObjBody
  }

}