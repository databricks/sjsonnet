package sjsonnet

import fastparse.WhitespaceApi
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
  case class Id(value: String) extends Expr with FieldName

  case class Arr(value: Seq[Expr]) extends Expr
  case class Obj(value: ObjBody) extends Expr
  sealed trait FieldName
  case class DynFieldName(expr: Expr) extends FieldName
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


  case class Params(args: Seq[(String, Option[Expr])]) extends Expr

  case class Args(args: Seq[(Option[String], Expr)]) extends Expr
  case class UnaryOp(op: String, value: Expr) extends Expr


  case class BinaryOp(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class AssertExpr(asserted: Member.AssertStmt, returned: Expr) extends Expr
  case class LocalExpr(bindings: Seq[Bind], returned: Expr) extends Expr

  case class Bind(name: String, args: Option[Params], rhs: Expr) extends Expr
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
object Parser{
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhileIn(" \t\n", 0))
  }
  import fastparse.noApi._
  import White._

  val id = P(
    CharIn("_" ++ ('a' to 'z') ++ ('A' to 'Z')) ~~
    CharsWhileIn("_" ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), min = 0)
  ).!

  val number: P[Expr.Num] = P(
    CharsWhileIn('0' until '9') ~~
    ("." ~ CharsWhileIn('0' until '9')).? ~~
    (("e" | "E") ~ ("+" | "-").? ~~ CharsWhileIn('0' until '9')).?
  ).!.map(s => Expr.Num(s.toDouble))

  val string: P[String] = P(
    "\"" ~~ (CharsWhile(x => x != '"' && x != '\\').! | "\\" ~~ AnyChar.!).repX ~~ "\"" |
    "'" ~~ (CharsWhile(x => x != '\'' && x != '\\').! | "\\" ~~ AnyChar.!).repX ~~ "'" |
    "@\"" ~~ (CharsWhile(_ != '"').! | "\"\"".!.map(_ => "\"")).repX ~~ "\"" |
    "@'" ~~ (CharsWhile(_ != '\'').! | "''".!.map(_ => "'")).repX ~~ "'" |
    "|||" ~~ CharsWhileIn(" \t", 0) ~~ "\n" ~~ CharsWhileIn(" ", min=1).!.flatMap(s =>
      (s ~~ CharsWhile(_ != '\n').!).repX(sep = "\n").map(x => Seq(x.mkString("\n")))
    )
  ).map(_.mkString)

  val `null` = P("null").map(_ => Expr.Null)
  val `true` = P("true").map(_ => Expr.True)
  val `false` = P("false").map(_ => Expr.False)
  val `self` = P("self").map(_ => Expr.Self)
  val $ = P("$").map(_ => Expr.$)
  val `super` = P("super").map(_ => Expr.Super)

  val obj: P[Expr] = P( "{" ~ objinside.map(Expr.Obj) ~ "}" )
  val arr: P[Expr] = P( "[" ~ expr.rep(sep = ",").map(Expr.Arr) ~ ",".? ~ "]" )
  val error: P[Expr] = P( "error" ~ expr.map(Expr.Error) )
  val importstr: P[Expr] = P( "importstr" ~ string.map(Expr.ImportStr) )
  val `import`: P[Expr] = P( "import" ~ string.map(Expr.Import) )
  val assertExpr: P[Expr] = P( assertStmt ~ ";" ~ expr ).map(Expr.AssertExpr.tupled)
  val function: P[Expr] = P( "function" ~ "(" ~ params ~ ")" ~ expr ).map(Expr.Function.tupled)
  val ifElse: P[Expr] = P( "if" ~ expr ~ "then" ~ expr ~ ("else" ~ expr).? ).map(Expr.IfElse.tupled)
  val localExpr: P[Expr] = P( "local" ~ bind.rep(min=1, sep=",") ~ ";" ~ expr ).map(Expr.LocalExpr.tupled)
  val lookupSlice: P[Expr] = P( expr ~ "[" ~ expr.? ~ (":" ~ expr).rep ~ "]" ).map{
    case (prefix, Some(tree), Seq()) => Expr.Lookup(prefix, tree)
    case (prefix, start, ins) => Expr.Slice(prefix, start, ins.lift(0), ins.lift(1))
  }
  val comp: P[Expr] = P( "[" ~ expr ~ ",".? ~ forspec ~ compspec ~ "]" ).map(Expr.Comp.tupled)
  val apply: P[Expr] = P( expr ~ "(" ~ args ~ ")" ).map(Expr.Apply.tupled)
  val inSuper: P[Expr] = P( expr ~ "in" ~ "super" )
  val objExtend: P[Expr] = P( expr ~ "{" ~ objinside ~ "}" ).map(Expr.ObjExtend.tupled)
  val expr: P[Expr] = P(
    `null` | `true` | `false` | `self` | $ | string.map(Expr.Str) | number |
    obj | arr | comp
    | (expr ~ "." ~ id).map(Expr.Select.tupled)
    | lookupSlice
    | (`super` ~ "." ~ id).map(Expr.Select.tupled)
    | (`super` ~ "[" ~ expr ~ "]").map(Expr.Lookup.tupled)
    | apply
    | id.map(Expr.Id)
    | localExpr
    | ifElse
    | (expr ~ binaryop ~ expr).map(Expr.BinaryOp.tupled)
    | (unaryop ~ expr).map(Expr.UnaryOp.tupled)
    | objExtend
    | function
    | assertExpr
    | `import`
    | importstr
    | error
    | inSuper
  )

  val objinside: P[Expr.ObjBody] = P( memberList | objComp )
  val memberList = P( member.rep(sep=",") ~ ",".? ).map(Expr.ObjBody.MemberList)
  val objComp = P(
    (objlocal ~ ",").rep ~ "[" ~ expr ~ "]" ~ ":" ~ expr ~ ("," ~ objlocal).rep  ~ ",".? ~ forspec ~ compspec
  ).map(Expr.ObjBody.ObjComp.tupled)

  val member: P[Expr.Member] = P( objlocal | assertStmt | field )
  val field = P(
    (fieldname ~ "+".!.? ~ ("(" ~ params.? ~ ")").? ~ h ~ expr).map{
      case (name, plus, p, h2, e) =>
        Expr.Member.Field(name, plus.nonEmpty, p.flatten.getOrElse(Expr.Params(Nil)), h2, e)
    }
  )
  val h = P( ":" | "::" | ":::" ).!
  val objlocal = P( "local" ~ bind ).map(Expr.Member.BindStmt)
  val compspec: P[Seq[Expr.CompSpec]] = P( (forspec | ifspec).rep )
  val forspec = P( "for" ~ id ~ "in" ~ expr ).map(Expr.ForSpec.tupled)
  val ifspec = P( "if" ~ expr ).map(Expr.IfSpec)
  val fieldname = P( id.map(Expr.Id) | string.map(Expr.Id) | "[" ~ expr.map(Expr.DynFieldName) ~ "]" )
  val assertStmt = P( "assert" ~ expr ~ (":" ~ expr).? ).map(Expr.Member.AssertStmt.tupled)
  val bind = P( id ~ ("(" ~ params.? ~ ")").?.map(_.flatten) ~ "=" ~ expr ).map(Expr.Bind.tupled)
  val args = P( ((id ~ "=").? ~ expr).rep(sep = ",") ~ ",".? ).map(Expr.Args)

  val params: P[Expr.Params] = P( (id ~ ("=" ~ expr).?).rep(sep=",") ~ ",".? ).map(Expr.Params)

  val binaryop = P("*" | "/" | "%" | "+" | "-" | "<<" | ">>" | "<" | "<=" | ">" | ">=" | "==" | "!=" | "in" | "&" | "^" | "|" | "&&" | "||" ).!
  val unaryop	= P("-" | "+" | "!" | "~").!

}