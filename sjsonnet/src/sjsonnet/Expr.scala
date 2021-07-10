package sjsonnet

import java.util.BitSet

import scala.collection.mutable

/**
  * [[Expr]]s are the parsed syntax trees of a Jsonnet program. They model the
  * program mostly as-written, except for resolving local variable names and
  * assigning them indices in the scope bindings array.
  *
  * Each [[Expr]] represents an expression in the Jsonnet program, and contains an
  * integer offset into the file that is later used to provide error messages.
  */
trait Expr{
  def pos: Position

  /** The name of this expression type to be shown in error messages */
  def exprErrorString: String = {
    val n = getClass.getName
    if(n.startsWith("sjsonnet.Expr$")) n.substring(14) else n
  }
}
object Expr{
  private final def arrStr(a: Array[_]): String = {
    if(a == null) "null" else a.mkString("[", ", ", "]")
  }

  case class Self(pos: Position) extends Expr
  case class Super(pos: Position) extends Expr
  case class $(pos: Position) extends Expr

  case class Id(pos: Position, name: String) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  case class ValidId(pos: Position, name: String, nameIdx: Int) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  case class Arr(pos: Position, value: Array[Expr]) extends Expr {
    override def toString = s"Arr($pos, ${arrStr(value)})"
  }

  sealed trait FieldName

  object FieldName{
    case class Fixed(value: String) extends FieldName
    case class Dyn(expr: Expr) extends FieldName
  }
  sealed trait Member

  object Member{
    sealed trait Visibility
    object Visibility{

      case object Normal extends Visibility
      case object Hidden extends Visibility
      case object Unhide extends Visibility
    }
    case class Field(pos: Position,
                     fieldName: FieldName,
                     plus: Boolean,
                     args: Params,
                     sep: Visibility,
                     rhs: Expr) extends Member {
      def isStatic = fieldName.isInstanceOf[FieldName.Fixed] && !plus && args == null && sep == Visibility.Normal && rhs.isInstanceOf[Val.Literal]
    }
    case class AssertStmt(value: Expr, msg: Expr) extends Member
  }

  case class Params(names: Array[String], defaultExprs: Array[Expr]){
    val paramMap = names.zipWithIndex.toMap
    override def toString = s"Params(${arrStr(names)}, ${arrStr(defaultExprs)})"
  }

  case class UnaryOp(pos: Position, op: Int, value: Expr) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} ${UnaryOp.name(op)}"
  }
  object UnaryOp{
    final val OP_! = 0
    final val OP_- = 1
    final val OP_~ = 2
    final val OP_+ = 3
    private val names = Map(OP_! -> "!", OP_- -> "-", OP_~ -> "~", OP_+ -> "+")
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  case class And(pos: Position, lhs: Expr, rhs: Expr) extends Expr
  case class Or(pos: Position, lhs: Expr, rhs: Expr) extends Expr
  case class BinaryOp(pos: Position, lhs: Expr, op: Int, rhs: Expr) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} ${BinaryOp.name(op)}"
  }
  object BinaryOp{
    final val OP_* = 0
    final val OP_/ = 1
    final val OP_% = 2
    final val OP_+ = 3
    final val OP_- = 4
    final val OP_<< = 5
    final val OP_>> = 6
    final val OP_< = 7
    final val OP_> = 8
    final val OP_<= = 9
    final val OP_>= = 10
    final val OP_in = 11
    final val OP_== = 12
    final val OP_!= = 13
    final val OP_& = 14
    final val OP_^ = 15
    final val OP_| = 16
    final val OP_&& = 17
    final val OP_|| = 18
    private val names = Map(OP_* -> "*", OP_/ -> "/", OP_% -> "%", OP_+ -> "+", OP_- -> "-", OP_<< -> "<<",
      OP_>> -> ">>", OP_< -> "<", OP_> -> ">", OP_<= -> "<=", OP_>= -> ">=", OP_in -> "in", OP_== -> "==",
      OP_!= -> "!=", OP_& -> "&", OP_^ -> "^", OP_| -> "|", OP_&& -> "&&", OP_|| -> "||" )
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  case class AssertExpr(pos: Position, asserted: Member.AssertStmt, returned: Expr) extends Expr
  case class LocalExpr(pos: Position, bindings: Array[Bind], returned: Expr) extends Expr {
    override def toString = s"LocalExpr($pos, ${arrStr(bindings)}, $returned)"
  }

  case class Bind(pos: Position, name: String, args: Params, rhs: Expr) extends Member
  case class Import(pos: Position, value: String) extends Expr
  case class ImportStr(pos: Position, value: String) extends Expr
  case class Error(pos: Position, value: Expr) extends Expr
  case class Apply(pos: Position, value: Expr, args: Array[Expr], namedNames: Array[String]) extends Expr
  case class Apply0(pos: Position, value: Expr) extends Expr
  case class Apply1(pos: Position, value: Expr, a1: Expr) extends Expr
  case class Apply2(pos: Position, value: Expr, a1: Expr, a2: Expr) extends Expr
  case class Apply3(pos: Position, value: Expr, a1: Expr, a2: Expr, a3: Expr) extends Expr
  case class ApplyBuiltin(pos: Position, func: Val.Builtin, argExprs: Array[Expr]) extends Expr
  case class ApplyBuiltin1(pos: Position, func: Val.Builtin1, a1: Expr) extends Expr
  case class ApplyBuiltin2(pos: Position, func: Val.Builtin2, a1: Expr, a2: Expr) extends Expr
  case class Select(pos: Position, value: Expr, name: String, safe: Boolean = false) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  case class SelectSuper(pos: Position, selfIdx: Int, name: String, safe: Boolean = false) extends Expr {
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  case class InSuper(pos: Position, value: Expr, selfIdx: Int) extends Expr
  case class Lookup(pos: Position, value: Expr, index: Expr) extends Expr
  case class LookupSuper(pos: Position, selfIdx: Int, index: Expr) extends Expr
  case class Slice(pos: Position,
                   value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr
  case class Function(pos: Position, params: Params, body: Expr) extends Expr
  case class IfElse(pos: Position, cond: Expr, then: Expr, `else`: Expr) extends Expr

  sealed trait CompSpec extends Expr
  case class IfSpec(pos: Position, cond: Expr) extends CompSpec
  case class ForSpec(pos: Position, name: String, cond: Expr) extends CompSpec

  case class Comp(pos: Position, value: Expr, first: ForSpec, rest: Array[CompSpec]) extends Expr
  case class ObjExtend(pos: Position, base: Expr, ext: ObjBody) extends Expr

  trait ObjBody extends Expr
  object ObjBody{
    case class MemberList(pos: Position, binds: Array[Bind], fields: Array[Member.Field], asserts: Array[Member.AssertStmt]) extends ObjBody
    case class ObjComp(pos: Position,
                       preLocals: Array[Bind],
                       key: Expr,
                       value: Expr,
                       postLocals: Array[Bind],
                       first: ForSpec,
                       rest: List[CompSpec]) extends ObjBody {
      override def toString = s"ObjComp($pos, ${arrStr(preLocals)}, $key, $value, ${arrStr(postLocals)}, $first, $rest)"
    }
  }

}
