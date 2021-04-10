package sjsonnet

import java.util.BitSet

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
}
object Expr{

  case class Self(pos: Position) extends Expr
  case class Super(pos: Position) extends Expr
  case class $(pos: Position) extends Expr

  case class Id(pos: Position, value: Int) extends Expr
  case class Arr(pos: Position, value: Array[Expr]) extends Expr

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


  case class Params(names: Array[String], defaultExprs: Array[Expr], indices: Array[Int]){
    val argIndices: Map[String, Int] = (names, indices).zipped.toMap
    val noDefaultIndices: BitSet = {
      val b = new BitSet(defaultExprs.size)
      (defaultExprs, indices).zipped.foreach((e, i) => if(e == null) b.set(i))
      b
    }
    val defaultsOnlyIndices: Array[Int] = (defaultExprs, indices).zipped.collect { case (x, i) if x != null => i }.toArray
    val defaultsOnly: Array[Expr] = defaultExprs.filter(_ != null).toArray
    val allIndices: BitSet = {
      val b = new BitSet(indices.size)
      indices.foreach(b.set)
      b
    }
  }
  case class Args(names: Array[String], exprs: Array[Expr])

  case class UnaryOp(pos: Position, op: UnaryOp.Op, value: Expr) extends Expr
  object UnaryOp{
    sealed trait Op
    case object `+` extends Op
    case object `-` extends Op
    case object `~` extends Op
    case object `!` extends Op
  }
  case class BinaryOp(pos: Position, lhs: Expr, op: BinaryOp.Op, rhs: Expr) extends Expr
  object BinaryOp{
    sealed trait Op
    case object `*` extends Op
    case object `/` extends Op
    case object `%` extends Op
    case object `+` extends Op
    case object `-` extends Op
    case object `<<` extends Op
    case object `>>` extends Op
    case object `<` extends Op
    case object `>` extends Op
    case object `<=` extends Op
    case object `>=` extends Op
    case object `in` extends Op
    case object `==` extends Op
    case object `!=` extends Op
    case object `&` extends Op
    case object `^` extends Op
    case object `|` extends Op
    case object `&&` extends Op
    case object `||` extends Op
  }
  case class AssertExpr(pos: Position, asserted: Member.AssertStmt, returned: Expr) extends Expr
  case class LocalExpr(pos: Position, bindings: Array[Bind], returned: Expr) extends Expr

  case class Bind(pos: Position, name: Int, args: Params, rhs: Expr) extends Member
  case class Import(pos: Position, value: String) extends Expr
  case class ImportStr(pos: Position, value: String) extends Expr
  case class Error(pos: Position, value: Expr) extends Expr
  case class Apply(pos: Position, value: Expr, argNames: Array[String], argExprs: Array[Expr]) extends Expr
  case class Select(pos: Position, value: Expr, name: String) extends Expr
  case class Lookup(pos: Position, value: Expr, index: Expr) extends Expr
  case class Slice(pos: Position,
                   value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr
  case class Function(pos: Position, params: Params, body: Expr) extends Expr
  case class IfElse(pos: Position, cond: Expr, then: Expr, `else`: Expr) extends Expr

  sealed trait CompSpec extends Expr
  case class IfSpec(pos: Position, cond: Expr) extends CompSpec
  case class ForSpec(pos: Position, name: Int, cond: Expr) extends CompSpec

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
                       rest: List[CompSpec]) extends ObjBody
  }

}
