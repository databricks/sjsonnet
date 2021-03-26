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
sealed trait Expr{
  def offset: Int
}
object Expr{
  case class Null(offset: Int) extends Expr
  case class True(offset: Int) extends Expr
  case class False(offset: Int) extends Expr
  case class Self(offset: Int) extends Expr
  case class Super(offset: Int) extends Expr
  case class $(offset: Int) extends Expr

  case class Str(offset: Int, value: String) extends Expr
  case class Num(offset: Int, value: Double) extends Expr
  case class Id(offset: Int, value: Int) extends Expr
  case class Arr(offset: Int, value: Seq[Expr]) extends Expr
  case class Obj(offset: Int, value: ObjBody) extends Expr

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
    case class Field(offset: Int,
                     fieldName: FieldName,
                     plus: Boolean,
                     args: Option[Params],
                     sep: Visibility,
                     rhs: Expr) extends Member
    case class BindStmt(value: Bind) extends Member
    case class AssertStmt(value: Expr, msg: Option[Expr]) extends Member
  }


  case class Parened(offset: Int, value: Expr) extends Expr
  //case class Params(args: IndexedSeq[(String, Option[Expr], Int)]){
  case class Params(names: Array[String], defaultExprs: Array[Expr], indices: Array[Int]){
    val argIndices: Map[String, Int] = (names, indices).zipped.toMap
    val noDefaultIndices: BitSet = {
      val b = new BitSet(defaultExprs.size)
      (defaultExprs, indices).zipped.foreach((e, i) => if(e == null) b.set(i))
      b
    }
    val defaultsOnlyIndices: Array[Int] = (defaultExprs, indices).zipped.collect { case (x, i) if x != null => i }.toArray
    val defaultsOnly: Array[Expr] = defaultExprs.filter(_ != null).toArray
    //val defaults: IndexedSeq[(Int, Expr)] = args.collect{case (_, Some(x), i) => (i, x)}
    val allIndices: BitSet = {
      val b = new BitSet(indices.size)
      indices.foreach(b.set)
      b
    }
  }
  object Params {
    def mk(params: (String, Option[Expr], Int)*): Params = {
      Params(params.map(_._1).toArray, params.map(_._2.getOrElse(null)).toArray, params.map(_._3).toArray)
    }
  }
  case class Args(args: Seq[(Option[String], Expr)])

  case class UnaryOp(offset: Int, op: UnaryOp.Op, value: Expr) extends Expr
  object UnaryOp{
    sealed trait Op
    case object `+` extends Op
    case object `-` extends Op
    case object `~` extends Op
    case object `!` extends Op
  }
  case class BinaryOp(offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr) extends Expr
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
  case class AssertExpr(offset: Int, asserted: Member.AssertStmt, returned: Expr) extends Expr
  case class LocalExpr(offset: Int, bindings: Seq[Bind], returned: Expr) extends Expr

  case class Bind(offset: Int, name: Int, args: Option[Params], rhs: Expr)
  case class Import(offset: Int, value: String) extends Expr
  case class ImportStr(offset: Int, value: String) extends Expr
  case class Error(offset: Int, value: Expr) extends Expr
  case class Apply(offset: Int, value: Expr, argNames: Array[String], argExprs: Array[Expr]) extends Expr
  case class Select(offset: Int, value: Expr, name: String) extends Expr
  case class Lookup(offset: Int, value: Expr, index: Expr) extends Expr
  case class Slice(offset: Int,
                   value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr
  case class Function(offset: Int, params: Params, body: Expr) extends Expr
  case class IfElse(offset: Int, cond: Expr, then: Expr, `else`: Option[Expr]) extends Expr

  sealed trait CompSpec extends Expr
  case class IfSpec(offset: Int, cond: Expr) extends CompSpec
  case class ForSpec(offset: Int, name: Int, cond: Expr) extends CompSpec

  case class Comp(offset: Int, value: Expr, first: ForSpec, rest: Seq[CompSpec]) extends Expr
  case class ObjExtend(offset: Int, base: Expr, ext: ObjBody) extends Expr

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
