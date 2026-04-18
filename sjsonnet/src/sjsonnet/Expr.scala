package sjsonnet

import java.util
import scala.collection.immutable.IntMap

/**
 * [[Expr]]s are the parsed syntax trees of a Jsonnet program. They model the program mostly
 * as-written, except for resolving local variable names and assigning them indices in the scope
 * bindings array.
 *
 * Each [[Expr]] represents an expression in the Jsonnet program, and contains an integer offset
 * into the file that is later used to provide error messages.
 *
 * '''Why `abstract class` and not `trait`?''' This is an intentional JVM performance decision.
 * Method calls on a class reference use `invokevirtual` (O(1) vtable lookup), while calls on a
 * trait/interface reference use `invokeinterface` (itable search, ~2-5ns slower per call). The
 * evaluator calls `e.tag` and pattern-matches on `Expr`-typed references millions of times per
 * evaluation; `instanceof` checks against a class hierarchy are also cheaper than against an
 * interface (single depth-indexed comparison vs. interface table scan). Since `Expr` does not
 * require multiple inheritance — no class mixes in `Expr` alongside an unrelated base class — an
 * `abstract class` is strictly faster with no loss of flexibility.
 *
 * @see
 *   [[Val]] which extends `Expr` to unify the type hierarchy for dispatch efficiency.
 */
abstract class Expr {

  /**
   * Source position of this expression, used for error messages.
   *
   * This is a `var` (not `val` or `def`) so that [[StaticOptimizer]] can update positions in-place
   * when it replaces an expression with a sub-expression (e.g. branch elimination:
   * `IfElse(pos, true, thenExpr, _)` → `thenExpr` with `thenExpr.pos = pos`). Without mutable
   * `pos`, the optimizer would need to wrap the result in a `LocalExpr` just to carry the original
   * position, adding unnecessary AST nodes. Do not change this to `val` or `def`.
   */
  var pos: Position
  private[sjsonnet] def tag: Byte = ExprTags.UNTAGGED

  /** The name of this expression type to be shown in error messages */
  def exprErrorString: String = {
    val n = getClass.getName
    if (n.startsWith("sjsonnet.Expr$")) n.substring(14) else n
  }

  override def toString: String = s"$exprErrorString@$pos"
}

/**
 * Marker trait for [[Expr]] nodes that represent function calls eligible for tail-call
 * optimization. All Apply* (user function calls) and ApplyBuiltin* (built-in function calls) mix in
 * this trait, providing a uniform `tailstrict` flag. The evaluator handles the two families
 * differently when `tailstrict` is true:
 *
 *   - '''User function calls''' (Apply*) in tail position: the evaluator constructs a [[TailCall]]
 *     sentinel and returns it to the caller's [[TailCall.resolve]] trampoline loop, avoiding JVM
 *     stack growth for tail-recursive calls.
 *   - '''Built-in function calls''' (ApplyBuiltin*): the evaluator wraps the result in
 *     [[TailCall.resolve]] at the call site, resolving any [[TailCall]] that a user-defined
 *     callback (e.g. the function argument to `std.makeArray` or `std.sort`) may have returned.
 *
 * @see
 *   [[TailCall]] for the sentinel value used in the TCO protocol
 */
trait TailstrictableExpr extends Expr {
  def tailstrict: Boolean

  /**
   * True when this call was marked as strict (eager argument evaluation) by an explicit
   * `tailstrict` annotation. False when auto-TCO'd (lazy argument evaluation to preserve Jsonnet's
   * standard lazy semantics).
   */
  def isStrict: Boolean = false
}

object Expr {
  private[sjsonnet] def callTargetName(value: Expr): String = value match {
    case ValidId(_, name, _) => name
    case Id(_, name)         => name
    case Select(_, _, name)  => name
    case f: Val.Builtin      => f.qualifiedName
    case f: Val.Func         =>
      val n = f.functionName
      if (n != null) n else "anonymous"
    case _ => "anonymous"
  }

  private final def arrStr(a: Array[?]): String = {
    if (a == null) "null" else a.mkString("[", ", ", "]")
  }

  final case class Self(var pos: Position) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Self
  }
  final case class Super(var pos: Position) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Super
  }
  final case class $(var pos: Position) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.`$`
  }

  final case class Id(var pos: Position, name: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Id
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }

  final case class ValidId(var pos: Position, name: String, nameIdx: Int) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.ValidId
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  final case class Arr(var pos: Position, value: Array[Expr]) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Arr
    override def toString: String = s"Arr($pos, ${arrStr(value)})"
  }

  sealed trait FieldName

  object FieldName {
    final case class Fixed(value: String) extends FieldName
    final case class Dyn(expr: Expr) extends FieldName
  }
  sealed trait Member

  object Member {
    sealed trait Visibility
    object Visibility {

      case object Normal extends Visibility
      case object Hidden extends Visibility
      case object Unhide extends Visibility
    }
    final case class Field(
        var pos: Position,
        fieldName: FieldName,
        plus: Boolean, // see https://jsonnet.org/ref/language.html#nested-field-inheritance
        args: Params,
        sep: Visibility,
        rhs: Expr)
        extends Member {
      def isStatic: Boolean = fieldName
        .isInstanceOf[FieldName.Fixed] && !plus && args == null && sep == Visibility.Normal && rhs
        .isInstanceOf[Val.Literal]
    }
    final case class AssertStmt(value: Expr, msg: Expr) extends Member
  }

  final case class Params(names: Array[String], defaultExprs: Array[Expr]) {
    val paramMap: Map[String, Int] = names.zipWithIndex.toMap
    override def toString: String = s"Params(${arrStr(names)}, ${arrStr(defaultExprs)})"
  }

  final case class UnaryOp(var pos: Position, op: Int, value: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.UnaryOp
    override def exprErrorString: String = s"${super.exprErrorString} ${UnaryOp.name(op)}"
  }
  object UnaryOp {
    final val OP_! = 0
    final val OP_- = 1
    final val OP_~ = 2
    final val OP_+ = 3
    private val names = IntMap(OP_! -> "!", OP_- -> "-", OP_~ -> "~", OP_+ -> "+")
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  final case class And(var pos: Position, lhs: Expr, rhs: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.And
  }
  final case class Or(var pos: Position, lhs: Expr, rhs: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Or
  }
  final case class BinaryOp(var pos: Position, lhs: Expr, op: Int, rhs: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.BinaryOp
    override def exprErrorString: String = s"${super.exprErrorString} ${BinaryOp.name(op)}"
  }
  object BinaryOp {
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
    private val names = IntMap(
      OP_* -> "*",
      OP_/ -> "/",
      OP_% -> "%",
      OP_+ -> "+",
      OP_- -> "-",
      OP_<< -> "<<",
      OP_>> -> ">>",
      OP_< -> "<",
      OP_> -> ">",
      OP_<= -> "<=",
      OP_>= -> ">=",
      OP_in -> "in",
      OP_== -> "==",
      OP_!= -> "!=",
      OP_& -> "&",
      OP_^ -> "^",
      OP_| -> "|",
      OP_&& -> "&&",
      OP_|| -> "||"
    )
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  final case class AssertExpr(var pos: Position, asserted: Member.AssertStmt, returned: Expr)
      extends Expr {
    final override private[sjsonnet] def tag = ExprTags.AssertExpr
  }
  final case class LocalExpr(var pos: Position, bindings: Array[Bind], returned: Expr)
      extends Expr {
    final override private[sjsonnet] def tag = ExprTags.LocalExpr
    override def toString: String = s"LocalExpr($pos, ${arrStr(bindings)}, $returned)"
    override def equals(o: Any): Boolean = o match {
      case o: LocalExpr =>
        pos == o.pos && util.Arrays.equals(
          bindings.asInstanceOf[Array[AnyRef]],
          o.bindings.asInstanceOf[Array[AnyRef]]
        ) && returned == o.returned
      case _ => false
    }
  }

  final case class Bind(var pos: Position, name: String, args: Params, rhs: Expr) extends Member
  final case class Import(var pos: Position, value: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Import
  }
  final case class ImportStr(var pos: Position, value: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.ImportStr
  }
  final case class ImportBin(var pos: Position, value: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.ImportBin
  }
  final case class Error(var pos: Position, value: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Error
  }
  final case class Apply(
      var pos: Position,
      value: Expr,
      args: Array[Expr],
      namedNames: Array[String],
      tailstrict: Boolean,
      strict: Boolean = true)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.Apply
    override def exprErrorString: String = Expr.callTargetName(value)
    override def isStrict: Boolean = strict
  }
  final case class Apply0(
      var pos: Position,
      value: Expr,
      tailstrict: Boolean,
      strict: Boolean = true)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.Apply0
    override def exprErrorString: String = Expr.callTargetName(value)
    override def isStrict: Boolean = strict
  }
  final case class Apply1(
      var pos: Position,
      value: Expr,
      a1: Expr,
      tailstrict: Boolean,
      strict: Boolean = true)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.Apply1
    override def exprErrorString: String = Expr.callTargetName(value)
    override def isStrict: Boolean = strict
  }
  final case class Apply2(
      var pos: Position,
      value: Expr,
      a1: Expr,
      a2: Expr,
      tailstrict: Boolean,
      strict: Boolean = true)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.Apply2
    override def exprErrorString: String = Expr.callTargetName(value)
    override def isStrict: Boolean = strict
  }
  final case class Apply3(
      var pos: Position,
      value: Expr,
      a1: Expr,
      a2: Expr,
      a3: Expr,
      tailstrict: Boolean,
      strict: Boolean = true)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.Apply3
    override def exprErrorString: String = Expr.callTargetName(value)
    override def isStrict: Boolean = strict
  }
  final case class ApplyBuiltin(
      var pos: Position,
      func: Val.Builtin,
      argExprs: Array[Expr],
      tailstrict: Boolean)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.ApplyBuiltin
    override def exprErrorString: String = func.qualifiedName
  }
  final case class ApplyBuiltin0(var pos: Position, func: Val.Builtin0, tailstrict: Boolean)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.ApplyBuiltin0
    override def exprErrorString: String = func.qualifiedName
  }
  final case class ApplyBuiltin1(
      var pos: Position,
      func: Val.Builtin1,
      a1: Expr,
      tailstrict: Boolean)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.ApplyBuiltin1
    override def exprErrorString: String = func.qualifiedName
  }
  final case class ApplyBuiltin2(
      var pos: Position,
      func: Val.Builtin2,
      a1: Expr,
      a2: Expr,
      tailstrict: Boolean)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.ApplyBuiltin2
    override def exprErrorString: String = func.qualifiedName
  }
  final case class ApplyBuiltin3(
      var pos: Position,
      func: Val.Builtin3,
      a1: Expr,
      a2: Expr,
      a3: Expr,
      tailstrict: Boolean)
      extends TailstrictableExpr {
    final override private[sjsonnet] def tag = ExprTags.ApplyBuiltin3
    override def exprErrorString: String = func.qualifiedName
  }
  final case class ApplyBuiltin4(
      var pos: Position,
      func: Val.Builtin4,
      a1: Expr,
      a2: Expr,
      a3: Expr,
      a4: Expr,
      tailstrict: Boolean)
      extends TailstrictableExpr {
    override private[sjsonnet] def tag = ExprTags.ApplyBuiltin4
    override def exprErrorString: String = func.qualifiedName
  }
  final case class Select(var pos: Position, value: Expr, name: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Select
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  final case class SelectSuper(var pos: Position, selfIdx: Int, name: String) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.SelectSuper
    override def exprErrorString: String = s"${super.exprErrorString} $name"
  }
  final case class InSuper(var pos: Position, value: Expr, selfIdx: Int) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.InSuper
  }
  final case class Lookup(var pos: Position, value: Expr, index: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Lookup
  }
  final case class LookupSuper(var pos: Position, selfIdx: Int, index: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.LookupSuper
  }
  final case class Slice(
      var pos: Position,
      value: Expr,
      start: Option[Expr],
      end: Option[Expr],
      stride: Option[Expr])
      extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Slice
  }
  final case class Function(var pos: Position, params: Params, body: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Function
  }
  final case class IfElse(var pos: Position, cond: Expr, `then`: Expr, `else`: Expr) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.IfElse
  }

  sealed trait CompSpec extends Expr
  final case class IfSpec(var pos: Position, cond: Expr) extends CompSpec
  final case class ForSpec(var pos: Position, name: String, cond: Expr) extends CompSpec

  final case class Comp(var pos: Position, value: Expr, first: ForSpec, rest: Array[CompSpec])
      extends Expr {
    final override private[sjsonnet] def tag = ExprTags.Comp
    override def exprErrorString: String = "array comprehension"
  }
  final case class ObjExtend(var pos: Position, base: Expr, ext: ObjBody) extends Expr {
    final override private[sjsonnet] def tag = ExprTags.ObjExtend
  }

  trait ObjBody extends Expr
  object ObjBody {
    final case class MemberList(
        var pos: Position,
        binds: Array[Bind],
        fields: Array[Member.Field],
        asserts: Array[Member.AssertStmt])
        extends ObjBody {
      final override private[sjsonnet] def tag = ExprTags.`ObjBody.MemberList`

      /**
       * Cached sorted field order for inline objects. Computed once, shared across all Val.Obj
       * instances created from this MemberList.
       */
      @volatile var _cachedSortedOrder: Array[Int] = null

      /**
       * Cached flag: true if no field body references `self` or `super` at this object scope level.
       * When true, the Materializer can skip `cacheFieldValue()` during inline object iteration,
       * eliminating HashMap allocations for objects with >2 fields. Null means not yet computed.
       */
      @volatile var _noSelfRef: java.lang.Boolean = null

      /**
       * Cached key-name arrays shared across all Val.Obj instances from this MemberList (when super
       * == null). Computed from the first instance and reused by subsequent ones, avoiding per-object
       * allKeyNames/visibleKeyNames allocation.
       */
      var _cachedAllKeyNames: Array[String] = null
      var _cachedVisibleKeyNames: Array[String] = null

      /** True if all fields have compile-time fixed names (no computed/Dyn field names). */
      lazy val allFieldNamesFixed: Boolean =
        fields.forall(_.fieldName.isInstanceOf[FieldName.Fixed])

      override def toString: String =
        s"MemberList($pos, ${arrStr(binds)}, ${arrStr(fields)}, ${arrStr(asserts)})"
    }
    final case class ObjComp(
        var pos: Position,
        preLocals: Array[Bind],
        key: Expr,
        value: Expr,
        plus: Boolean, // see https://jsonnet.org/ref/language.html#nested-field-inheritance
        postLocals: Array[Bind],
        first: ForSpec,
        rest: List[CompSpec])
        extends ObjBody {
      final override private[sjsonnet] def tag = ExprTags.`ObjBody.ObjComp`
      override def exprErrorString: String = "object comprehension"
      override def toString: String =
        s"ObjComp($pos, ${arrStr(preLocals)}, $key, $value, ${arrStr(postLocals)}, $first, $rest)"
    }
  }
}

private[sjsonnet] object ExprTags {
  // used for optimization only, used in Evaluator#visitExpr
  // the order is the same as the origin order in Evaluator#visitExpr
  final val UNTAGGED =
    0 // A special value used to indicate that the expression is not tagged for optimization
  final val ValidId = 1
  final val BinaryOp = 2
  final val Select = 3
  final val `Val.Literal` = 4
  final val `Val.Func` = 5
  final val ApplyBuiltin0 = 6
  final val ApplyBuiltin1 = 7
  final val ApplyBuiltin2 = 8
  final val ApplyBuiltin3 = 9
  final val ApplyBuiltin4 = 10
  final val And = 11
  final val Or = 12
  final val UnaryOp = 13
  final val Apply1 = 14
  final val Lookup = 15
  final val Function = 16
  final val LocalExpr = 17
  final val Apply = 18
  final val IfElse = 19
  final val Apply3 = 20
  final val `ObjBody.MemberList` = 21
  final val Apply2 = 22
  final val AssertExpr = 23
  final val ApplyBuiltin = 24
  final val Comp = 25
  final val Arr = 26
  final val SelectSuper = 27
  final val LookupSuper = 28
  final val InSuper = 29
  final val ObjExtend = 30
  final val `ObjBody.ObjComp` = 31
  final val Slice = 32
  final val Import = 33
  final val Apply0 = 34
  final val ImportStr = 35
  final val ImportBin = 36
  final val Error = 37
  // used in Evaluator#visitInvalid
  final val Id = 0
  final val Self = 1
  final val `$` = 2
  final val Super = 3
  // other
}
