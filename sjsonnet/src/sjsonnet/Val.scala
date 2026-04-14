package sjsonnet

import java.util
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * [[Eval]] is the common parent trait for both lazy and eager evaluation, providing a unified
 * abstraction for evaluation strategies within a Jsonnet program.
 */
trait Eval {
  def value: Val
}

/**
 * Abstract marker base for deferred (lazy) evaluation. Contains no fields — subclasses manage their
 * own caching to minimize per-instance memory.
 *
 * Hierarchy (allocation percentages measured across 591 test and benchmark files; actual
 * distribution varies by workload):
 *   - [[LazyFunc]] — wraps a `() => Val` closure with a separate `cached` field (~0.1%)
 *   - [[LazyExpr]] — closure-free `visitExpr` thunk, repurposes fields for caching (~91%)
 *   - [[LazyApply1]] — closure-free `func.apply1` thunk (~9%)
 *   - [[LazyApply2]] — closure-free `func.apply2` thunk (<1%)
 *
 * @see
 *   [[Eval]] the parent trait shared with [[Val]] (eager values).
 */
abstract class Lazy extends Eval

/**
 * Closure-based [[Lazy]]: wraps an arbitrary `() => Val` thunk.
 *
 * Used for deferred evaluations that don't fit the specialized [[LazyExpr]]/[[LazyApply1]]/
 * [[LazyApply2]] patterns, e.g. `visitMethod` (local function defs), `visitBindings` (object field
 * bindings), and default parameter evaluation. These account for <1% of all deferred evaluations
 * (profiled across 591 benchmark and test files).
 */
final class LazyFunc(private var f: () => Val) extends Lazy {
  private var cached: Val = _
  def value: Val = {
    if (cached != null) return cached
    cached = f()
    f = null // allow GC of captured references
    cached
  }
}

/**
 * Closure-free [[Lazy]] that defers `evaluator.visitExpr(expr)(scope)`.
 *
 * Used in [[Evaluator.visitAsLazy]] instead of `new LazyFunc(() => visitExpr(e)(scope))`. By
 * storing (expr, scope, evaluator) as fields rather than capturing them in a closure, this cuts
 * per-thunk allocation from 2 JVM objects (LazyFunc + closure) to 1 (LazyExpr), and from 56B to 24B
 * (compressed oops).
 *
 * Profiling across all benchmark and test suites (591 files) shows [[Evaluator.visitAsLazy]]
 * produces ~91% of all deferred evaluations.
 *
 * After computation, the cached [[Val]] is stored in the `exprOrVal` field (which originally held
 * the [[Expr]]), and `ev` is nulled as a sentinel. `scope` is also cleared to allow GC.
 */
final class LazyExpr(
    private var exprOrVal: AnyRef, // Expr before compute, Val after
    private var scope: ValScope,
    private var ev: Evaluator)
    extends Lazy {
  def value: Val = {
    if (ev == null) exprOrVal.asInstanceOf[Val]
    else {
      val r = ev.visitExpr(exprOrVal.asInstanceOf[Expr])(scope)
      exprOrVal = r // cache result
      scope = null.asInstanceOf[sjsonnet.ValScope] // allow GC
      ev = null // sentinel: marks as computed
      r
    }
  }
}

/**
 * Closure-free [[Lazy]] that defers `func.apply1(arg, pos)(ev, TailstrictModeDisabled)`.
 *
 * Used in stdlib builtins (`std.map`, `std.filterMap`, `std.makeArray`, etc.) to eliminate the
 * 2-object allocation (LazyFunc + Function0 closure), cutting from 56B to 32B per instance. After
 * computation, `funcOrVal` caches the result, `ev == null` serves as the computed sentinel, and
 * remaining fields are cleared for GC.
 */
final class LazyApply1(
    private var funcOrVal: AnyRef, // Val.Func before compute, Val after
    private var arg: Eval,
    private var pos: Position,
    private var ev: EvalScope)
    extends Lazy {
  def value: Val = {
    if (ev == null) funcOrVal.asInstanceOf[Val]
    else {
      val r = funcOrVal.asInstanceOf[Val.Func].apply1(arg, pos)(ev, TailstrictModeDisabled)
      funcOrVal = r
      arg = null; pos = null; ev = null
      r
    }
  }
}

/**
 * Closure-free [[Lazy]] that defers `func.apply2(arg1, arg2, pos)(ev, TailstrictModeDisabled)`.
 *
 * Used in stdlib builtins (`std.mapWithIndex`, etc.). Same field-repurposing strategy as
 * [[LazyApply1]], cutting from 56B to 32B per instance.
 */
final class LazyApply2(
    private var funcOrVal: AnyRef, // Val.Func before compute, Val after
    private var arg1: Eval,
    private var arg2: Eval,
    private var pos: Position,
    private var ev: EvalScope)
    extends Lazy {
  def value: Val = {
    if (ev == null) funcOrVal.asInstanceOf[Val]
    else {
      val r = funcOrVal.asInstanceOf[Val.Func].apply2(arg1, arg2, pos)(ev, TailstrictModeDisabled)
      funcOrVal = r
      arg1 = null; arg2 = null; pos = null; ev = null
      r
    }
  }
}

/**
 * Closure-free [[Lazy]] that defers `func.evalDefault(expr, scope, ev)`.
 *
 * Used in [[Val.Func.apply]] for default parameter evaluation, eliminating the 2-object allocation
 * (LazyFunc + Function0 closure) of the original pattern.
 */
final class LazyDefault(
    private var exprOrVal: AnyRef, // Expr before compute, Val after
    private var scope: ValScope,
    private var func: Val.Func,
    private var ev: EvalScope)
    extends Lazy {
  def value: Val = {
    if (ev == null) exprOrVal.asInstanceOf[Val]
    else {
      val r = func.evalDefault(exprOrVal.asInstanceOf[Expr], scope, ev)
      exprOrVal = r
      scope = null.asInstanceOf[ValScope]
      func = null
      ev = null
      r
    }
  }
}

/**
 * [[Val]]s represented Jsonnet values that are the result of evaluating a Jsonnet program. The
 * [[Val]] data structure is essentially a JSON tree, except evaluation of object attributes and
 * array contents are lazy, and the tree can contain functions.
 */
sealed abstract class Val extends Eval {
  final def value: Val = this

  /** Runtime type tag for O(1) dispatch in Materializer (tableswitch). */
  private[sjsonnet] def valTag: Byte

  def pos: Position
  def prettyName: String

  def cast[T: ClassTag: PrettyNamed]: T =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else Error.fail("Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName)

  private def failAs(err: String): Nothing =
    Error.fail("Wrong parameter type: expected " + err + ", got " + prettyName)

  def asString: String = failAs("String")
  def asBoolean: Boolean = failAs("Boolean")
  def asInt: Int = failAs("Int")
  def asLong: Long = failAs("Long")
  def asDouble: Double = failAs("Number")
  def asObj: Val.Obj = failAs("Object")
  def asArr: Val.Arr = failAs("Array")
  def asFunc: Val.Func = failAs("Function")
}

class PrettyNamed[T](val s: String)
object PrettyNamed {
  implicit val strName: PrettyNamed[Val.Str] = new PrettyNamed("string")
  implicit val numName: PrettyNamed[Val.Num] = new PrettyNamed("number")
  implicit val arrName: PrettyNamed[Val.Arr] = new PrettyNamed("array")
  implicit val boolName: PrettyNamed[Val.Bool] = new PrettyNamed("boolean")
  implicit val objName: PrettyNamed[Val.Obj] = new PrettyNamed("object")
  implicit val funName: PrettyNamed[Val.Func] = new PrettyNamed("function")
  implicit val nullName: PrettyNamed[Val.Null] = new PrettyNamed("null")
}
object Val {
  // Constants for safe double-to-int conversion
  // IEEE 754 doubles precisely represent integers up to 2^53, beyond which precision is lost
  private[sjsonnet] final val DOUBLE_MAX_SAFE_INTEGER = (1L << 53) - 1
  private[sjsonnet] final val DOUBLE_MIN_SAFE_INTEGER = -((1L << 53) - 1)

  // Runtime type tags for O(1) dispatch in Materializer (tableswitch).
  // Values 0-7 form a contiguous range enabling JVM tableswitch bytecode.
  private[sjsonnet] final val TAG_STR: Byte = 0
  private[sjsonnet] final val TAG_NUM: Byte = 1
  private[sjsonnet] final val TAG_TRUE: Byte = 2
  private[sjsonnet] final val TAG_FALSE: Byte = 3
  private[sjsonnet] final val TAG_NULL: Byte = 4
  private[sjsonnet] final val TAG_ARR: Byte = 5
  private[sjsonnet] final val TAG_OBJ: Byte = 6
  private[sjsonnet] final val TAG_FUNC: Byte = 7

  abstract class Literal extends Val with Expr {
    final override private[sjsonnet] def tag = ExprTags.`Val.Literal`
  }
  abstract class Bool extends Literal {
    override def asBoolean: Boolean = this.isInstanceOf[True]
  }

  /**
   * Shared singletons for runtime boolean results — avoids per-comparison allocation. WARNING:
   * These singletons have mutable `var pos` (inherited from Expr). Their `pos` must NEVER be
   * mutated. The evaluation model is single-threaded, but mutating shared singleton state would
   * corrupt all subsequent uses.
   */
  val staticTrue: Bool = True(new Position(null, -1))
  val staticFalse: Bool = False(new Position(null, -1))

  /**
   * Returns a shared singleton boolean. Use for runtime comparison results where position is not
   * needed.
   */
  def bool(b: Boolean): Bool = if (b) staticTrue else staticFalse

  def bool(pos: Position, b: Boolean): Bool = if (b) True(pos) else False(pos)

  /**
   * Pre-allocated pool of Val.Num for small non-negative integers 0–255. Used by Evaluator
   * arithmetic fast paths to avoid per-operation allocation. Position is synthetic — acceptable for
   * intermediate runtime results.
   */
  private val numCacheSize = 256
  private val numCache: Array[Num] = {
    val pos = new Position(null, -1)
    val arr = new Array[Num](numCacheSize)
    var i = 0
    while (i < numCacheSize) {
      arr(i) = Num(pos, i.toDouble)
      i += 1
    }
    arr
  }

  /**
   * Returns a cached Val.Num for small non-negative integers (0–255), or a fresh instance
   * otherwise. Use in evaluator arithmetic paths where the pos is not critical for error reporting.
   */
  def cachedNum(pos: Position, d: Double): Num = {
    val i = d.toInt
    // Use raw bits comparison to correctly distinguish -0.0 from +0.0
    // (IEEE-754: -0.0 == 0.0 is true, but they have different bit patterns
    // and different observable behavior e.g. via std.math.atan2)
    if (
      i >= 0 && i < numCacheSize &&
      java.lang.Double.doubleToRawLongBits(i.toDouble) == java.lang.Double.doubleToRawLongBits(d)
    )
      numCache(i)
    else Num(pos, d)
  }

  final case class True(var pos: Position) extends Bool {
    def prettyName = "boolean"
    private[sjsonnet] def valTag: Byte = TAG_TRUE
  }
  final case class False(var pos: Position) extends Bool {
    def prettyName = "boolean"
    private[sjsonnet] def valTag: Byte = TAG_FALSE
  }
  final case class Null(var pos: Position) extends Literal {
    def prettyName = "null"
    private[sjsonnet] def valTag: Byte = TAG_NULL
  }

  /**
   * Singleton null for runtime results where position is not meaningful. Safe in single-threaded
   * evaluation. See staticTrue/staticFalse for rationale.
   */
  val staticNull: Val.Null = Val.Null(new Position(null, -1))

  /**
   * Rope string: O(1) concatenation via inline tree nodes.
   *
   * Leaf nodes have `_str != null` and `_left == _right == null` — the common case (99%+ of all
   * strings). Concat nodes have `_str == null` and non-null children; the flat string is lazily
   * computed on first `.str` access, then cached and children cleared for GC.
   *
   * Single monomorphic class ensures optimal JIT inlining — no virtual dispatch on `.str`.
   */
  final class Str private[sjsonnet] (var pos: Position, private[sjsonnet] var _str: String)
      extends Literal {

    // DO NOT CHANGE to separate _left/_right fields.
    // WHY: A single nullable array reference keeps leaf objects at 24 bytes (same as the original
    // case class) under JVM compressed oops. Two separate Str fields would add +8 bytes → 32 bytes
    // per leaf, and 99%+ of all Str instances are leaves. The array indirection only matters on the
    // cold flatten path, which is amortized O(1) per character.
    private[sjsonnet] var _children: Array[Str] = null

    def prettyName = "string"
    private[sjsonnet] def valTag: Byte = TAG_STR

    /** Get the flat string, flattening the rope tree if needed. */
    def str: String = {
      val s = _str
      if (s != null) return s
      val flat = flattenIterative()
      _str = flat
      _children = null
      flat
    }

    override def asString: String = str

    /**
     * Iterative rope flattening — stack-safe for arbitrarily deep trees. For a left-leaning rope of
     * depth N (typical from repeated foldl concat), the ArrayDeque holds at most 2 elements.
     */
    private def flattenIterative(): String = {
      val stack = new java.util.ArrayDeque[Str](16)
      // Pre-compute total length for exact StringBuilder sizing — avoids resize+copy overhead.
      var totalLen = 0
      stack.push(this)
      while (!stack.isEmpty) {
        val node = stack.pop()
        val s = node._str
        if (s != null) {
          totalLen += s.length
        } else {
          val ch = node._children
          stack.push(ch(1))
          stack.push(ch(0))
        }
      }
      val sb = new java.lang.StringBuilder(totalLen)
      stack.push(this)
      while (!stack.isEmpty) {
        val node = stack.pop()
        val s = node._str
        if (s != null) {
          sb.append(s)
        } else {
          val ch = node._children
          // Push right first so left is processed first (LIFO)
          stack.push(ch(1))
          stack.push(ch(0))
        }
      }
      sb.toString
    }

    override def equals(other: Any): Boolean = other match {
      case o: Str => (this eq o) || str == o.str
      case _      => false
    }

    override def hashCode: Int = str.hashCode

    override def toString: String = s"Str($pos, $str)"
  }

  object Str {

    /** Create a leaf string node — zero overhead vs the old case class. */
    def apply(pos: Position, s: String): Str = new Str(pos, s)

    /** Backward-compatible extractor: `case Val.Str(pos, s) =>` still works. */
    def unapply(s: Str): Option[(Position, String)] = Some((s.pos, s.str))

    /**
     * O(1) rope concatenation. Falls back to eager concat for small flat strings to avoid rope node
     * overhead when the copy cost is negligible.
     */
    def concat(pos: Position, left: Str, right: Str): Str = {
      val ls = left._str
      val rs = right._str
      // Empty string elimination
      if (ls != null && ls.isEmpty) return right
      if (rs != null && rs.isEmpty) return left
      // Small string eagerness: both flat and combined length <= 128
      if (ls != null && rs != null && ls.length + rs.length <= 128)
        return new Str(pos, ls + rs)
      // Rope node: O(1)
      val node = new Str(pos, null)
      node._children = Array(left, right)
      node
    }
  }
  final case class Num(var pos: Position, private val num: Double) extends Literal {
    if (num.isInfinite) {
      Error.fail("overflow")
    }

    def prettyName = "number"

    /**
     * Access the raw double value without NaN check. Safe for internal comparison and equality
     * operations where IEEE 754 NaN semantics are acceptable. NaN values cannot arise from valid
     * Jsonnet expressions (the constructor only guards against infinity, and no standard Jsonnet
     * operator produces NaN). This is consistent with the comparison operators (OP_<, OP_>, etc.)
     * which already extracted the raw double via case class pattern matching.
     */
    def rawDouble: Double = num
    override def asInt: Int = num.toInt

    def asPositiveInt: Int = {
      if (!num.isWhole || !num.isValidInt) {
        Error.fail("index value is not a valid integer")
      }

      if (num.toInt < 0) {
        Error.fail("index value is not a positive integer, got: " + num.toInt)
      }
      num.toInt
    }

    override def asLong: Long = num.toLong

    def asSafeLong: Long = {
      if (num.isInfinite || num.isNaN) {
        Error.fail("numeric value is not finite")
      }

      if (num < DOUBLE_MIN_SAFE_INTEGER || num > DOUBLE_MAX_SAFE_INTEGER) {
        Error.fail("numeric value outside safe integer range for bitwise operation")
      }
      num.toLong
    }

    override def asDouble: Double = {
      if (num.isNaN) {
        Error.fail("not a number")
      }
      num
    }
    private[sjsonnet] def valTag: Byte = TAG_NUM
  }

  final class Arr(var pos: Position, private var arr: Array[? <: Eval]) extends Literal {
    def prettyName = "array"
    private[sjsonnet] def valTag: Byte = TAG_ARR

    /**
     * Flag indicating this array is a lazy reversed view of the backing array. When true, element
     * access is remapped: `value(i)` returns `arr(length - 1 - i)` instead of `arr(i)`. This avoids
     * allocating and copying a new array for `std.reverse`.
     *
     * Inspired by jrsonnet's ReverseArray approach which uses zero-copy index remapping.
     */
    private[sjsonnet] var _reversed: Boolean = false

    // Lazy concat state. When _concatLeft is non-null, this array is a virtual view
    // over the concatenation of _concatLeft and _concatRight. Inspired by jrsonnet's
    // ExtendedArray which uses O(1) concat for large arrays to avoid copying.
    // The 'arr' field is lazily materialized when bulk access (asLazyArray, etc.) is needed.
    private var _concatLeft: Arr = _
    private var _concatRight: Arr = _
    private var _length: Int = -1

    // Lazy range state. When _isRange is true, this array represents
    // a contiguous integer sequence [from, from+1, ..., from+length-1].
    // Elements are computed on demand via Val.cachedNum, avoiding upfront allocation
    // of the full backing array. Inspired by jrsonnet's RangeArray (arr/spec.rs)
    // which uses O(1) creation for std.range results.
    // Uses a separate boolean flag instead of a sentinel value to avoid collisions
    // with valid range start values (e.g. Int.MinValue).
    private var _isRange: Boolean = false
    private var _rangeFrom: Int = 0

    @inline private def isConcatView: Boolean = _concatLeft ne null
    @inline private def isRange: Boolean = _isRange

    override def asArr: Arr = this

    def length: Int = {
      val l = _length
      if (l >= 0) l
      else {
        val computed =
          if (isConcatView) _concatLeft.length + _concatRight.length
          else arr.length // isRange always has _length pre-set, never reaches here
        _length = computed
        computed
      }
    }

    def value(i: Int): Val = {
      if (isConcatView) {
        val leftLen = _concatLeft.length
        if (i < leftLen) _concatLeft.value(i) else _concatRight.value(i - leftLen)
      } else if (isRange) {
        // For reversed ranges, _rangeFrom is the last element and we count down
        if (_reversed) Val.cachedNum(pos, _rangeFrom - i)
        else Val.cachedNum(pos, _rangeFrom + i)
      } else if (_reversed) {
        arr(arr.length - 1 - i).value
      } else {
        arr(i).value
      }
    }

    /**
     * Return the raw Eval at index i without forcing it. For ConcatViews, follows the tree without
     * materializing the backing array — this preserves reference equality for shared prefix
     * elements, enabling O(1) identity checks in compare/equal.
     */
    def eval(i: Int): Eval = {
      if (isConcatView) {
        val leftLen = _concatLeft.length
        if (i < leftLen) _concatLeft.eval(i) else _concatRight.eval(i - leftLen)
      } else if (isRange) {
        if (_reversed) Val.cachedNum(pos, _rangeFrom - i)
        else Val.cachedNum(pos, _rangeFrom + i)
      } else if (_reversed) {
        arr(arr.length - 1 - i)
      } else {
        arr(i)
      }
    }

    /**
     * If both this and other are ConcatViews sharing the same left array, return the shared prefix
     * length. Otherwise return 0. Used by compare/equal to skip identical prefix elements entirely,
     * turning O(n) comparison into O(right_len) for patterns like
     * `big_array + [x] < big_array + [y]`.
     */
    def sharedConcatPrefixLength(other: Arr): Int = {
      if (isConcatView && other.isConcatView && (_concatLeft eq other._concatLeft))
        _concatLeft.length
      else 0
    }

    /**
     * Returns the backing array in logical order. For concat views, materializes into a flat array.
     * For reversed arrays, creates a new copy with elements in reversed order.
     */
    def asLazyArray: Array[Eval] = {
      if (isConcatView) materialize()
      if (isRange) materializeRange()
      if (_reversed) {
        val len = arr.length
        val result = new Array[Eval](len)
        var i = 0
        while (i < len) {
          result(i) = arr(len - 1 - i).asInstanceOf[Eval]
          i += 1
        }
        result
      } else {
        arr.asInstanceOf[Array[Eval]]
      }
    }

    def asStrictArray: Array[Val] = {
      val len = length
      val result = new Array[Val](len)
      var i = 0
      while (i < len) {
        result(i) = value(i)
        i += 1
      }
      result
    }

    /**
     * Materialize a lazy concat view into a flat array. After this call, `arr` holds the full
     * contents and the concat references are released for GC.
     */
    private def materialize(): Unit = {
      val left = _concatLeft
      val right = _concatRight
      val lArr = left.asLazyArray
      val rArr = right.asLazyArray
      val lLen = lArr.length
      val rLen = rArr.length
      val result = new Array[Eval](lLen + rLen)
      System.arraycopy(lArr, 0, result, 0, lLen)
      System.arraycopy(rArr, 0, result, lLen, rLen)
      arr = result
      _concatLeft = null
      _concatRight = null
    }

    /**
     * Materialize a lazy range view into a flat array. After this call, `arr` holds the full
     * Val.Num elements and the range flag is cleared. Handles both forward and reversed ranges.
     */
    private def materializeRange(): Unit = {
      val len = _length
      val from = _rangeFrom
      val rev = _reversed
      val p = pos
      val result = new Array[Eval](len)
      var i = 0
      while (i < len) {
        result(i) = Val.cachedNum(p, if (rev) from - i else from + i)
        i += 1
      }
      arr = result
      _isRange = false // clear range flag
      _reversed = false // range is now materialized in correct order
    }

    /**
     * Concatenate two arrays. For large left-side arrays where neither operand is already a concat
     * view, creates a lazy ConcatView that defers the copy until bulk access is needed. This is
     * particularly beneficial for patterns like `big_array + [x] < big_array + [y]` where
     * element-wise comparison via value(i) never needs the flattened backing array.
     *
     * Inspired by jrsonnet's ExtendedArray (arr/spec.rs) which uses O(1) concat for arrays above a
     * size threshold.
     */
    def concat(newPos: Position, rhs: Arr): Arr = {
      val leftLen = this.length
      // Use lazy concat when the left side is large enough that avoiding the
      // arraycopy is worthwhile. Limit to depth-1 (neither side is a concat view)
      // to prevent O(depth) access chains and ensure value(i) stays O(1).
      if (leftLen >= Arr.LAZY_CONCAT_THRESHOLD && !this.isConcatView && !rhs.isConcatView) {
        val result = Arr(newPos, Arr.EMPTY_EVAL_ARRAY)
        result._concatLeft = this
        result._concatRight = rhs
        result._length = leftLen + rhs.length
        result
      } else {
        // Eager path: allocate + arraycopy (also used when either side is a concat view
        // to flatten and prevent deep nesting)
        val lArr = this.asLazyArray
        val rArr = rhs.asLazyArray
        val rLen = rArr.length
        val result = new Array[Eval](leftLen + rLen)
        System.arraycopy(lArr, 0, result, 0, leftLen)
        System.arraycopy(rArr, 0, result, leftLen, rLen)
        Arr(newPos, result)
      }
    }

    def iterator: Iterator[Val] = {
      val self = this
      new Iterator[Val] {
        private val len = self.length
        private var i = 0
        def hasNext: Boolean = i < len
        def next(): Val = { val v = self.value(i); i += 1; v }
      }
    }
    def foreach[U](f: Val => U): Unit = {
      val len = length
      var i = 0
      while (i < len) {
        f(value(i))
        i += 1
      }
    }
    def forall(f: Val => Boolean): Boolean = {
      val len = length
      var i = 0
      while (i < len) {
        if (!f(value(i))) return false
        i += 1
      }
      true
    }

    /**
     * Create a reversed view of this array without copying. The returned Arr shares the same
     * backing array but flips the reversed flag, so element access is O(1) with zero allocation.
     * Double-reversal cancels out.
     */
    def reversed(newPos: Position): Arr = {
      if (isRange) {
        // Double-reverse of a range cancels out: return a forward range with original start
        if (_reversed) {
          // Currently reversed: _rangeFrom is the high end, counting down.
          // Reversing again restores the original forward range.
          val originalFrom = _rangeFrom - _length + 1
          val result = new Arr(newPos, null)
          result._isRange = true
          result._rangeFrom = originalFrom
          result._length = _length
          // _reversed defaults to false — forward range
          result
        } else {
          // Forward range: reverse to [from+len-1, from+len-2, ..., from]
          val len = _length
          val newFrom = _rangeFrom + len - 1
          val result = new Arr(newPos, null)
          result._isRange = true
          result._rangeFrom = newFrom
          result._length = len
          result._reversed = true // signal to compute from-i instead of from+i
          result
        }
      } else {
        if (isConcatView) materialize() // flatten before reverse
        val result = Arr(newPos, arr)
        result._reversed = !this._reversed
        result
      }
    }
  }

  object Arr {
    def apply(pos: Position, arr: Array[? <: Eval]): Arr = new Arr(pos, arr)

    /**
     * Create a lazy range array representing the integer sequence [from, from+1, ..., from+size-1].
     * Elements are computed on demand via Val.cachedNum, avoiding upfront allocation of the full
     * backing array. This turns `std.range(1, 1000000)` from O(n) to O(1) creation.
     *
     * Inspired by jrsonnet's RangeArray (arr/spec.rs) which uses the same deferred approach.
     */
    def range(pos: Position, from: Int, size: Int): Arr = {
      val a = new Arr(pos, null)
      a._isRange = true
      a._rangeFrom = from
      a._length = size
      a
    }

    /**
     * Threshold for lazy concat. Arrays with left.length >= this value use a virtual ConcatView
     * instead of eager arraycopy. Below this size, arraycopy is cheap enough that the indirection
     * overhead of virtual dispatch outweighs the copy savings.
     */
    val LAZY_CONCAT_THRESHOLD = 256
    private[sjsonnet] val EMPTY_EVAL_ARRAY: Array[Eval] = Array.empty[Eval]
  }

  object Obj {

    /** Package-private ref to DebugStats, set by Evaluator when --debug-stats is active. */
    private[sjsonnet] var currentDebugStats: DebugStats = _

    /**
     * @param add
     *   whether this field was defined the "+:", "+::" or "+:::" separators, corresponding to the
     *   "nested field inheritance" language feature; see
     *   https://jsonnet.org/ref/language.html#nested-field-inheritance
     */
    abstract class Member(
        val add: Boolean,
        val visibility: Visibility,
        val cached: Boolean = true,
        val deprecatedSkipAsserts: Boolean = false) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val
    }

    class ConstMember(add2: Boolean, visibility2: Visibility, v: Val, cached2: Boolean = true)
        extends Member(add2, visibility2, cached2, deprecatedSkipAsserts = true) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val = v
    }

    def mk(pos: Position, members: (String, Obj.Member)*): Obj = {
      val m = Util.preSizedJavaLinkedHashMap[String, Obj.Member](members.length)
      for ((k, v) <- members) m.put(k, v)
      new Obj(pos, m, false, null, null)
    }

    def mk(pos: Position, sizeHint: Int, membersArray: Iterable[(String, Obj.Member)]*): Obj = {
      val m = Util.preSizedJavaLinkedHashMap[String, Obj.Member](sizeHint)
      for (members <- membersArray; (k, v) <- members) {
        m.put(k, v)
      }
      new Obj(pos, m, false, null, null)
    }

    def mk(pos: Position, members: Array[(String, Obj.Member)]): Obj = {
      val m = Util.preSizedJavaLinkedHashMap[String, Obj.Member](members.length)
      var i = 0
      while (i < members.length) {
        val e = members(i)
        m.put(e._1, e._2)
        i += 1
      }
      new Obj(pos, m, false, null, null)
    }
  }

  /**
   * Represents json/jsonnet objects.
   *
   * Obj implements special optimizations for "static objects", which are objects without `super`
   * where all fields are constant and have default visibility. Static objects can be created during
   * parsing or in [[StaticOptimizer]].
   *
   * @param value0
   *   maps fields to their Member definitions. This is initially null for static objects and is
   *   non-null for non-static objects.
   * @param static
   *   true if this object is static, false otherwise.
   * @param triggerAsserts
   *   callback to evaluate assertions defined in the object. Parameters are (self, super).
   * @param `super`
   *   the super object, or null if there is no super object.
   * @param valueCache
   *   a cache for computed values. For static objects, this is pre-populated with all fields. For
   *   non-static objects, this is lazily populated as fields are accessed.
   * @param allKeys
   *   a map of all keys in the object (including keys inherited from `super`), where the boolean
   *   value is true if the key is hidden and false otherwise. For static objects, this is
   *   pre-populated and the mapping may be interned and shared across instances. For non-static
   *   objects, it is dynamically computed only if the object has a `super`
   */
  final class Obj(
      var pos: Position,
      private var value0: util.LinkedHashMap[String, Obj.Member],
      private val static: Boolean,
      private val triggerAsserts: (Val.Obj, Val.Obj) => Unit,
      `super`: Obj,
      private var valueCache: util.HashMap[Any, Val] = null,
      private var allKeys: util.LinkedHashMap[String, java.lang.Boolean] = null,
      private val excludedKeys: java.util.Set[String] = null,
      private val singleFieldKey: String = null,
      private val singleFieldMember: Obj.Member = null,
      private val inlineFieldKeys: Array[String] = null,
      private val inlineFieldMembers: Array[Obj.Member] = null)
      extends Literal
      with Expr.ObjBody {
    private[sjsonnet] def valTag: Byte = TAG_OBJ
    private var asserting: Boolean = false
    // Pre-computed flag: true if this object or any super has assert statements.
    // Computed O(1) at construction from the super chain (super is already constructed).
    // Allows skipping the triggerAllAsserts super-chain walk for assert-free objects.
    private val hasAnyAsserts: Boolean =
      triggerAsserts != null || (`super` != null && `super`.hasAnyAsserts)

    // Inline value cache: avoids HashMap allocation for objects with ≤2 cached fields.
    // For bench.02 (object fibonacci), this eliminates ~242K HashMap allocations (~37MB).
    private var ck1: Any = null
    private var cv1: Val = null
    private var ck2: Any = null
    private var cv2: Val = null

    /** Store a computed value in the inline cache or overflow HashMap. */
    private def putCache(key: Any, v: Val): Unit = {
      if (ck1 == null) { ck1 = key; cv1 = v }
      else if (ck2 == null) { ck2 = key; cv2 = v }
      else {
        if (valueCache == null) {
          val ds = Obj.currentDebugStats
          if (ds != null) ds.valueCacheOverflows += 1
          valueCache = new util.HashMap[Any, Val]()
        }
        valueCache.put(key, v)
      }
    }

    def getSuper: Obj = `super`

    /**
     * True if this object can be iterated directly via inline field arrays, bypassing the value()
     * lookup chain (cache checks, valueRaw dispatch, key scan). Only safe when: no super chain, no
     * excluded keys, and inline storage is present.
     */
    @inline private[sjsonnet] def canDirectIterate: Boolean =
      `super` == null && excludedKeys == null && (singleFieldKey != null || inlineFieldKeys != null)

    /** Raw inline field keys array (may be null for single-field objects). */
    @inline private[sjsonnet] def inlineKeys: Array[String] = inlineFieldKeys

    /** Raw inline field members array (may be null for single-field objects). */
    @inline private[sjsonnet] def inlineMembers: Array[Obj.Member] = inlineFieldMembers

    /** Single-field key (null if object has 0 or 2+ fields). */
    @inline private[sjsonnet] def singleKey: String = singleFieldKey

    /** Single-field member (null if object has 0 or 2+ fields). */
    @inline private[sjsonnet] def singleMem: Obj.Member = singleFieldMember

    /**
     * Cached sorted field order for inline objects. Shared across all objects from the same
     * MemberList to avoid per-object sort + allocation.
     */
    @volatile private[sjsonnet] var _sortedInlineOrder: Array[Int] = null

    /**
     * When true, field caching can be skipped during materialization because no field body
     * references `self` or `super`. This eliminates HashMap allocation overhead for objects with >2
     * fields (where the 2-slot inline cache overflows).
     */
    private[sjsonnet] var _skipFieldCache: Boolean = false

    /**
     * Reference to the source MemberList expression, set for objects with super == null. Enables
     * lazy sharing of allKeyNames/visibleKeyNames: the first access computes and caches on the
     * MemberList; subsequent objects from the same expression find the cached result.
     */
    private[sjsonnet] var _sourceMemberList: Expr.ObjBody.MemberList = null

    /**
     * Store a computed field value in the object's inline cache, preserving memoization semantics
     * when bypassing `value()` during direct iteration. This ensures that subsequent accesses via
     * `self.field` within sibling field computations see the cached value, preventing double
     * evaluation and duplicate side effects (e.g., `std.trace`).
     */
    @inline private[sjsonnet] def cacheFieldValue(key: String, v: Val): Unit = putCache(key, v)

    private def getValue0: util.LinkedHashMap[String, Obj.Member] = {
      if (value0 == null) {
        if (singleFieldKey != null) {
          // Single-field object: lazily construct LinkedHashMap from inline storage
          val m = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](1)
          m.put(singleFieldKey, singleFieldMember)
          this.value0 = m
        } else if (inlineFieldKeys != null) {
          // Multi-field inline object: lazily construct LinkedHashMap from arrays
          val keys = inlineFieldKeys
          val members = inlineFieldMembers
          val n = keys.length
          val m = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](n)
          var i = 0
          while (i < n) {
            m.put(keys(i), members(i))
            i += 1
          }
          this.value0 = m
        } else {
          // value0 is always defined for non-static objects, so if we're computing it here
          // then that implies that the object is static and therefore valueCache should be
          // pre-populated and all members should be visible and constant.
          val value0 = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](allKeys.size())
          allKeys.forEach { (k, _) =>
            value0.put(k, new Val.Obj.ConstMember(false, Visibility.Normal, valueCache.get(k)))
          }
          // Only assign to field after initialization is complete to allow unsynchronized multi-threaded use:
          this.value0 = value0
        }
      }
      value0
    }

    def triggerAllAsserts(brokenAssertionLogic: Boolean): Unit = {
      // Short-circuit: no asserts in this object or any super
      if (hasAnyAsserts && !asserting) {
        asserting = true
        triggerAllAsserts(this, `super`, brokenAssertionLogic)
      }
    }

    // As we walk up the superclass hierarchy, the `self` binding is unchanged
    // but `super` climbs up the hierarchy as well.
    @tailrec private def triggerAllAsserts(
        obj: Val.Obj,
        sup: Val.Obj,
        brokenAssertionLogic: Boolean): Unit = {
      if (triggerAsserts != null) triggerAsserts(obj, sup)
      if ((!brokenAssertionLogic || triggerAsserts == null) && sup != null)
        sup.triggerAllAsserts(obj, sup.getSuper, brokenAssertionLogic)
    }

    def addSuper(pos: Position, lhs: Val.Obj): Val.Obj = {
      val ds = Obj.currentDebugStats
      if (ds != null) ds.addSuperCalls += 1
      // Fast path: no super chain — avoid ArrayBuilder + Array allocation.
      // Invariant: excludedKeys != null implies getSuper != null (removeKeys always sets super),
      // so when getSuper == null, excludedKeys is always null and the re-introduction logic
      // in the slow path is not needed.
      if (getSuper == null) {
        assert(excludedKeys == null, "excludedKeys should be null when getSuper is null")
        return new Val.Obj(
          this.pos,
          this.getValue0,
          false,
          this.triggerAsserts,
          lhs,
          null, // Inline cache handles ≤2 fields; overflow HashMap allocated lazily
          null,
          null
        )
      }
      if (ds != null) ds.addSuperChainWalks += 1
      // Single traversal: collect chain in this-first order
      val builder = new mutable.ArrayBuilder.ofRef[Val.Obj]
      var current = this
      while (current != null) {
        builder += current
        current = current.getSuper
      }
      val chain = builder.result()
      if (ds != null && chain.length > ds.maxSuperChainDepth)
        ds.maxSuperChainDepth = chain.length

      // Pre-collect all keys defined in this chain once (only needed if any obj has excludedKeys)
      lazy val keysInThisChain: java.util.Set[String] = {
        val set = Util.preSizedJavaHashSet[String](chain.length * 4)
        for (s <- chain) set.addAll(s.getValue0.keySet())
        set
      }

      // Iterate root-first (reverse of collection order) to build the new super chain
      current = lhs
      var i = chain.length - 1
      while (i >= 0) {
        val s = chain(i)
        var members = s.getValue0
        var filteredExcludedKeys = if (s.excludedKeys != null) {
          Util.intersect(s.excludedKeys, keysInThisChain)
        } else null

        // If this object has excluded keys that the LHS provides as visible,
        // re-introduce them with synthetic members that delegate to the LHS.
        // This ensures `lhs + removeKey(rhs, k)` preserves lhs's visible key `k`.
        if (filteredExcludedKeys != null) {
          val iter = filteredExcludedKeys.iterator()
          while (iter.hasNext) {
            val key = iter.next()
            if (lhs.containsVisibleKey(key)) {
              if (members eq s.getValue0) {
                members = new util.LinkedHashMap[String, Obj.Member](s.getValue0)
              }
              val capturedKey = key
              members.put(
                key,
                new Obj.Member(false, Visibility.Normal, deprecatedSkipAsserts = true) {
                  def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
                    lhs.value(capturedKey, pos, self)(ev)
                }
              )
              iter.remove()
            }
          }
          if (filteredExcludedKeys.isEmpty) filteredExcludedKeys = null
        }

        current = new Val.Obj(
          s.pos,
          members,
          false,
          s.triggerAsserts,
          current,
          null, // Inline cache handles ≤2 fields; overflow HashMap allocated lazily
          null,
          filteredExcludedKeys
        )
        i -= 1
      }
      current
    }

    /**
     * Create a new object that removes the specified keys from this object.
     *
     * The implementation preserves both internal and external inheritance:
     *   1. Internal: For `objectRemoveKey({ a: 1 } + { b: super.a }, 'a')`, the original object's
     *      internal super chain is preserved, so `b: super.a` can still access `a`.
     *   2. External: For `{ a: 1 } + objectRemoveKey({ b: super.a }, 'a')`, the result can
     *      participate in a new inheritance chain, where `super.a` accesses the new super.
     *
     * The approach is to create a thin wrapper object with the original object as super, and mark
     * the key as excluded via the excludedKeys set. The excluded key won't appear in
     * allKeyNames/visibleKeyNames, but super.key can still access the value.
     */
    @nowarn("cat=deprecation")
    def removeKeys(pos: Position, keys: String*): Val.Obj = {
      val excluded =
        if (keys.length == 1)
          java.util.Collections.singleton(keys.head)
        else {
          import scala.collection.JavaConverters._
          new util.HashSet[String](keys.asJavaCollection)
        }

      new Val.Obj(
        pos,
        Util.emptyJavaLinkedHashMap[String, Obj.Member],
        false,
        null, // No asserts in wrapper; original object's asserts are triggered via super chain
        this,
        null, // Inline cache handles ≤2 fields; overflow HashMap allocated lazily
        null,
        excluded
      )
    }

    def prettyName = "object"
    override def asObj: Val.Obj = this

    private def gatherKeys(mapping: util.LinkedHashMap[String, java.lang.Boolean]): Unit = {
      // Fast path: no super chain — just copy this object's keys directly
      if (this.getSuper == null) {
        gatherKeysForSingle(this, null, mapping)
        return
      }

      // Single traversal: collect chain in this-first order using ArrayBuilder
      val builder = new mutable.ArrayBuilder.ofRef[Val.Obj]
      var current = this
      while (current != null) {
        builder += current
        current = current.getSuper
      }
      val chain = builder.result()
      val chainLength = chain.length

      // Iterate root-first (reverse of collection order) and populate the mapping.
      // Each object's excludedKeys removes keys gathered from lower levels (closer to root),
      // but objects above it in the chain can re-introduce those keys via their own members.
      var i = chainLength - 1
      while (i >= 0) {
        val s = chain(i)
        if (Util.isNotEmpty(s.excludedKeys)) {
          val iter = s.excludedKeys.iterator()
          while (iter.hasNext) mapping.remove(iter.next())
        }
        gatherKeysForSingle(s, null, mapping)
        i -= 1
      }
    }

    /** Gather keys from a single object into the mapping, filtering by exclusions. */
    private def gatherKeysForSingle(
        obj: Val.Obj,
        exclusionSet: java.util.Set[String],
        mapping: util.LinkedHashMap[String, java.lang.Boolean]): Unit = {
      if (obj.static) {
        obj.allKeys
          .keySet()
          .forEach(key => {
            if (exclusionSet == null || !exclusionSet.contains(key)) {
              mapping.put(key, false)
            }
          })
      } else {
        obj.getValue0.forEach { (k, m) =>
          if (exclusionSet == null || !exclusionSet.contains(k)) {
            val vis = m.visibility
            if (!mapping.containsKey(k)) mapping.put(k, vis == Visibility.Hidden)
            else if (vis == Visibility.Hidden) mapping.put(k, true)
            else if (vis == Visibility.Unhide) mapping.put(k, false)
          }
        }
      }
    }

    private def getAllKeys = {
      if (allKeys == null) {
        val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
        gatherKeys(allKeys)
        // Only assign to field after initialization is complete to allow unsynchronized multi-threaded use:
        this.allKeys = allKeys
      }
      allKeys
    }

    @inline def hasKeys: Boolean = {
      if (singleFieldKey != null) true
      else if (inlineFieldKeys != null && `super` == null) inlineFieldKeys.length > 0
      else {
        val m = if (static || `super` != null) getAllKeys else getValue0
        !m.isEmpty
      }
    }

    @inline def containsKey(k: String): Boolean = {
      if (singleFieldKey != null && `super` == null) singleFieldKey.equals(k)
      else if (inlineFieldKeys != null && `super` == null) {
        val keys = inlineFieldKeys
        val n = keys.length
        var i = 0
        while (i < n) {
          if (keys(i).equals(k)) return true
          i += 1
        }
        false
      } else {
        val m = if (static || `super` != null) getAllKeys else getValue0
        m.containsKey(k)
      }
    }

    @inline def containsVisibleKey(k: String): Boolean = {
      if (static || `super` != null) {
        getAllKeys.get(k) == java.lang.Boolean.FALSE
      } else if (inlineFieldKeys != null) {
        val keys = inlineFieldKeys
        val members = inlineFieldMembers
        val n = keys.length
        var i = 0
        while (i < n) {
          if (keys(i).equals(k)) return members(i).visibility != Visibility.Hidden
          i += 1
        }
        false
      } else {
        val m = getValue0.get(k)
        m != null && (m.visibility != Visibility.Hidden)
      }
    }

    lazy val allKeyNames: Array[String] = {
      val ml = _sourceMemberList
      val cached = if (ml != null) ml._cachedAllKeyNames else null
      if (cached != null) cached
      else {
        val result =
          if (inlineFieldKeys != null && `super` == null) inlineFieldKeys.clone()
          else {
            val m = if (static || `super` != null) getAllKeys else getValue0
            m.keySet().toArray(new Array[String](m.size()))
          }
        if (ml != null) ml._cachedAllKeyNames = result
        result
      }
    }

    lazy val visibleKeyNames: Array[String] = {
      val ml = _sourceMemberList
      val cached = if (ml != null) ml._cachedVisibleKeyNames else null
      if (cached != null) cached
      else {
        val result = if (static) {
          allKeyNames
        } else if (inlineFieldKeys != null && `super` == null) {
          // Inline multi-field fast path: check if all visible (common case)
          val keys = inlineFieldKeys
          val members = inlineFieldMembers
          val n = keys.length
          var allVisible = true
          var i = 0
          while (allVisible && i < n) {
            if (members(i).visibility == Visibility.Hidden) allVisible = false
            i += 1
          }
          if (allVisible) keys.clone()
          else {
            val buf = new mutable.ArrayBuilder.ofRef[String]
            buf.sizeHint(n)
            var j = 0
            while (j < n) {
              if (members(j).visibility != Visibility.Hidden) buf += keys(j)
              j += 1
            }
            buf.result()
          }
        } else {
          val buf = new mutable.ArrayBuilder.ofRef[String]
          if (`super` == null) {
            val v0 = getValue0
            // This size hint is based on an optimistic assumption that most fields are visible,
            // avoiding re-sizing or trimming the buffer in the common case:
            buf.sizeHint(v0.size())
            v0.forEach((k, m) => if (m.visibility != Visibility.Hidden) buf += k)
          } else {
            val iter = getAllKeys.entrySet().iterator()
            while (iter.hasNext()) {
              val e = iter.next()
              if (e.getValue() == java.lang.Boolean.FALSE) buf += e.getKey()
            }
          }
          buf.result()
        }
        if (ml != null) ml._cachedVisibleKeyNames = result
        result
      }
    }

    def value(k: String, pos: Position, self: Obj = this)(implicit evaluator: EvalScope): Val = {
      if (static) {
        valueCache.get(k) match {
          case null => Error.fail("Field does not exist: " + k, pos)
          case x    => x
        }
      } else {
        if ((self eq this) && excludedKeys != null && excludedKeys.contains(k)) {
          Error.fail("Field does not exist: " + k, pos)
        }
        val cacheKey: Any = if (self eq this) k else (k, self)
        if (ck1 != null && ck1 == cacheKey) {
          return cv1
        }
        if (ck2 != null && ck2 == cacheKey) {
          return cv2
        }
        val cachedValue = if (valueCache != null) valueCache.get(cacheKey) else null
        if (cachedValue != null) {
          cachedValue
        } else {
          valueRaw(k, self, pos, this, cacheKey) match {
            case null => Error.fail("Field does not exist: " + k, pos)
            case x    => x
          }
        }
      }
    }

    private def renderString(v: Val)(implicit evaluator: EvalScope): String =
      evaluator.materialize(v).transform(new Renderer()).toString

    /**
     * Merge two values for "nested field inheritance"; see
     * https://jsonnet.org/ref/language.html#nested-field-inheritance for background.
     */
    private def mergeMember(l: Val, r: Val, pos: Position)(implicit evaluator: EvalScope): Literal =
      (l, r) match {
        case (lStr: Val.Str, rStr: Val.Str) =>
          Val.Str.concat(pos, lStr, rStr)
        case (lStr: Val.Str, _) =>
          Val.Str.concat(pos, lStr, Val.Str(pos, renderString(r)))
        case (_, rStr: Val.Str) =>
          Val.Str.concat(pos, Val.Str(pos, renderString(l)), rStr)
        case (lNum: Val.Num, rNum: Val.Num) =>
          Val.Num(pos, lNum.asDouble + rNum.asDouble)
        case (lArr: Val.Arr, rArr: Val.Arr) =>
          lArr.concat(pos, rArr)
        case (lObj: Val.Obj, rObj: Val.Obj) =>
          rObj.addSuper(pos, lObj)
        case (_: Val.Null, _) =>
          Error.fail("Cannot merge null with " + r.prettyName, pos)
        case (_, _: Val.Null) =>
          Error.fail("Cannot merge " + l.prettyName + " with null", pos)
        case _ =>
          throw new MatchError((l, r))
      }

    def valueRaw(k: String, self: Obj, pos: Position, cacheOwner: Obj = null, cacheKey: Any = null)(
        implicit evaluator: EvalScope): Val = {
      if (static) {
        val v = valueCache.get(k)
        if (cacheOwner != null && v != null) cacheOwner.putCache(cacheKey, v)
        v
      } else {
        val s = this.`super`
        val sfk = singleFieldKey
        if (sfk != null) {
          // Single-field fast path: avoid LinkedHashMap lookup
          if (sfk.equals(k)) {
            val m = singleFieldMember
            if (!evaluator.settings.brokenAssertionLogic || !m.deprecatedSkipAsserts) {
              self.triggerAllAsserts(evaluator.settings.brokenAssertionLogic)
            }
            val vv = m.invoke(self, s, pos.fileScope, evaluator)
            val v = if (s != null && m.add) {
              s.valueRaw(k, self, pos, null, null) match {
                case null     => vv
                case supValue => mergeMember(supValue, vv, pos)
              }
            } else vv
            if (cacheOwner != null && m.cached) cacheOwner.putCache(cacheKey, v)
            v
          } else {
            if (s == null) null else s.valueRaw(k, self, pos, cacheOwner, cacheKey)
          }
        } else if (inlineFieldKeys != null) {
          // Inline multi-field fast path: linear scan over small arrays
          val keys = inlineFieldKeys
          val members = inlineFieldMembers
          val n = keys.length
          var i = 0
          while (i < n) {
            if (keys(i).equals(k)) {
              val m = members(i)
              if (!evaluator.settings.brokenAssertionLogic || !m.deprecatedSkipAsserts) {
                self.triggerAllAsserts(evaluator.settings.brokenAssertionLogic)
              }
              val vv = m.invoke(self, s, pos.fileScope, evaluator)
              val v = if (s != null && m.add) {
                s.valueRaw(k, self, pos, null, null) match {
                  case null     => vv
                  case supValue => mergeMember(supValue, vv, pos)
                }
              } else vv
              if (cacheOwner != null && m.cached) cacheOwner.putCache(cacheKey, v)
              return v
            }
            i += 1
          }
          if (s == null) null else s.valueRaw(k, self, pos, cacheOwner, cacheKey)
        } else {
          getValue0.get(k) match {
            case null =>
              if (s == null) null else s.valueRaw(k, self, pos, cacheOwner, cacheKey)
            case m =>
              if (!evaluator.settings.brokenAssertionLogic || !m.deprecatedSkipAsserts) {
                self.triggerAllAsserts(evaluator.settings.brokenAssertionLogic)
              }
              val vv = m.invoke(self, s, pos.fileScope, evaluator)
              val v = if (s != null && m.add) {
                s.valueRaw(k, self, pos, null, null) match {
                  case null     => vv
                  case supValue => mergeMember(supValue, vv, pos)
                }
              } else vv
              if (cacheOwner != null && m.cached) cacheOwner.putCache(cacheKey, v)
              v
          }
        }
      }
    }

    def foreachElement(sort: Boolean, pos: Position)(f: (String, Val) => Unit)(implicit
        ev: EvalScope): Unit = {
      val keys = if (sort) visibleKeyNames.sorted(Util.CodepointStringOrdering) else visibleKeyNames
      for (k <- keys) {
        val v = value(k, pos)
        f(k, v)
      }
    }
  }

  final class StaticObjectFieldSet(protected val keys: Array[String]) {

    override def hashCode(): Int = {
      util.Arrays.hashCode(keys.asInstanceOf[Array[Object]])
    }

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case that: StaticObjectFieldSet =>
          keys.sameElements(that.keys)
        case _ => false
      }
    }
  }

  def staticObject(
      pos: Position,
      fields: Array[Expr.Member.Field],
      internedKeyMaps: mutable.HashMap[
        StaticObjectFieldSet,
        java.util.LinkedHashMap[String, java.lang.Boolean]
      ],
      internedStrings: mutable.HashMap[String, String]): Obj = {
    val cache = Util.preSizedJavaHashMap[Any, Val](fields.length)
    val allKeys = Util.preSizedJavaLinkedHashMap[String, java.lang.Boolean](fields.length)
    val keys = new Array[String](fields.length)
    var idx = 0
    fields.foreach {
      case Expr.Member.Field(_, Expr.FieldName.Fixed(k), _, _, _, rhs: Val.Literal) =>
        val uniqueKey = internedStrings.getOrElseUpdate(k, k)
        cache.put(uniqueKey, rhs)
        allKeys.put(uniqueKey, false)
        keys(idx) = uniqueKey
        idx += 1
      case other =>
        throw new Error(
          s"Unexpected non-literal field in static object: $other of class ${other.getClass}"
        )
    }
    val fieldSet = new StaticObjectFieldSet(keys)
    new Val.Obj(
      pos,
      null,
      true,
      null,
      null,
      cache,
      internedKeyMaps.getOrElseUpdate(fieldSet, allKeys)
    )
  }

  abstract class Func(var pos: Position, val defSiteValScope: ValScope, val params: Params)
      extends Val
      with Expr {
    final override private[sjsonnet] def tag = ExprTags.`Val.Func`
    private[sjsonnet] def valTag: Byte = TAG_FUNC

    def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val

    /**
     * Override to expose the function's body AST for pattern detection (e.g. foldl string concat,
     * constant-body makeArray optimization). Returns null by default.
     */
    def bodyExpr: Expr = null

    // Convenience wrapper: evaluates the function body and resolves any TailCall sentinel.
    // Use this instead of raw `evalRhs` at call sites that bypass `apply*` and consume
    // the result directly (e.g. stdlib scope-reuse fast paths).
    final def evalRhsResolved(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val =
      TailCall.resolve(evalRhs(scope, ev, fs, pos))(ev)

    def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = null

    /** Override to provide a function name for error messages. Only called on error paths. */
    def functionName: String = null

    /**
     * Extension point: override to customize the prefix in error messages (e.g. "mylib." instead of
     * "std.").
     */
    def qualifiedName: String = s"std.$functionName"

    private def functionNameSuffix: String = {
      val name = functionName
      if (name != null) s" in function $name" else ""
    }

    def prettyName = "function"

    override def exprErrorString: String = "Function"

    override def asFunc: Func = this

    /**
     * Core function application with tail call optimization (TCO) support.
     *
     * TCO protocol: when `tailstrictMode == TailstrictModeEnabled`, `evalRhs` may return a
     * [[TailCall]] sentinel which is propagated back to the caller's [[TailCall.resolve]] loop
     * without resolution. When `tailstrictMode == TailstrictModeDisabled` (the common case — called
     * from std library, object fields, etc.), any TailCall is resolved here via `TailCall.resolve`
     * to prevent sentinel leakage.
     */
    def apply(argsL: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      val simple = namedNames == null && params.names.length == argsL.length
      val funDefFileScope: FileScope = pos match {
        case null => outerPos.fileScope
        case p    => p.fileScope
      }
      if (simple) {
        if (tailstrictMode == TailstrictModeEnabled) {
          argsL.foreach(_.value)
        }
        val newScope = defSiteValScope.extendSimple(argsL)
        val result = evalRhs(newScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      } else {
        val newScopeLen = math.max(params.names.length, argsL.length)
        // Initialize positional args
        val base = defSiteValScope.length
        val newScope = defSiteValScope.extendBy(newScopeLen)
        val argVals = newScope.bindings
        val posArgs = if (namedNames == null) argsL.length else argsL.length - namedNames.length
        System.arraycopy(argsL, 0, argVals, base, posArgs)
        if (namedNames != null) { // Add named args
          var i = 0
          var j = posArgs
          while (i < namedNames.length) {
            val idx = params.paramMap.getOrElse(
              namedNames(i),
              Error.fail(
                s"has no parameter ${namedNames(i)}",
                outerPos
              )
            )
            if (argVals(base + idx) != null)
              Error.fail(
                s"binding parameter a second time: ${namedNames(i)}$functionNameSuffix",
                outerPos
              )
            argVals(base + idx) = argsL(j)
            i += 1
            j += 1
          }
        }
        if (argsL.length > params.names.length)
          Error.fail(
            "Too many args, has " + params.names.length + " parameter(s)",
            outerPos
          )
        if (params.names.length != argsL.length) { // Args missing -> add defaults
          var missing: ArrayBuffer[String] = null
          var i = posArgs
          var j = base + posArgs
          while (j < argVals.length) {
            if (argVals(j) == null) {
              val default = params.defaultExprs(i)
              if (default != null) {
                argVals(j) = new LazyDefault(default, newScope, this, ev)
              } else {
                if (missing == null) missing = new ArrayBuffer
                missing.+=(params.names(i))
              }
            }
            i += 1
            j += 1
          }
          if (missing != null) {
            val plural = if (missing.size > 1) "s" else ""
            Error.fail(
              s"parameter$plural ${missing.mkString(", ")} not bound in call",
              outerPos
            )
          }
        }
        if (tailstrictMode == TailstrictModeEnabled) {
          argVals.foreach(_.value)
        }
        val result = evalRhs(newScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      }
    }

    // apply0–apply3: fast paths for the most common call arities, called from
    // Evaluator.visitApply0–visitApply3. When the arity matches exactly and there are
    // no named/default arguments, these skip the general-purpose scope-extension logic
    // in `apply` (named-arg mapping, defaults filling, arraycopy) and use the cheaper
    // `ValScope.extendSimple` instead.
    def apply0(outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = {
      if (params.names.length != 0) apply(Evaluator.emptyLazyArray, null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match {
          case null => outerPos.fileScope
          case p    => p.fileScope
        }
        val result = evalRhs(defSiteValScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      }
    }

    def apply1(argVal: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      if (params.names.length != 1) apply(Array(argVal), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match {
          case null => outerPos.fileScope
          case p    => p.fileScope
        }
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal.value
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal)
        val result = evalRhs(newScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      }
    }

    def apply2(argVal1: Eval, argVal2: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      if (params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match {
          case null => outerPos.fileScope
          case p    => p.fileScope
        }
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal1.value
          argVal2.value
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2)
        val result = evalRhs(newScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      }
    }

    def apply3(argVal1: Eval, argVal2: Eval, argVal3: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      if (params.names.length != 3) apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match {
          case null => outerPos.fileScope
          case p    => p.fileScope
        }
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal1.value
          argVal2.value
          argVal3.value
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2, argVal3)
        val result = evalRhs(newScope, ev, funDefFileScope, outerPos)
        if (tailstrictMode == TailstrictModeDisabled) TailCall.resolve(result) else result
      }
    }
  }

  /**
   * Superclass for standard library functions.
   *
   * TCO note: the arity-specialized overrides (`apply1`–`apply3`) intentionally omit the
   * `TailCall.resolve` guard present in [[Func.apply1]]–[[Func.apply3]]. This is safe because
   * built-in `evalRhs` implementations are concrete Scala code that never produce [[TailCall]]
   * sentinels directly. When a built-in internally invokes a user-defined callback (e.g.
   * `std.makeArray`, `std.sort`), it passes `TailstrictModeDisabled` explicitly, so the callback's
   * own `Val.Func.apply*` resolves any TailCall before returning.
   */
  abstract class Builtin(
      override val functionName: String,
      paramNames: Array[String],
      defaults: Array[Expr] = null)
      extends Func(
        null,
        ValScope.empty,
        Params(paramNames, if (defaults == null) new Array[Expr](paramNames.length) else defaults)
      ) {

    override final def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val =
      expr.asInstanceOf[Val]

    override final def evalRhs(
        scope: ValScope,
        ev: EvalScope,
        fs: FileScope,
        pos: Position): Val = {
      evalRhs(scope.bindings, ev, pos)
    }

    def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val

    // No TailCall.resolve needed: Builtin evalRhs is pure Scala and never produces TailCall.
    // When builtins invoke user callbacks internally, they pass TailstrictModeDisabled,
    // so the callback's own Func.apply* resolves any TailCall before returning.
    override def apply1(argVal: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (params.names.length != 1) apply(Array(argVal), null, outerPos)
      else {
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal.value
        }
        evalRhs(Array(argVal), ev, outerPos)
      }

    override def apply2(argVal1: Eval, argVal2: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else {
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal1.value
          argVal2.value
        }
        evalRhs(Array(argVal1, argVal2), ev, outerPos)
      }

    override def apply3(argVal1: Eval, argVal2: Eval, argVal3: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (params.names.length != 3) apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      else {
        if (tailstrictMode == TailstrictModeEnabled) {
          argVal1.value
          argVal2.value
          argVal3.value
        }
        evalRhs(Array(argVal1, argVal2, argVal3), ev, outerPos)
      }

    /**
     * Specialize a call to this function in the optimizer. Must return either `null` to leave the
     * call-site as it is or a pair of a (possibly different) `Builtin` and the arguments to pass to
     * it (usually a subset of the supplied arguments).
     * @param args
     *   the positional arguments for this function call. Named arguments and defaults have already
     *   been resolved.
     */
    def specialize(args: Array[Expr], tailstrict: Boolean): (Builtin, Array[Expr]) = null

    /** Is this builtin safe to use in static evaluation */
    def staticSafe: Boolean = true
  }

  abstract class Builtin0(fn: String, def1: Expr = null)
      extends Builtin(fn: String, Array.empty, if (def1 == null) null else Array(def1)) {
    final def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
      evalRhs(ev, pos)

    def evalRhs(ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (namedNames == null && argVals.length == 0)
        evalRhs(ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin1(fn: String, pn1: String, def1: Expr = null)
      extends Builtin(fn: String, Array(pn1), if (def1 == null) null else Array(def1)) {
    final def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).value, ev, pos)

    def evalRhs(arg1: Eval, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (namedNames == null && argVals.length == 1) evalRhs(argVals(0).value, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin2(fn: String, pn1: String, pn2: String, defs: Array[Expr] = null)
      extends Builtin(fn: String, Array(pn1, pn2), defs) {
    final def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).value, args(1).value, ev, pos)

    def evalRhs(arg1: Eval, arg2: Eval, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (namedNames == null && argVals.length == 2)
        evalRhs(argVals(0).value, argVals(1).value, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)

    override def apply2(argVal1: Eval, argVal2: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (params.names.length == 2) evalRhs(argVal1.value, argVal2.value, ev, outerPos)
      else super.apply(Array(argVal1, argVal2), null, outerPos)
  }

  abstract class Builtin3(
      fn: String,
      pn1: String,
      pn2: String,
      pn3: String,
      defs: Array[Expr] = null)
      extends Builtin(fn: String, Array(pn1, pn2, pn3), defs) {
    final def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).value, args(1).value, args(2).value, ev, pos)

    def evalRhs(arg1: Eval, arg2: Eval, arg3: Eval, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (namedNames == null && argVals.length == 3)
        evalRhs(argVals(0).value, argVals(1).value, argVals(2).value, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin4(
      fn: String,
      pn1: String,
      pn2: String,
      pn3: String,
      pn4: String,
      defs: Array[Expr] = null)
      extends Builtin(fn: String, Array(pn1, pn2, pn3, pn4), defs) {
    final def evalRhs(args: Array[? <: Eval], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).value, args(1).value, args(2).value, args(3).value, ev, pos)

    def evalRhs(arg1: Eval, arg2: Eval, arg3: Eval, arg4: Eval, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val =
      if (namedNames == null && argVals.length == 4)
        evalRhs(
          argVals(0).value,
          argVals(1).value,
          argVals(2).value,
          argVals(3).value,
          ev,
          outerPos
        )
      else super.apply(argVals, namedNames, outerPos)
  }
}

/**
 * Discriminator for the TCO protocol, passed as an implicit through the call chain.
 *
 * Using a sealed trait (rather than a plain Boolean) gives the JVM JIT better type-profile
 * information at `if` guards, and makes the two modes self-documenting at call sites.
 *
 *   - [[TailstrictModeEnabled]]: explicit `tailstrict` — caller will handle TailCall via
 *     [[TailCall.resolve]]; sentinels may be returned without resolution. Arguments are eagerly
 *     evaluated per the Jsonnet spec.
 *   - [[TailstrictModeDisabled]]: normal call; any TailCall must be resolved before returning.
 *   - [[TailstrictModeAutoTCO]]: auto-TCO — like Enabled (TailCall sentinels may be returned), but
 *     arguments are NOT forced, preserving Jsonnet's standard lazy evaluation semantics.
 */
sealed trait TailstrictMode
case object TailstrictModeEnabled extends TailstrictMode
case object TailstrictModeDisabled extends TailstrictMode
case object TailstrictModeAutoTCO extends TailstrictMode

/**
 * Sentinel value for tail call optimization of `tailstrict` and auto-TCO calls. When a function
 * body's tail position is a `tailstrict` or auto-TCO call, the evaluator returns a [[TailCall]]
 * instead of recursing into the callee. [[TailCall.resolve]] then re-invokes the target function
 * iteratively, eliminating native stack growth.
 *
 * This is an internal protocol value and must never escape to user-visible code paths (e.g.
 * materialization, object field access). Every call site that may produce a TailCall must either
 * pass `TailstrictModeEnabled` / `TailstrictModeAutoTCO` (so the caller resolves it) or guard the
 * result with [[TailCall.resolve]].
 *
 * @param strict
 *   when true, [[TailCall.resolve]] uses [[TailstrictModeEnabled]] (explicit `tailstrict` — eager
 *   argument forcing per the Jsonnet spec). When false, [[TailstrictModeAutoTCO]] is used so that
 *   arguments remain lazy (preserving Jsonnet semantics).
 */
final class TailCall(
    val func: Val.Func,
    val args: Array[Eval],
    val namedNames: Array[String],
    val callSiteExpr: Expr,
    val strict: Boolean = false)
    extends Val {
  private[sjsonnet] def valTag: Byte = -1
  def pos: Position = callSiteExpr.pos
  def prettyName = "tailcall"
  def exprErrorString: String = callSiteExpr.exprErrorString
}

object TailCall {

  /**
   * Iteratively resolve a [[TailCall]] chain (trampoline loop). If `current` is not a TailCall, it
   * is returned immediately. Otherwise, each TailCall's target function is re-invoked until a
   * non-TailCall result is produced.
   *
   * The mode used for re-invocation depends on [[TailCall.strict]]:
   *   - `strict = true` (explicit `tailstrict`): uses [[TailstrictModeEnabled]], which forces eager
   *     argument evaluation inside `func.apply`.
   *   - `strict = false` (auto-TCO): uses [[TailstrictModeAutoTCO]], which preserves lazy argument
   *     evaluation — arguments are only evaluated when the function body accesses them.
   *
   * Error frames preserve the original call-site expression name (e.g. "Apply2") so that TCO does
   * not alter user-visible stack traces.
   */
  @tailrec
  def resolve(current: Val)(implicit ev: EvalScope): Val = current match {
    case tc: TailCall =>
      val mode: TailstrictMode =
        if (tc.strict) TailstrictModeEnabled else TailstrictModeAutoTCO
      val next =
        try {
          tc.func.apply(tc.args, tc.namedNames, tc.callSiteExpr.pos)(ev, mode)
        } catch {
          case e: Error =>
            throw e.addFrame(tc.callSiteExpr.pos, tc.callSiteExpr)
        }
      resolve(next)
    case result => result
  }
}

/**
 * [[EvalScope]] models the per-evaluator context that is propagated throughout the Jsonnet
 * evaluation.
 */
abstract class EvalScope extends EvalErrorScope with Ordering[Val] {
  def visitExpr(expr: Expr)(implicit scope: ValScope): Val

  def materialize(v: Val): ujson.Value

  def equal(x: Val, y: Val): Boolean

  def compare(x: Val, y: Val): Int

  private val emptyMaterializeFileScope = new FileScope(wd / "(materialize)")
  val emptyMaterializeFileScopePos = new Position(emptyMaterializeFileScope, -1)

  def settings: Settings
  def debugStats: DebugStats
  def trace(msg: String): Unit
  def warn(e: Error): Unit

  /**
   * The format cache used by the `%` operator and `std.format` to avoid re-parsing format strings.
   * Defaults to [[FormatCache.SharedDefault]] (a process-wide LRU cache), preserving the original
   * shared caching behavior. Override to supply a custom implementation (e.g., Caffeine-based).
   */
  def formatCache: FormatCache = FormatCache.SharedDefault
}
