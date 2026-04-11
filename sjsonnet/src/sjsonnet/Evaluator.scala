package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{Error as _, *}
import ujson.Value

import sjsonnet.Evaluator.SafeDoubleOps

import scala.annotation.{switch, tailrec}
import scala.util.control.NonFatal

/**
 * Recursively walks the [[Expr]] trees to convert them into into [[Val]] objects that can be
 * materialized to JSON.
 *
 * Performs import resolution and parsing on-demand when the relevant nodes in the syntax tree are
 * reached, and caches the evaluated result of each imported module to be re-used. Parsing is cached
 * separatedly by an external `parseCache`.
 */
class Evaluator(
    resolver: CachedResolver,
    val extVars: String => Option[Expr],
    val wd: Path,
    val settings: Settings,
    logger: Evaluator.Logger = null,
    val debugStats: DebugStats = null,
    override val formatCache: FormatCache = FormatCache.SharedDefault)
    extends EvalScope {
  implicit def evalScope: EvalScope = this
  def importer: CachedImporter = resolver

  if (debugStats != null) Val.Obj.currentDebugStats = debugStats

  def trace(e: String): Unit = if (logger != null) logger(true, e)
  def warn(e: Error): Unit = if (logger != null) logger(false, Error.formatError(e))

  private[this] var stackDepth: Int = 0
  private[this] val maxStack: Int = settings.maxStack
  private[sjsonnet] var profiler: Profiler = _

  @inline private[sjsonnet] final def checkStackDepth(pos: Position): Unit = {
    stackDepth += 1
    if (debugStats != null && stackDepth > debugStats.maxStackDepth)
      debugStats.maxStackDepth = stackDepth
    if (stackDepth > maxStack)
      Error.fail("Max stack frames exceeded.", pos)
  }

  @inline private[sjsonnet] final def checkStackDepth(pos: Position, expr: Expr): Unit = {
    stackDepth += 1
    if (stackDepth > maxStack)
      Error.fail("Max stack frames exceeded.", pos)
  }

  @inline private[sjsonnet] final def checkStackDepth(pos: Position, name: String): Unit = {
    stackDepth += 1
    if (stackDepth > maxStack)
      Error.fail("Max stack frames exceeded.", pos)
  }

  @inline private[sjsonnet] final def decrementStackDepth(): Unit = {
    stackDepth -= 1
  }

  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports: collection.mutable.HashMap[Path, Val] =
    collection.mutable.HashMap.empty[Path, Val]

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = try {
    val p = profiler
    val saved: (AnyRef, Int) = if (p != null) p.enter(e) else null
    try {
      e match {
        case e: ValidId            => visitValidId(e)
        case e: BinaryOp           => visitBinaryOp(e)
        case e: Select             => visitSelect(e)
        case e: Val                => e
        case e: ApplyBuiltin0      => visitApplyBuiltin0(e)
        case e: ApplyBuiltin1      => visitApplyBuiltin1(e)
        case e: ApplyBuiltin2      => visitApplyBuiltin2(e)
        case e: ApplyBuiltin3      => visitApplyBuiltin3(e)
        case e: ApplyBuiltin4      => visitApplyBuiltin4(e)
        case e: And                => visitAnd(e)
        case e: Or                 => visitOr(e)
        case e: UnaryOp            => visitUnaryOp(e)
        case e: Apply1             => visitApply1(e)
        case e: Lookup             => visitLookup(e)
        case e: Function           => visitMethod(e.body, e.params, e.pos)
        case e: LocalExpr          => visitLocalExpr(e)
        case e: Apply              => visitApply(e)
        case e: IfElse             => visitIfElse(e)
        case e: Apply3             => visitApply3(e)
        case e: ObjBody.MemberList => visitMemberList(e.pos, e, null)
        case e: Apply2             => visitApply2(e)
        case e: AssertExpr         => visitAssert(e)
        case e: ApplyBuiltin       => visitApplyBuiltin(e)
        case e: Comp               => visitComp(e)
        case e: Arr                => visitArr(e)
        case e: SelectSuper        => visitSelectSuper(e)
        case e: LookupSuper        => visitLookupSuper(e)
        case e: InSuper            => visitInSuper(e)
        case e: ObjExtend          => visitObjExtend(e)
        case e: ObjBody.ObjComp    => visitObjComp(e, null)
        case e: Slice              => visitSlice(e)
        case e: Import             => visitImport(e)
        case e: Apply0             => visitApply0(e)
        case e: ImportStr          => visitImportStr(e)
        case e: ImportBin          => visitImportBin(e)
        case e: Expr.Error         => visitError(e)
        case e                     => visitInvalid(e)
      }
    } finally if (p != null) p.exit(saved)
  } catch {
    Error.withStackFrame(e)
  }
  // This is only needed for --no-static-errors, otherwise these expression types do not make it past the optimizer
  def visitInvalid(e: Expr): Nothing = e match {
    case Id(pos, name) =>
      Error.fail("Unknown variable: " + name, pos)
    case Self(pos) =>
      Error.fail("Can't use self outside of an object", pos)
    case $(pos) =>
      Error.fail("Can't use $ outside of an object", pos)
    case Super(pos) =>
      Error.fail("Can't use super outside of an object", pos)
    case _ =>
      Error.fail("Should not have happened.", e.pos)
  }

  /**
   * Convert an expression to an [[Eval]] for deferred evaluation.
   *
   * Three fast paths eliminate or reduce allocation vs the naive
   * `new LazyFunc(() => visitExpr(e))`:
   *
   *   1. [[Val]] literals — already evaluated, return as-is (zero cost).
   *   2. [[ValidId]] (variable reference) where the binding slot is non-null — reuse the existing
   *      [[Eval]] from scope directly (zero allocation). Covers ~18% of calls. When the slot IS
   *      null (self-recursive local, e.g. `local a = [a[1], 0]`), the binding hasn't been written
   *      yet, so we must create a deferred thunk to defer the lookup.
   *   3. All other expressions — [[LazyExpr]] stores (Expr, ValScope, Evaluator) as fields instead
   *      of capturing them in a closure: 1 JVM object vs 2. Covers ~76% of calls (dominated by
   *      BinaryOp).
   *
   * PERF: Do not revert to `new LazyFunc(() => visitExpr(e))` — profiling across all benchmark
   * suites shows this method produces ~93% of deferred evaluations. The fast paths eliminate 242K
   * allocations (bench.02) and improve wall-clock time ~5% (comparison2).
   */
  def visitAsLazy(e: Expr)(implicit scope: ValScope): Eval = e match {
    case v: Val     => v
    case e: ValidId =>
      val binding = scope.bindings(e.nameIdx)
      if (binding != null) binding
      else {
        if (debugStats != null) debugStats.lazyCreated += 1
        new LazyExpr(e, scope, this)
      }
    case e =>
      // Try eager evaluation for simple BinaryOp/UnaryOp with resolvable operands.
      // This avoids wrapping them in a LazyExpr thunk that would be immediately forced.
      // Delegates to separate methods to keep visitAsLazy's bytecode frame small.
      val eager = tryEagerEval(e)
      if (eager != null) eager
      else {
        if (debugStats != null) debugStats.lazyCreated += 1
        new LazyExpr(e, scope, this)
      }
  }

  /**
   * Attempt eager evaluation for BinaryOp/UnaryOp with immediately-resolvable operands (Val
   * literals or already-bound scope entries). Returns null if the expression is not eligible or
   * evaluation fails. Kept in a separate method to avoid enlarging visitAsLazy's bytecode frame.
   */
  private def tryEagerEval(e: Expr)(implicit scope: ValScope): Val = e match {
    case bo: BinaryOp =>
      // Inline fast path for numeric arithmetic — avoids the full dispatch chain
      // (visitExpr → visitBinaryOp → visitBinaryOpAsDouble → visitExprAsDouble)
      // and the try/catch in tryEvalCatch. Covers fibonacci hot path (n-1, n-2).
      val lDouble = resolveAsDouble(bo.lhs)
      if (!lDouble.isNaN) {
        val rDouble = resolveAsDouble(bo.rhs)
        if (!rDouble.isNaN)
          return tryInlineArith(bo.op, lDouble, rDouble, bo.pos)
      }
      if (isImmediatelyResolvable(bo.lhs) && isImmediatelyResolvable(bo.rhs))
        tryEvalCatch(bo)
      else null
    case uo: UnaryOp =>
      if (isImmediatelyResolvable(uo.value)) tryEvalCatch(uo)
      else null
    case _ => null
  }

  /**
   * Resolve an expression to a double without full visitExpr dispatch. Returns NaN as sentinel when
   * the expression can't be directly resolved.
   */
  @inline private def resolveAsDouble(e: Expr)(implicit scope: ValScope): Double = e match {
    case n: Val.Num => n.rawDouble
    case v: ValidId =>
      val idx = v.nameIdx
      if (idx < scope.length) {
        val binding = scope.bindings(idx)
        if (binding != null) binding.value match {
          case n: Val.Num => n.rawDouble
          case _          => Double.NaN
        }
        else Double.NaN
      } else Double.NaN
    case _ => Double.NaN
  }

  /**
   * Perform inline numeric binary op, returning null on error or unsupported op. Covers arithmetic,
   * comparison, and bitwise operators. Returns null (fallback to LazyExpr) for: overflow, division
   * by zero, out-of-range bitwise operands, OP_in (string+object), OP_&&/OP_|| (short-circuit).
   */
  @inline private def tryInlineArith(op: Int, ld: Double, rd: Double, pos: Position): Val =
    (op: @switch) match {
      case Expr.BinaryOp.OP_* =>
        val r = ld * rd; if (r.isInfinite) null else Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_/ =>
        if (rd == 0) null
        else { val r = ld / rd; if (r.isInfinite) null else Val.cachedNum(pos, r) }
      case Expr.BinaryOp.OP_% =>
        Val.cachedNum(pos, ld % rd)
      case Expr.BinaryOp.OP_+ =>
        val r = ld + rd; if (r.isInfinite) null else Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_- =>
        val r = ld - rd; if (r.isInfinite) null else Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_<< =>
        val ll = ld.toLong; val rl = rd.toLong
        if (ll.toDouble != ld || rl.toDouble != rd) null // not safe integers
        else if (rl < 0) null
        else if (rl >= 1 && math.abs(ll) >= (1L << (63 - rl))) null
        else Val.cachedNum(pos, (ll << rl).toDouble)
      case Expr.BinaryOp.OP_>> =>
        val ll = ld.toLong; val rl = rd.toLong
        if (ll.toDouble != ld || rl.toDouble != rd) null
        else if (rl < 0) null
        else Val.cachedNum(pos, (ll >> rl).toDouble)
      case Expr.BinaryOp.OP_<  => Val.bool(ld < rd)
      case Expr.BinaryOp.OP_>  => Val.bool(ld > rd)
      case Expr.BinaryOp.OP_<= => Val.bool(ld <= rd)
      case Expr.BinaryOp.OP_>= => Val.bool(ld >= rd)
      case Expr.BinaryOp.OP_== => Val.bool(ld == rd)
      case Expr.BinaryOp.OP_!= => Val.bool(ld != rd)
      case Expr.BinaryOp.OP_&  =>
        val ll = ld.toLong; val rl = rd.toLong
        if (ll.toDouble != ld || rl.toDouble != rd) null
        else Val.cachedNum(pos, (ll & rl).toDouble)
      case Expr.BinaryOp.OP_^ =>
        val ll = ld.toLong; val rl = rd.toLong
        if (ll.toDouble != ld || rl.toDouble != rd) null
        else Val.cachedNum(pos, (ll ^ rl).toDouble)
      case Expr.BinaryOp.OP_| =>
        val ll = ld.toLong; val rl = rd.toLong
        if (ll.toDouble != ld || rl.toDouble != rd) null
        else Val.cachedNum(pos, (ll | rl).toDouble)
      case _ => null // OP_in (string+object), OP_&&/OP_|| (short-circuit)
    }

  /**
   * Evaluate an expression, returning null on any exception (preserving lazy error semantics for
   * unused arguments).
   */
  private def tryEvalCatch(e: Expr)(implicit scope: ValScope): Val =
    try visitExpr(e)
    catch { case NonFatal(_) => null }

  @inline private def isImmediatelyResolvable(e: Expr)(implicit scope: ValScope): Boolean =
    e match {
      case _: Val     => true
      case v: ValidId => v.nameIdx < scope.length && scope.bindings(v.nameIdx) != null
      case _          => false
    }

  def visitValidId(e: ValidId)(implicit scope: ValScope): Val = {
    val ref = scope.bindings(e.nameIdx)
    ref.value
  }

  def visitSelect(e: Select)(implicit scope: ValScope): Val = visitExpr(e.value) match {
    case obj: Val.Obj => obj.value(e.name, e.pos)
    case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${e.name}", e.pos)
  }

  def visitLocalExpr(e: LocalExpr)(implicit scope: ValScope): Val = {
    val bindings = e.bindings
    val s =
      if (bindings == null) scope
      else {
        val base = scope.length
        val newScope = scope.extendBy(bindings.length)
        var i = 0
        while (i < bindings.length) {
          val b = bindings(i)
          newScope.bindings(base + i) = b.args match {
            case null    => visitAsLazy(b.rhs)(newScope)
            case argSpec =>
              if (debugStats != null) debugStats.lazyCreated += 1
              new LazyFunc(() => visitMethod(b.rhs, argSpec, b.pos, b.name)(newScope))
          }
          i += 1
        }
        newScope
      }
    visitExpr(e.returned)(s)
  }

  def visitComp(e: Comp)(implicit scope: ValScope): Val = {
    val results = new collection.mutable.ArrayBuilder.ofRef[Eval]
    results.sizeHint(16)
    visitCompFused(e.first :: e.rest.toList, scope, e.value, results)
    Val.Arr(e.pos, results.result())
  }

  /**
   * Fused scope-building + body evaluation: eliminates intermediate scope array allocation. Instead
   * of first collecting all valid scopes into an Array[ValScope] and then mapping over them with
   * visitAsLazy, this method directly appends body results as it encounters valid scopes. For
   * nested comprehensions like `[x+y for x in arr for y in arr if x==y]`, this avoids allocating
   * O(n²) intermediate scopes — only the O(n) matching results are materialized.
   *
   * For innermost ForSpec with BinaryOp(ValidId,ValidId) body, inlines scope lookups and numeric
   * binary-op dispatch to avoid 3× visitExpr overhead per iteration.
   */
  private def visitCompFused(
      specs: List[CompSpec],
      scope: ValScope,
      body: Expr,
      results: collection.mutable.ArrayBuilder.ofRef[Eval]
  ): Unit = specs match {
    case (spec @ ForSpec(_, name, expr)) :: rest =>
      visitExpr(expr)(scope) match {
        case a: Val.Arr =>
          if (debugStats != null) debugStats.arrayCompIterations += a.length
          val lazyArr = a.asLazyArray
          if (rest.isEmpty) {
            // Innermost loop: try BinaryOp(ValidId,ValidId) fast path
            body match {
              case binOp: BinaryOp
                  if binOp.lhs.tag == ExprTags.ValidId
                    && binOp.rhs.tag == ExprTags.ValidId =>
                // Fast path: reuse mutable scope, inline scope lookups + binary-op dispatch.
                // NOTE: Evaluates eagerly (not lazy). Both go-jsonnet and jrsonnet also
                // evaluate comprehensions eagerly, so this is compatible. Eagerness is
                // required for mutable scope reuse — a lazy thunk would capture the
                // mutable scope and see stale bindings from later iterations.
                val mutableScope = scope.extendBy(1)
                val slot = scope.bindings.length
                val bindings = mutableScope.bindings
                val lhsIdx = binOp.lhs.asInstanceOf[ValidId].nameIdx
                val rhsIdx = binOp.rhs.asInstanceOf[ValidId].nameIdx
                val op = binOp.op
                val bpos = binOp.pos
                var j = 0
                while (j < lazyArr.length) {
                  bindings(slot) = lazyArr(j)
                  val l = bindings(lhsIdx).value
                  val r = bindings(rhsIdx).value
                  (l, r) match {
                    // Only dispatch to numeric fast path for ops it handles (0-16 except OP_in=11).
                    // OP_in expects string+object, OP_&&/OP_|| need short-circuit semantics.
                    case (ln: Val.Num, rn: Val.Num)
                        if op <= Expr.BinaryOp.OP_| && op != Expr.BinaryOp.OP_in =>
                      results += evalBinaryOpNumNum(op, ln, rn, bpos)
                    case _ =>
                      // Fallback to general evaluator for non-numeric types
                      results += visitExpr(binOp)(mutableScope)
                  }
                  j += 1
                }
              case _ if lazyArr.length > 1 && isNonCapturingBody(body) =>
                // Scope reuse for non-capturing bodies: evaluate eagerly with a single
                // mutable scope instead of allocating a new scope per iteration.
                // Safe because non-capturing bodies (ValidId, BinaryOp, UnaryOp, And, Or,
                // IfElse, literals) don't create lazy values that retain scope references.
                val mutableScope = scope.extendBy(1)
                val slot = scope.bindings.length
                var j = 0
                while (j < lazyArr.length) {
                  mutableScope.bindings(slot) = lazyArr(j)
                  results += visitExpr(body)(mutableScope)
                  j += 1
                }
              case _ =>
                var j = 0
                while (j < lazyArr.length) {
                  results += visitAsLazy(body)(scope.extendSimple(lazyArr(j)))
                  j += 1
                }
            }
          } else {
            rest match {
              // Two-level loop-invariant fast path: when the inner ForSpec's expression
              // doesn't depend on the outer variable, evaluate it once instead of N times.
              // E.g. [x*y for x in A for y in B] where B is invariant w.r.t. x.
              case (innerFor: ForSpec) :: Nil
                  if lazyArr.length > 1
                    && isNonCapturingBody(body)
                    && isInvariantExpr(innerFor.cond, scope.bindings.length) =>
                visitCompTwoLevel(lazyArr, innerFor, scope, body, results)
              case _ =>
                // Generic outer loop: recurse for remaining specs
                var j = 0
                while (j < lazyArr.length) {
                  visitCompFused(rest, scope.extendSimple(lazyArr(j)), body, results)
                  j += 1
                }
            }
          }
        case r =>
          Error.fail(
            "In comprehension, can only iterate over array, not " + r.prettyName,
            spec
          )
      }
    case (spec @ IfSpec(offset, expr)) :: rest =>
      visitExpr(expr)(scope) match {
        case Val.True(_) =>
          if (rest.isEmpty) results += visitAsLazy(body)(scope)
          else visitCompFused(rest, scope, body, results)
        case Val.False(_) => // filtered out
        case other        =>
          Error.fail(
            "Condition must be boolean, got " + other.prettyName,
            spec
          )
      }
    case Nil =>
      results += visitAsLazy(body)(scope)
  }

  /**
   * Check if a body expression is "non-capturing" — its evaluation result doesn't retain any
   * reference to the scope bindings array. For such bodies, we can safely reuse a single mutable
   * scope (via extendBy) instead of allocating a new immutable scope per iteration.
   *
   * Non-capturing expressions produce immediate values (Val) rather than lazy thunks that close
   * over the scope. This is critical for correctness: a lazy thunk would capture a reference to the
   * mutable bindings array and see stale values from later iterations.
   */
  private def isNonCapturingBody(e: Expr): Boolean = (e.tag: @scala.annotation.switch) match {
    case ExprTags.ValidId  => true
    case ExprTags.BinaryOp =>
      val b = e.asInstanceOf[BinaryOp]
      isNonCapturingBody(b.lhs) && isNonCapturingBody(b.rhs)
    case ExprTags.UnaryOp =>
      isNonCapturingBody(e.asInstanceOf[UnaryOp].value)
    case ExprTags.And =>
      val a = e.asInstanceOf[And]
      isNonCapturingBody(a.lhs) && isNonCapturingBody(a.rhs)
    case ExprTags.Or =>
      val o = e.asInstanceOf[Or]
      isNonCapturingBody(o.lhs) && isNonCapturingBody(o.rhs)
    case ExprTags.IfElse =>
      val ie = e.asInstanceOf[IfElse]
      isNonCapturingBody(ie.cond) && isNonCapturingBody(ie.`then`) && isNonCapturingBody(
        ie.`else`
      )
    case _ =>
      e.isInstanceOf[Val.Literal]
  }

  /**
   * Check if an expression is "invariant" with respect to scope variables at or above `maxIdx`. An
   * invariant expression only references scope bindings with nameIdx strictly less than maxIdx,
   * meaning it doesn't depend on the outer loop variable and can be hoisted out of the loop.
   *
   * Used by the two-level comprehension optimizer to detect when the inner array expression (e.g.
   * `std.range(0, 999)`) doesn't depend on the outer variable.
   */
  private def isInvariantExpr(e: Expr, maxIdx: Int): Boolean =
    (e.tag: @scala.annotation.switch) match {
      case ExprTags.`Val.Literal` | ExprTags.`Val.Func` => true
      case ExprTags.ValidId                             => e.asInstanceOf[ValidId].nameIdx < maxIdx
      case ExprTags.BinaryOp                            =>
        val b = e.asInstanceOf[BinaryOp]
        isInvariantExpr(b.lhs, maxIdx) && isInvariantExpr(b.rhs, maxIdx)
      case ExprTags.UnaryOp =>
        isInvariantExpr(e.asInstanceOf[UnaryOp].value, maxIdx)
      case ExprTags.Select =>
        isInvariantExpr(e.asInstanceOf[Select].value, maxIdx)
      case ExprTags.And =>
        val a = e.asInstanceOf[And]
        isInvariantExpr(a.lhs, maxIdx) && isInvariantExpr(a.rhs, maxIdx)
      case ExprTags.Or =>
        val o = e.asInstanceOf[Or]
        isInvariantExpr(o.lhs, maxIdx) && isInvariantExpr(o.rhs, maxIdx)
      case ExprTags.ApplyBuiltin0 => true
      case ExprTags.ApplyBuiltin1 =>
        isInvariantExpr(e.asInstanceOf[ApplyBuiltin1].a1, maxIdx)
      case ExprTags.ApplyBuiltin2 =>
        val ab = e.asInstanceOf[ApplyBuiltin2]
        isInvariantExpr(ab.a1, maxIdx) && isInvariantExpr(ab.a2, maxIdx)
      case ExprTags.ApplyBuiltin3 =>
        val ab = e.asInstanceOf[ApplyBuiltin3]
        isInvariantExpr(ab.a1, maxIdx) && isInvariantExpr(ab.a2, maxIdx) && isInvariantExpr(
          ab.a3,
          maxIdx
        )
      case ExprTags.Apply0 =>
        isInvariantExpr(e.asInstanceOf[Apply0].value, maxIdx)
      case ExprTags.Apply1 =>
        val a = e.asInstanceOf[Apply1]
        isInvariantExpr(a.value, maxIdx) && isInvariantExpr(a.a1, maxIdx)
      case ExprTags.Apply2 =>
        val a = e.asInstanceOf[Apply2]
        isInvariantExpr(a.value, maxIdx) && isInvariantExpr(a.a1, maxIdx) &&
        isInvariantExpr(a.a2, maxIdx)
      case ExprTags.Apply3 =>
        val a = e.asInstanceOf[Apply3]
        isInvariantExpr(a.value, maxIdx) && isInvariantExpr(a.a1, maxIdx) &&
        isInvariantExpr(a.a2, maxIdx) && isInvariantExpr(a.a3, maxIdx)
      case _ => false
    }

  /**
   * Optimized two-level comprehension handler for `[body for x in A for y in B]` when B is
   * invariant w.r.t. x and body is non-capturing.
   *
   * Key optimizations:
   *   1. Loop-invariant hoisting: evaluates `B` once instead of `|A|` times.
   *   2. Mutable scope reuse: allocates one scope with 2 slots (outer + inner), mutated in-place
   *      instead of allocating `|A|×|B|` scope copies.
   *   3. Outer value hoisting: for `BinaryOp(ValidId, ValidId)` bodies, resolves the outer operand
   *      once per outer iteration and hoists its type check.
   *
   * Extracted as a separate method to keep `visitCompFused` small enough for JIT inlining — large
   * methods hurt JIT optimization decisions.
   */
  private def visitCompTwoLevel(
      outerArr: Array[Eval],
      innerFor: ForSpec,
      scope: ValScope,
      body: Expr,
      results: collection.mutable.ArrayBuilder.ofRef[Eval]
  ): Unit = {
    // Evaluate inner array once (it's invariant w.r.t. outer var)
    val innerArrVal = visitExpr(innerFor.cond)(scope).cast[Val.Arr]
    if (debugStats != null) debugStats.arrayCompIterations += outerArr.length * innerArrVal.length
    val innerArr = innerArrVal.asLazyArray

    if (innerArr.length == 0) return

    // Pre-size the result array for the cross-product
    results.sizeHint(outerArr.length * innerArr.length)

    // Allocate a single mutable scope with 2 slots: outer (slot) + inner (slot+1)
    val mutableScope = scope.extendBy(2)
    val slot = scope.bindings.length
    val extBindings = mutableScope.bindings

    body match {
      case binOp: BinaryOp
          if binOp.lhs.tag == ExprTags.ValidId
            && binOp.rhs.tag == ExprTags.ValidId =>
        // Hoisted BinaryOp(ValidId, ValidId) fast path: resolve outer operand once per
        // outer iteration, type-check it once, then iterate inner array tightly.
        val lhsIdx = binOp.lhs.asInstanceOf[ValidId].nameIdx
        val rhsIdx = binOp.rhs.asInstanceOf[ValidId].nameIdx
        val op = binOp.op
        val bpos = binOp.pos
        val innerSlot = slot + 1

        if (lhsIdx == slot && op <= Expr.BinaryOp.OP_| && op != Expr.BinaryOp.OP_in) {
          // body = outerVar op innerVar — tightest path
          var i = 0
          while (i < outerArr.length) {
            extBindings(slot) = outerArr(i)
            val outerVal = extBindings(lhsIdx).value
            outerVal match {
              case outerNum: Val.Num =>
                var j = 0
                while (j < innerArr.length) {
                  extBindings(innerSlot) = innerArr(j)
                  val innerVal = extBindings(rhsIdx).value
                  innerVal match {
                    case innerNum: Val.Num =>
                      results += evalBinaryOpNumNum(op, outerNum, innerNum, bpos)
                    case _ =>
                      results += visitExpr(binOp)(mutableScope)
                  }
                  j += 1
                }
              case _ =>
                var j = 0
                while (j < innerArr.length) {
                  extBindings(innerSlot) = innerArr(j)
                  results += visitExpr(binOp)(mutableScope)
                  j += 1
                }
            }
            i += 1
          }
        } else if (rhsIdx == slot && op <= Expr.BinaryOp.OP_| && op != Expr.BinaryOp.OP_in) {
          // body = innerVar op outerVar (swapped operands)
          var i = 0
          while (i < outerArr.length) {
            extBindings(slot) = outerArr(i)
            val outerVal = extBindings(rhsIdx).value
            outerVal match {
              case outerNum: Val.Num =>
                var j = 0
                while (j < innerArr.length) {
                  extBindings(innerSlot) = innerArr(j)
                  val innerVal = extBindings(lhsIdx).value
                  innerVal match {
                    case innerNum: Val.Num =>
                      results += evalBinaryOpNumNum(op, innerNum, outerNum, bpos)
                    case _ =>
                      results += visitExpr(binOp)(mutableScope)
                  }
                  j += 1
                }
              case _ =>
                var j = 0
                while (j < innerArr.length) {
                  extBindings(innerSlot) = innerArr(j)
                  results += visitExpr(binOp)(mutableScope)
                  j += 1
                }
            }
            i += 1
          }
        } else {
          // Generic BinaryOp(ValidId, ValidId) — not numeric-optimizable
          var i = 0
          while (i < outerArr.length) {
            extBindings(slot) = outerArr(i)
            var j = 0
            while (j < innerArr.length) {
              extBindings(innerSlot) = innerArr(j)
              results += visitExpr(binOp)(mutableScope)
              j += 1
            }
            i += 1
          }
        }

      case _ =>
        // General non-capturing body with mutable scope reuse
        val innerSlot = slot + 1
        var i = 0
        while (i < outerArr.length) {
          extBindings(slot) = outerArr(i)
          var j = 0
          while (j < innerArr.length) {
            extBindings(innerSlot) = innerArr(j)
            results += visitExpr(body)(mutableScope)
            j += 1
          }
          i += 1
        }
    }
  }

  /**
   * Fast-path binary op evaluation for Num×Num operands within comprehension inner loops. Handles
   * the most common operations without visitExpr dispatch overhead.
   */
  @inline private def evalBinaryOpNumNum(op: Int, ln: Val.Num, rn: Val.Num, pos: Position): Val = {
    val ld = ln.asDouble
    val rd = rn.asDouble
    (op: @switch) match {
      case Expr.BinaryOp.OP_+ => Val.cachedNum(pos, ld + rd)
      case Expr.BinaryOp.OP_- =>
        val r = ld - rd
        if (r.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_* =>
        val r = ld * rd
        if (r.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_/ =>
        if (rd == 0) Error.fail("division by zero", pos)
        val r = ld / rd
        if (r.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_% => Val.cachedNum(pos, ld % rd)
      // Use position-free static singletons for boolean results — this method is only called
      // from comprehension fast paths where position info on boolean results is unnecessary.
      // Avoids 1 object allocation per comparison in inner loops (significant for 1M+ iterations).
      case Expr.BinaryOp.OP_<  => Val.bool(ld < rd)
      case Expr.BinaryOp.OP_>  => Val.bool(ld > rd)
      case Expr.BinaryOp.OP_<= => Val.bool(ld <= rd)
      case Expr.BinaryOp.OP_>= => Val.bool(ld >= rd)
      case Expr.BinaryOp.OP_== => Val.bool(ld == rd)
      case Expr.BinaryOp.OP_!= => Val.bool(ld != rd)
      case Expr.BinaryOp.OP_<< =>
        val ll = ld.toSafeLong(pos); val rr = rd.toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr)))
          Error.fail("numeric value outside safe integer range for bitwise operation", pos)
        Val.cachedNum(pos, (ll << rr).toDouble)
      case Expr.BinaryOp.OP_>> =>
        val ll = ld.toSafeLong(pos); val rr = rd.toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        Val.cachedNum(pos, (ll >> rr).toDouble)
      case Expr.BinaryOp.OP_& =>
        Val.cachedNum(pos, (ld.toSafeLong(pos) & rd.toSafeLong(pos)).toDouble)
      case Expr.BinaryOp.OP_^ =>
        Val.cachedNum(pos, (ld.toSafeLong(pos) ^ rd.toSafeLong(pos)).toDouble)
      case Expr.BinaryOp.OP_| =>
        Val.cachedNum(pos, (ld.toSafeLong(pos) | rd.toSafeLong(pos)).toDouble)
      case _ =>
        // Should be unreachable: caller filters to ops 0-16 except OP_in
        throw new AssertionError(s"Unexpected numeric binary op: $op")
    }
  }

  def visitArr(e: Arr)(implicit scope: ValScope): Val =
    Val.Arr(e.pos, e.value.map(visitAsLazy))

  def visitSelectSuper(e: SelectSuper)(implicit scope: ValScope): Val = {
    val sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    if (sup == null) Error.fail("Attempt to use `super` when there is no super class", e.pos)
    else sup.value(e.name, e.pos, scope.bindings(e.selfIdx).asInstanceOf[Val.Obj])
  }

  def visitObjExtend(e: ObjExtend)(implicit scope: ValScope): Val = {
    val original = visitExpr(e.base).cast[Val.Obj]
    e.ext match {
      case ext: ObjBody.MemberList => visitMemberList(e.pos, ext, original)
      case ext: ObjBody.ObjComp    => visitObjComp(ext, original)
      case o: Val.Obj              => o.addSuper(e.pos, original)
      case _                       => Error.fail("Should not have happened", e.pos)
    }
  }

  def visitIfElse(e: IfElse)(implicit scope: ValScope): Val = {
    visitExpr(e.cond) match {
      case Val.True(_)  => visitExpr(e.`then`)
      case Val.False(_) =>
        e.`else` match {
          case null => Val.staticNull
          case v    => visitExpr(v)
        }
      case v => Error.fail("Need boolean, found " + v.prettyName, e.pos)
    }
  }

  def visitError(e: Expr.Error)(implicit scope: ValScope): Nothing = {
    Error.fail(materializeError(visitExpr(e.value)), e.pos)
  }

  protected def materializeError(value: Val): String = value match {
    case Val.Str(_, s) => s
    case r             => Materializer.stringify(r)
  }

  def visitUnaryOp(e: UnaryOp)(implicit scope: ValScope): Val = {
    val pos = e.pos
    (e.op: @switch) match {
      case Expr.UnaryOp.OP_+ => Val.cachedNum(pos, visitExprAsDouble(e.value))
      case Expr.UnaryOp.OP_- => Val.cachedNum(pos, -visitExprAsDouble(e.value))
      case Expr.UnaryOp.OP_~ =>
        Val.cachedNum(pos, (~visitExprAsDouble(e.value).toSafeLong(pos)).toDouble)
      case Expr.UnaryOp.OP_! =>
        visitExpr(e.value) match {
          case Val.True(_)  => Val.staticFalse
          case Val.False(_) => Val.staticTrue
          case v            =>
            Error.fail(s"Unknown unary operation: ! ${v.prettyName}", pos)
        }
      case _ =>
        val v = visitExpr(e.value)
        Error.fail(s"Unknown unary operation: ${Expr.UnaryOp.name(e.op)} ${v.prettyName}", pos)
    }
  }

  /**
   * Fast path: evaluate an expression expected to produce a Double, avoiding intermediate
   * [[Val.Num]] allocation. When a numeric expression chain like `a + b * c - d` is evaluated,
   * intermediate results stay as raw JVM `double` primitives (zero allocation) instead of being
   * boxed into `Val.Num` objects (~24-32 bytes each) at every step.
   *
   * Only the outermost operation (in [[visitBinaryOp]]) boxes the final result into a `Val.Num`.
   */
  private def visitExprAsDouble(e: Expr)(implicit scope: ValScope): Double = try {
    e match {
      case v: Val.Num => v.asDouble
      case v: Val     => Error.fail("Expected Number, got " + v.prettyName, e.pos)
      case e: ValidId =>
        scope.bindings(e.nameIdx).value match {
          case n: Val.Num => n.asDouble
          case v          => Error.fail("Expected Number, got " + v.prettyName, e.pos)
        }
      case e: BinaryOp => visitBinaryOpAsDouble(e)
      case e: UnaryOp  => visitUnaryOpAsDouble(e)
      case e           =>
        visitExpr(e) match {
          case n: Val.Num => n.asDouble
          case v          => Error.fail("Expected Number, got " + v.prettyName, e.pos)
        }
    }
  } catch {
    Error.withStackFrame(e)
  }

  private def visitBinaryOpAsDouble(e: BinaryOp)(implicit scope: ValScope): Double = {
    val pos = e.pos
    (e.op: @switch) match {
      case Expr.BinaryOp.OP_* =>
        val r = visitExprAsDouble(e.lhs) * visitExprAsDouble(e.rhs)
        if (r.isInfinite) Error.fail("overflow", pos); r
      case Expr.BinaryOp.OP_/ =>
        val l = visitExprAsDouble(e.lhs)
        val r = visitExprAsDouble(e.rhs)
        if (r == 0) Error.fail("division by zero", pos)
        val result = l / r
        if (result.isInfinite) Error.fail("overflow", pos); result
      case Expr.BinaryOp.OP_% =>
        visitExprAsDouble(e.lhs) % visitExprAsDouble(e.rhs)
      case Expr.BinaryOp.OP_+ =>
        val r = visitExprAsDouble(e.lhs) + visitExprAsDouble(e.rhs)
        if (r.isInfinite) Error.fail("overflow", pos); r
      case Expr.BinaryOp.OP_- =>
        val r = visitExprAsDouble(e.lhs) - visitExprAsDouble(e.rhs)
        if (r.isInfinite) Error.fail("overflow", pos); r
      case Expr.BinaryOp.OP_<< =>
        val ll = visitExprAsDouble(e.lhs).toSafeLong(pos)
        val rr = visitExprAsDouble(e.rhs).toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr)))
          Error.fail("numeric value outside safe integer range for bitwise operation", pos)
        (ll << rr).toDouble
      case Expr.BinaryOp.OP_>> =>
        val ll = visitExprAsDouble(e.lhs).toSafeLong(pos)
        val rr = visitExprAsDouble(e.rhs).toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        (ll >> rr).toDouble
      case Expr.BinaryOp.OP_& =>
        (visitExprAsDouble(e.lhs).toSafeLong(pos) & visitExprAsDouble(e.rhs).toSafeLong(
          pos
        )).toDouble
      case Expr.BinaryOp.OP_^ =>
        (visitExprAsDouble(e.lhs).toSafeLong(pos) ^ visitExprAsDouble(e.rhs).toSafeLong(
          pos
        )).toDouble
      case Expr.BinaryOp.OP_| =>
        (visitExprAsDouble(e.lhs).toSafeLong(pos) | visitExprAsDouble(e.rhs).toSafeLong(
          pos
        )).toDouble
      case _ =>
        visitBinaryOp(e).asDouble
    }
  }

  private def visitUnaryOpAsDouble(e: UnaryOp)(implicit scope: ValScope): Double =
    (e.op: @switch) match {
      case Expr.UnaryOp.OP_- => -visitExprAsDouble(e.value)
      case Expr.UnaryOp.OP_+ => visitExprAsDouble(e.value)
      case Expr.UnaryOp.OP_~ => (~visitExprAsDouble(e.value).toSafeLong(e.pos)).toDouble
      case _                 => visitUnaryOp(e).asDouble
    }

  /**
   * Function application entry points (visitApply/visitApply0-3 for user functions,
   * visitApplyBuiltin/visitApplyBuiltin0-4 for built-in functions).
   *
   * When `e.tailstrict` is true, the result is wrapped in `TailCall.resolve()` which iteratively
   * resolves any [[TailCall]] chain. When false, arguments are wrapped as lazy thunks to preserve
   * Jsonnet's default lazy evaluation semantics, and `Val.Func.apply` resolves any TailCall
   * internally via `TailCall.resolve` before returning.
   */
  protected def visitApply(e: Apply)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.functionCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val lhs = visitExpr(e.value)
      // Auto-TCO'd calls should normally be intercepted by visitExprWithTailCallSupport,
      // but we handle them defensively here to preserve lazy semantics if this path is ever reached.
      implicit val tailstrictMode: TailstrictMode =
        if (!e.strict && e.tailstrict) TailstrictModeAutoTCO
        else if (e.tailstrict) TailstrictModeEnabled
        else TailstrictModeDisabled

      if (e.tailstrict) {
        val args: Array[Eval] =
          if (!e.strict) e.args.map(visitAsLazy(_))
          else e.args.map(visitExpr(_)).asInstanceOf[Array[Eval]]
        TailCall.resolve(lhs.cast[Val.Func].apply(args, e.namedNames, e.pos))
      } else {
        lhs.cast[Val.Func].apply(e.args.map(visitAsLazy(_)), e.namedNames, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApply0(e: Apply0)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.functionCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val lhs = visitExpr(e.value)
      implicit val tailstrictMode: TailstrictMode =
        if (!e.strict && e.tailstrict) TailstrictModeAutoTCO
        else if (e.tailstrict) TailstrictModeEnabled
        else TailstrictModeDisabled
      if (e.tailstrict) {
        TailCall.resolve(lhs.cast[Val.Func].apply0(e.pos))
      } else {
        lhs.cast[Val.Func].apply0(e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApply1(e: Apply1)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.functionCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val lhs = visitExpr(e.value)
      implicit val tailstrictMode: TailstrictMode =
        if (!e.strict && e.tailstrict) TailstrictModeAutoTCO
        else if (e.tailstrict) TailstrictModeEnabled
        else TailstrictModeDisabled
      if (e.tailstrict) {
        val arg: Eval = if (!e.strict) visitAsLazy(e.a1) else visitExpr(e.a1)
        TailCall.resolve(lhs.cast[Val.Func].apply1(arg, e.pos))
      } else {
        val l1 = visitAsLazy(e.a1)
        lhs.cast[Val.Func].apply1(l1, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApply2(e: Apply2)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.functionCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val lhs = visitExpr(e.value)
      implicit val tailstrictMode: TailstrictMode =
        if (!e.strict && e.tailstrict) TailstrictModeAutoTCO
        else if (e.tailstrict) TailstrictModeEnabled
        else TailstrictModeDisabled

      if (e.tailstrict) {
        if (!e.strict) {
          TailCall.resolve(
            lhs.cast[Val.Func].apply2(visitAsLazy(e.a1), visitAsLazy(e.a2), e.pos)
          )
        } else {
          TailCall.resolve(lhs.cast[Val.Func].apply2(visitExpr(e.a1), visitExpr(e.a2), e.pos))
        }
      } else {
        val l1 = visitAsLazy(e.a1)
        val l2 = visitAsLazy(e.a2)
        lhs.cast[Val.Func].apply2(l1, l2, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApply3(e: Apply3)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.functionCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val lhs = visitExpr(e.value)
      implicit val tailstrictMode: TailstrictMode =
        if (!e.strict && e.tailstrict) TailstrictModeAutoTCO
        else if (e.tailstrict) TailstrictModeEnabled
        else TailstrictModeDisabled

      if (e.tailstrict) {
        if (!e.strict) {
          TailCall.resolve(
            lhs
              .cast[Val.Func]
              .apply3(visitAsLazy(e.a1), visitAsLazy(e.a2), visitAsLazy(e.a3), e.pos)
          )
        } else {
          TailCall.resolve(
            lhs.cast[Val.Func].apply3(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), e.pos)
          )
        }
      } else {
        val l1 = visitAsLazy(e.a1)
        val l2 = visitAsLazy(e.a2)
        val l3 = visitAsLazy(e.a3)
        lhs.cast[Val.Func].apply3(l1, l2, l3, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin0(e: ApplyBuiltin0): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val result = e.func.evalRhs(this, e.pos)
      if (e.tailstrict) TailCall.resolve(result) else result
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin1(e: ApplyBuiltin1)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      if (e.tailstrict) {
        TailCall.resolve(e.func.evalRhs(visitExpr(e.a1), this, e.pos))
      } else {
        e.func.evalRhs(visitAsLazy(e.a1), this, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin2(e: ApplyBuiltin2)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      if (e.tailstrict) {
        TailCall.resolve(e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), this, e.pos))
      } else {
        e.func.evalRhs(visitAsLazy(e.a1), visitAsLazy(e.a2), this, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin3(e: ApplyBuiltin3)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      if (e.tailstrict) {
        TailCall.resolve(
          e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), this, e.pos)
        )
      } else {
        e.func.evalRhs(visitAsLazy(e.a1), visitAsLazy(e.a2), visitAsLazy(e.a3), this, e.pos)
      }
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin4(e: ApplyBuiltin4)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      if (e.tailstrict) {
        TailCall.resolve(
          e.func.evalRhs(
            visitExpr(e.a1),
            visitExpr(e.a2),
            visitExpr(e.a3),
            visitExpr(e.a4),
            this,
            e.pos
          )
        )
      } else {
        e.func.evalRhs(
          visitAsLazy(e.a1),
          visitAsLazy(e.a2),
          visitAsLazy(e.a3),
          visitAsLazy(e.a4),
          this,
          e.pos
        )
      }
    } finally decrementStackDepth()
  }

  protected def visitApplyBuiltin(e: ApplyBuiltin)(implicit scope: ValScope): Val = {
    if (debugStats != null) debugStats.builtinCalls += 1
    checkStackDepth(e.pos, e)
    try {
      val arr = new Array[Eval](e.argExprs.length)
      var idx = 0

      if (e.tailstrict) {
        while (idx < e.argExprs.length) {
          arr(idx) = visitExpr(e.argExprs(idx))
          idx += 1
        }
        TailCall.resolve(e.func.evalRhs(arr, this, e.pos))
      } else {
        while (idx < e.argExprs.length) {
          val boundIdx = idx
          arr(idx) = visitAsLazy(e.argExprs(boundIdx))
          idx += 1
        }
        e.func.evalRhs(arr, this, e.pos)
      }
    } finally decrementStackDepth()
  }

  def visitAssert(e: AssertExpr)(implicit scope: ValScope): Val = {
    if (!visitExpr(e.asserted.value).isInstanceOf[Val.True]) {
      e.asserted.msg match {
        case null => Error.fail("Assertion failed", e)
        case msg  =>
          Error.fail("Assertion failed: " + materializeError(visitExpr(msg)), e)
      }
    }
    visitExpr(e.returned)
  }

  protected def visitSlice(e: Slice)(implicit scope: ValScope): Val = {
    def extractParam(e: Option[Expr]): Option[Int] = e.flatMap(visitExpr(_) match {
      case _: Val.Null => None
      case v: Val.Num  => Some(v.asInt)
      case v: Val      => Some(v.cast[Val.Num].asInt)
    })

    val indexable = visitExpr(e.value) match {
      case arr: Val.Arr => arr
      case str: Val.Str => str
      case x            => Error.fail("Can only slice array or string, not " + x.prettyName, e.pos)
    }
    Util.slice(
      e.pos,
      this,
      indexable,
      extractParam(e.start),
      extractParam(e.end),
      extractParam(e.stride)
    )
  }

  // Nested match avoids Tuple2 allocation from (visitExpr(...), visitExpr(...)) match
  def visitLookup(e: Lookup)(implicit scope: ValScope): Val = {
    val pos = e.pos
    val lhs = visitExpr(e.value)
    val rhs = visitExpr(e.index)
    lhs match {
      case v: Val.Arr =>
        rhs match {
          case i: Val.Num =>
            val int = i.asPositiveInt
            if (v.length == 0) Error.fail("array bounds error: array is empty", pos)
            if (int >= v.length)
              Error.fail(s"array bounds error: $int not within [0, ${v.length})", pos)
            v.value(int)
          case _ =>
            Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
        }
      case v: Val.Str =>
        rhs match {
          case i: Val.Num =>
            val int = i.asPositiveInt
            val str = v.str
            if (str.isEmpty) Error.fail("string bounds error: string is empty", pos)
            val unicodeLength = str.codePointCount(0, str.length)
            if (int >= unicodeLength)
              Error.fail(s"string bounds error: $int not within [0, $unicodeLength)", pos)
            val startUtf16 = if (int == 0) 0 else str.offsetByCodePoints(0, int)
            val endUtf16 = str.offsetByCodePoints(startUtf16, 1)
            Val.Str(pos, str.substring(startUtf16, endUtf16))
          case _ =>
            Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
        }
      case v: Val.Obj =>
        rhs match {
          case i: Val.Str => v.value(i.str, pos)
          case _          =>
            Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
        }
      case _ =>
        Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
    }
  }

  def visitLookupSuper(e: LookupSuper)(implicit scope: ValScope): Val = {
    var sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    val key = visitExpr(e.index).cast[Val.Str]
    if (sup == null) sup = scope.bindings(e.selfIdx).asInstanceOf[Val.Obj]
    sup.value(key.str, e.pos)
  }

  def visitImportStr(e: ImportStr): Val.Str = {
    Val.Str(
      e.pos,
      importer.resolveAndReadOrFail(e.value, e.pos, binaryData = false)._2.readString()
    )
  }

  def visitImportBin(e: ImportBin): Val.Arr = {
    Val.Arr(
      e.pos,
      importer
        .resolveAndReadOrFail(e.value, e.pos, binaryData = true)
        ._2
        .readRawBytes()
        .map(x => Val.cachedNum(e.pos, (x & 0xff).doubleValue))
    )
  }

  def visitImport(e: Import): Val = {
    val (p, str) = importer.resolveAndReadOrFail(e.value, e.pos, binaryData = false)
    val cached = cachedImports.contains(p)
    if (debugStats != null && cached) debugStats.importCacheHits += 1
    cachedImports.getOrElseUpdate(
      p, {
        if (debugStats != null) debugStats.importCalls += 1
        checkStackDepth(e.pos, e)
        try {
          val doc = resolver.parse(p, str) match {
            case Right((expr, _)) => expr
            case Left(err)        => throw err.asSeenFrom(this)
          }
          visitExpr(doc)(ValScope.empty)
        } finally decrementStackDepth()
      }
    )
  }

  def visitAnd(e: And)(implicit scope: ValScope): Val.Bool = {
    visitExpr(e.lhs) match {
      case _: Val.True =>
        visitExpr(e.rhs) match {
          case b: Val.Bool =>
            b
          case unknown =>
            Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case _: Val.False =>
        Val.staticFalse
      case unknown =>
        Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitOr(e: Or)(implicit scope: ValScope): Val.Bool = {
    visitExpr(e.lhs) match {
      case _: Val.True  => Val.staticTrue
      case _: Val.False =>
        visitExpr(e.rhs) match {
          case b: Val.Bool => b
          case unknown     =>
            Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case unknown =>
        Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitInSuper(e: InSuper)(implicit scope: ValScope): Val.Bool = {
    val sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    if (sup == null) Val.staticFalse
    else {
      val key = visitExpr(e.value).cast[Val.Str]
      Val.bool(sup.containsKey(key.str))
    }
  }

  def visitBinaryOp(e: BinaryOp)(implicit scope: ValScope): Val.Literal = {
    val pos = e.pos
    (e.op: @switch) match {
      // Pure numeric fast path: avoid intermediate Val.Num allocation
      case Expr.BinaryOp.OP_* =>
        val r = visitExprAsDouble(e.lhs) * visitExprAsDouble(e.rhs)
        if (r.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_- =>
        val r = visitExprAsDouble(e.lhs) - visitExprAsDouble(e.rhs)
        if (r.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, r)
      case Expr.BinaryOp.OP_/ =>
        val l = visitExprAsDouble(e.lhs)
        val r = visitExprAsDouble(e.rhs)
        if (r == 0) Error.fail("division by zero", pos)
        val result = l / r
        if (result.isInfinite) Error.fail("overflow", pos)
        Val.cachedNum(pos, result)
      // Polymorphic ops: nested match avoids Tuple2 allocation; Num checked first (most common)
      case Expr.BinaryOp.OP_% =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        l match {
          case Val.Num(_, ld) =>
            r match {
              case Val.Num(_, rd) => Val.cachedNum(pos, ld % rd)
              case _              => failBinOp(l, e.op, r, pos)
            }
          case ls: Val.Str => Val.Str(pos, Format.format(ls.str, r, pos))
          case _           => failBinOp(l, e.op, r, pos)
        }

      case Expr.BinaryOp.OP_+ =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.cachedNum(pos, l + r)
          case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
          case (n: Val.Num, Val.Str(_, r)) => Val.Str(pos, RenderUtils.renderDouble(n.asDouble) + r)
          case (Val.Str(_, l), n: Val.Num) => Val.Str(pos, l + RenderUtils.renderDouble(n.asDouble))
          case (Val.Str(_, l), r)          => Val.Str(pos, l + Materializer.stringify(r))
          case (l, Val.Str(_, r))          => Val.Str(pos, Materializer.stringify(l) + r)
          case (l: Val.Obj, r: Val.Obj)    => r.addSuper(pos, l)
          case (l: Val.Arr, r: Val.Arr)    => l.concat(pos, r)
          case _                           => failBinOp(l, e.op, r, pos)
        }

      // Shift ops: pure numeric with safe-integer range check
      case Expr.BinaryOp.OP_<< =>
        val ll = visitExprAsDouble(e.lhs).toSafeLong(pos)
        val rr = visitExprAsDouble(e.rhs).toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr)))
          Error.fail("numeric value outside safe integer range for bitwise operation", pos)
        else
          Val.cachedNum(pos, (ll << rr).toDouble)

      case Expr.BinaryOp.OP_>> =>
        val ll = visitExprAsDouble(e.lhs).toSafeLong(pos)
        val rr = visitExprAsDouble(e.rhs).toSafeLong(pos)
        if (rr < 0) Error.fail("shift by negative exponent", pos)
        Val.cachedNum(pos, (ll >> rr).toDouble)

      // Comparison ops: polymorphic (Num/Str/Arr)
      case Expr.BinaryOp.OP_< =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) =>
            Val.bool(Util.compareStringsByCodepoint(l, r) < 0)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(l < r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(compare(x, y) < 0)
          case _                              => failBinOp(l, e.op, r, pos)
        }

      case Expr.BinaryOp.OP_> =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) =>
            Val.bool(Util.compareStringsByCodepoint(l, r) > 0)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(l > r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(compare(x, y) > 0)
          case _                              => failBinOp(l, e.op, r, pos)
        }

      case Expr.BinaryOp.OP_<= =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) =>
            Val.bool(Util.compareStringsByCodepoint(l, r) <= 0)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(l <= r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(compare(x, y) <= 0)
          case _                              => failBinOp(l, e.op, r, pos)
        }

      case Expr.BinaryOp.OP_>= =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) =>
            Val.bool(Util.compareStringsByCodepoint(l, r) >= 0)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(l >= r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(compare(x, y) >= 0)
          case _                              => failBinOp(l, e.op, r, pos)
        }

      case Expr.BinaryOp.OP_in =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        l match {
          case ls: Val.Str =>
            r match {
              case o: Val.Obj => Val.bool(o.containsKey(ls.str))
              case _          => failBinOp(l, e.op, r, pos)
            }
          case _ => failBinOp(l, e.op, r, pos)
        }

      // Equality ops
      case Expr.BinaryOp.OP_== =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func])
          Error.fail("cannot test equality of functions", pos)
        Val.bool(equal(l, r))

      case Expr.BinaryOp.OP_!= =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func])
          Error.fail("cannot test equality of functions", pos)
        Val.bool(!equal(l, r))

      // Bitwise ops: pure numeric with safe-integer range check
      case Expr.BinaryOp.OP_& =>
        Val.cachedNum(
          pos,
          (visitExprAsDouble(e.lhs).toSafeLong(pos) &
          visitExprAsDouble(e.rhs).toSafeLong(pos)).toDouble
        )

      case Expr.BinaryOp.OP_^ =>
        Val.cachedNum(
          pos,
          (visitExprAsDouble(e.lhs).toSafeLong(pos) ^
          visitExprAsDouble(e.rhs).toSafeLong(pos)).toDouble
        )

      case Expr.BinaryOp.OP_| =>
        Val.cachedNum(
          pos,
          (visitExprAsDouble(e.lhs).toSafeLong(pos) |
          visitExprAsDouble(e.rhs).toSafeLong(pos)).toDouble
        )

      case _ =>
        val l = visitExpr(e.lhs)
        val r = visitExpr(e.rhs)
        failBinOp(l, e.op, r, pos)
    }
  }

  @inline private def failBinOp(l: Val, op: Int, r: Val, pos: Position): Nothing =
    Error.fail(
      s"Unknown binary operation: ${l.prettyName} ${Expr.BinaryOp.name(op)} ${r.prettyName}",
      pos
    )

  def visitFieldName(fieldName: FieldName, pos: Position)(implicit scope: ValScope): String = {
    fieldName match {
      case FieldName.Fixed(s) => s
      case FieldName.Dyn(k)   =>
        visitExpr(k) match {
          case Val.Str(_, k1) => k1
          case Val.Null(_)    => null
          case x              => fieldNameTypeError(x, pos)
        }
    }
  }

  private def fieldNameTypeError(fieldName: Val, pos: Position): Nothing = {
    Error.fail(s"Field name must be string or null, not ${fieldName.prettyName}", pos)
  }

  def visitMethod(rhs: Expr, params: Params, outerPos: Position, name: String = null)(implicit
      scope: ValScope): Val.Func =
    new Val.Func(outerPos, scope, params) {
      override def functionName: String = name
      override val bodyExpr: Expr = rhs
      def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position): Val =
        visitExprWithTailCallSupport(rhs)(vs)
      override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = {
        checkStackDepth(expr.pos, "default")
        try visitExpr(expr)(vs)
        finally decrementStackDepth()
      }
    }

  // And/Or rhs tail-position helpers — extracted to preserve @tailrec on visitExprWithTailCallSupport.
  // TailCall sentinels pass through without a boolean type check: this is a deliberate semantic
  // relaxation matching google/jsonnet behavior (where `&&` is simply `if a then b else false`).
  // Direct non-boolean rhs values (e.g. `true && "hello"`) are still caught.
  private def visitAndRhsTailPos(rhs: Expr, andPos: Position)(implicit scope: ValScope): Val = {
    visitExprWithTailCallSupport(rhs) match {
      case b: Val.Bool  => b
      case tc: TailCall => tc
      case unknown      =>
        Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", andPos)
    }
  }

  private def visitOrRhsTailPos(rhs: Expr, orPos: Position)(implicit scope: ValScope): Val = {
    visitExprWithTailCallSupport(rhs) match {
      case b: Val.Bool  => b
      case tc: TailCall => tc
      case unknown      =>
        Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", orPos)
    }
  }

  /**
   * Evaluate an expression with tail-call support. When a `tailstrict` call is encountered at a
   * potential tail position, returns a [[TailCall]] sentinel instead of recursing, enabling
   * `TailCall.resolve` in `visitApply*` to iterate rather than grow the JVM stack.
   *
   * Potential tail positions are propagated through: IfElse (both branches), LocalExpr (returned),
   * AssertExpr (returned), And (rhs), Or (rhs), and Expr.Error (value). All other expression types
   * delegate to normal `visitExpr`.
   */
  @tailrec
  private def visitExprWithTailCallSupport(e: Expr)(implicit scope: ValScope): Val = e match {
    case e: IfElse =>
      visitExpr(e.cond) match {
        case Val.True(_)  => visitExprWithTailCallSupport(e.`then`)
        case Val.False(_) =>
          e.`else` match {
            case null => Val.staticNull
            case v    => visitExprWithTailCallSupport(v)
          }
        case v => Error.fail("Need boolean, found " + v.prettyName, e.pos)
      }
    case e: LocalExpr =>
      val bindings = e.bindings
      val s =
        if (bindings == null) scope
        else {
          val base = scope.length
          val newScope = scope.extendBy(bindings.length)
          var i = 0
          while (i < bindings.length) {
            val b = bindings(i)
            newScope.bindings(base + i) = b.args match {
              case null    => visitAsLazy(b.rhs)(newScope)
              case argSpec =>
                if (debugStats != null) debugStats.lazyCreated += 1
                new LazyFunc(() => visitMethod(b.rhs, argSpec, b.pos)(newScope))
            }
            i += 1
          }
          newScope
        }
      visitExprWithTailCallSupport(e.returned)(s)
    case e: AssertExpr =>
      if (!visitExpr(e.asserted.value).isInstanceOf[Val.True]) {
        e.asserted.msg match {
          case null => Error.fail("Assertion failed", e)
          case msg  =>
            Error.fail("Assertion failed: " + materializeError(visitExpr(msg)), e)
        }
      }
      visitExprWithTailCallSupport(e.returned)
    case e: And =>
      // rhs of && is in tail position: when lhs is true, rhs is returned directly.
      // Type check via helper to preserve @tailrec on this method.
      visitExpr(e.lhs) match {
        case _: Val.True  => visitAndRhsTailPos(e.rhs, e.pos)
        case _: Val.False => Val.staticFalse
        case unknown      =>
          Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
      }
    case e: Or =>
      // rhs of || is in tail position: when lhs is false, rhs is returned directly.
      // Type check via helper to preserve @tailrec on this method.
      visitExpr(e.lhs) match {
        case _: Val.True  => Val.staticTrue
        case _: Val.False => visitOrRhsTailPos(e.rhs, e.pos)
        case unknown      =>
          Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
      }
    case e: Expr.Error =>
      Error.fail(materializeError(visitExpr(e.value)), e.pos)
    // Tail-position tailstrict calls: match TailstrictableExpr to unify the tailstrict guard,
    // then dispatch by concrete type.
    //
    // - Apply* (user function calls): construct a TailCall sentinel that the caller's
    //   TailCall.resolve loop will resolve iteratively, avoiding JVM stack growth for
    //   tail-recursive calls.
    // - ApplyBuiltin* (built-in function calls): fall through to visitExpr, which dispatches to
    //   visitApplyBuiltin*. Those methods already wrap their result in TailCall.resolve() when
    //   tailstrict=true, resolving any TailCall that a user-defined callback (e.g. the function
    //   argument to std.makeArray or std.sort) may have returned.
    case e: TailstrictableExpr if e.tailstrict =>
      e match {
        case e: Apply =>
          try {
            val isStrict = e.isStrict
            val func = visitExpr(e.value).cast[Val.Func]
            val args: Array[Eval] =
              if (!isStrict) e.args.map(visitAsLazy(_))
              else e.args.map(visitExpr(_)).asInstanceOf[Array[Eval]]
            new TailCall(func, args, e.namedNames, e, strict = isStrict)
          } catch Error.withStackFrame(e)
        case e: Apply0 =>
          try {
            val func = visitExpr(e.value).cast[Val.Func]
            new TailCall(func, Evaluator.emptyLazyArray, null, e, strict = e.isStrict)
          } catch Error.withStackFrame(e)
        case e: Apply1 =>
          try {
            val isStrict = e.isStrict
            val func = visitExpr(e.value).cast[Val.Func]
            val arg: Eval = if (!isStrict) visitAsLazy(e.a1) else visitExpr(e.a1)
            new TailCall(func, Array[Eval](arg), null, e, strict = isStrict)
          } catch Error.withStackFrame(e)
        case e: Apply2 =>
          try {
            val isStrict = e.isStrict
            val func = visitExpr(e.value).cast[Val.Func]
            val a1: Eval = if (!isStrict) visitAsLazy(e.a1) else visitExpr(e.a1)
            val a2: Eval = if (!isStrict) visitAsLazy(e.a2) else visitExpr(e.a2)
            new TailCall(func, Array[Eval](a1, a2), null, e, strict = isStrict)
          } catch Error.withStackFrame(e)
        case e: Apply3 =>
          try {
            val isStrict = e.isStrict
            val func = visitExpr(e.value).cast[Val.Func]
            val a1: Eval = if (!isStrict) visitAsLazy(e.a1) else visitExpr(e.a1)
            val a2: Eval = if (!isStrict) visitAsLazy(e.a2) else visitExpr(e.a2)
            val a3: Eval = if (!isStrict) visitAsLazy(e.a3) else visitExpr(e.a3)
            new TailCall(func, Array[Eval](a1, a2, a3), null, e, strict = isStrict)
          } catch Error.withStackFrame(e)
        case _ => visitExpr(e)
      }
    case _ =>
      visitExpr(e)
  }

  // Note: can't use LazyExpr here — `scope` is by-name (=> ValScope), must remain lazy.
  def visitBindings(bindings: Array[Bind], scope: => ValScope): Array[Eval] = {
    if (debugStats != null) debugStats.lazyCreated += bindings.length
    val arrF = new Array[Eval](bindings.length)
    var i = 0
    while (i < bindings.length) {
      val b = bindings(i)
      arrF(i) = b.args match {
        case null =>
          new LazyFunc(() => visitExpr(b.rhs)(scope))
        case argSpec =>
          new LazyFunc(() => visitMethod(b.rhs, argSpec, b.pos, b.name)(scope))
      }
      i += 1
    }
    arrF
  }

  def visitMemberList(objPos: Position, e: ObjBody.MemberList, sup: Val.Obj)(implicit
      scope: ValScope): Val.Obj = {
    val asserts = e.asserts
    val fields = e.fields
    val factory = new ObjectScopeFactory(scope, e.binds, this)

    // Trigger an object's own assertions. This defines a closure which is
    // invoked from within Val.Obj; it should not be called directly.
    def triggerAsserts(self: Val.Obj, sup: Val.Obj): Unit = {
      val newScope: ValScope = factory.makeScope(self, sup)
      var i = 0
      while (i < asserts.length) {
        val a = asserts(i)
        if (!visitExpr(a.value)(newScope).isInstanceOf[Val.True]) {
          a.msg match {
            case null => Error.fail("Assertion failed", a.value.pos)
            case msg  =>
              Error.fail(
                "Assertion failed: " + visitExpr(msg)(newScope).cast[Val.Str].str,
                a.value.pos
              )
          }
        }
        i += 1
      }
    }

    // Lazily allocate builder only when we have more than 8 fields,
    // using flat arrays (single-field or inline arrays) for small objects
    var builder: java.util.LinkedHashMap[String, Val.Obj.Member] = null
    // Track inline fields: for 1 field use singleKey/singleMember, for 2-8 use arrays
    var singleKey: String = null
    var singleMember: Val.Obj.Member = null
    var inlineKeys: Array[String] = null
    var inlineMembers: Array[Val.Obj.Member] = null
    var fieldCount = 0
    val maxInlineFields = 8

    // Shared field-tracking logic: manages singleKey → inlineKeys → builder transitions.
    // Handles duplicate key detection at each tier.
    def trackField(k: String, v: Val.Obj.Member, offset: Position): Unit = {
      if (fieldCount == 0) {
        singleKey = k
        singleMember = v
      } else if (fieldCount == 1) {
        // Moving from single-field to multi-field: allocate inline arrays
        inlineKeys = new Array[String](math.min(fields.length, maxInlineFields))
        inlineMembers = new Array[Val.Obj.Member](inlineKeys.length)
        inlineKeys(0) = singleKey
        inlineMembers(0) = singleMember
        if (singleKey.equals(k)) {
          Error.fail(s"Duplicate key $k in evaluated object.", offset)
        }
        inlineKeys(1) = k
        inlineMembers(1) = v
        singleKey = null
        singleMember = null
      } else if (fieldCount <= maxInlineFields && inlineKeys != null) {
        // Check for duplicates in inline array
        var di = 0
        while (di < fieldCount) {
          if (inlineKeys(di).equals(k)) {
            Error.fail(s"Duplicate key $k in evaluated object.", offset)
          }
          di += 1
        }
        if (fieldCount < inlineKeys.length) {
          inlineKeys(fieldCount) = k
          inlineMembers(fieldCount) = v
        } else {
          // Overflow: move all inline fields into LinkedHashMap builder
          builder = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](fields.length)
          var mi = 0
          while (mi < fieldCount) {
            builder.put(inlineKeys(mi), inlineMembers(mi))
            mi += 1
          }
          inlineKeys = null
          inlineMembers = null
          builder.put(k, v)
        }
      } else {
        // Already using builder
        val previousValue = builder.put(k, v)
        if (previousValue != null) {
          Error.fail(s"Duplicate key $k in evaluated object.", offset)
        }
      }
      fieldCount += 1
    }

    fields.foreach {
      case Member.Field(offset, fieldName, plus, null, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if (k != null) {
          trackField(k, new ExprFieldMember(plus, sep, rhs, k, factory), offset)
        }
      case Member.Field(offset, fieldName, false, argSpec, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if (k != null) {
          trackField(k, new MethodFieldMember(sep, rhs, argSpec, offset, k, factory), offset)
        }
      case _ =>
        Error.fail("This case should never be hit", objPos)
    }
    // Compute no-self-ref flag once per MemberList (shared across all objects from same expression).
    // When true, the Materializer can skip cacheFieldValue() during inline iteration, eliminating
    // HashMap allocation overhead for objects with >2 fields.
    val noSelfRef = sup == null && Materializer.computeNoSelfRef(e)
    if (debugStats != null) debugStats.objectsCreated += 1
    factory.cachedObj = if (fieldCount == 1 && singleKey != null) {
      // Single-field object: store key and member inline, avoid LinkedHashMap allocation entirely
      val obj = new Val.Obj(
        objPos,
        null,
        false,
        if (asserts != null) triggerAsserts else null,
        sup,
        singleFieldKey = singleKey,
        singleFieldMember = singleMember
      )
      if (noSelfRef) obj._skipFieldCache = true
      obj
    } else if (inlineKeys != null && fieldCount >= 2) {
      // Multi-field inline object: use flat arrays instead of LinkedHashMap
      val finalKeys =
        if (fieldCount == inlineKeys.length) inlineKeys
        else java.util.Arrays.copyOf(inlineKeys, fieldCount)
      val finalMembers =
        if (fieldCount == inlineMembers.length) inlineMembers
        else java.util.Arrays.copyOf(inlineMembers, fieldCount)
      val obj = new Val.Obj(
        objPos,
        null,
        false,
        if (asserts != null) triggerAsserts else null,
        sup,
        inlineFieldKeys = finalKeys,
        inlineFieldMembers = finalMembers
      )
      // Cache sorted field order on MemberList (shared across all objects from same expression)
      var sortedOrder = e._cachedSortedOrder
      if (sortedOrder == null && sup == null) {
        sortedOrder = Materializer.computeSortedInlineOrder(finalKeys, finalMembers)
        e._cachedSortedOrder = sortedOrder
      }
      if (sortedOrder != null) obj._sortedInlineOrder = sortedOrder
      if (noSelfRef) obj._skipFieldCache = true
      obj
    } else {
      new Val.Obj(
        objPos,
        if (builder != null) builder
        else Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](0),
        false,
        if (asserts != null) triggerAsserts else null,
        sup
      )
    }
    factory.cachedObj
  }

  def visitObjComp(e: ObjBody.ObjComp, sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val binds = e.preLocals ++ e.postLocals
    val compScope: ValScope = scope // .clearSuper
    val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
    val compScopes = visitComp(e.first :: e.rest, Array(compScope))
    if (debugStats != null) debugStats.objectCompIterations += compScopes.length
    for (s <- compScopes) {
      visitExpr(e.key)(s) match {
        case Val.Str(_, k) =>
          val previousValue = builder.put(
            k,
            new Val.Obj.Member(e.plus, Visibility.Normal, deprecatedSkipAsserts = true) {
              def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
                checkStackDepth(e.value.pos, "object comprehension")
                try {
                  lazy val newScope: ValScope = s.extend(newBindings, self, sup)
                  lazy val newBindings = visitBindings(binds, newScope)
                  visitExpr(e.value)(newScope)
                } finally decrementStackDepth()
              }
            }
          )
          if (previousValue != null) {
            Error.fail(s"Duplicate key $k in evaluated object comprehension.", e.pos)
          }
        case Val.Null(_) => // do nothing
        case x           => fieldNameTypeError(x, e.pos)
      }
    }
    if (debugStats != null) debugStats.objectsCreated += 1
    new Val.Obj(e.pos, builder, false, null, sup)
  }

  @tailrec
  final def visitComp(f: List[CompSpec], scopes: Array[ValScope]): Array[ValScope] = f match {
    case (spec @ ForSpec(_, name, expr)) :: rest =>
      val newScopes = collection.mutable.ArrayBuilder.make[ValScope]
      newScopes.sizeHint(math.max(16, scopes.length * 4))
      var i = 0
      while (i < scopes.length) {
        val s = scopes(i)
        visitExpr(expr)(s) match {
          case a: Val.Arr =>
            if (debugStats != null) debugStats.arrayCompIterations += a.length
            val lazyArr = a.asLazyArray
            var j = 0
            while (j < lazyArr.length) {
              newScopes += s.extendSimple(lazyArr(j))
              j += 1
            }
          case r =>
            Error.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              spec
            )
        }
        i += 1
      }
      visitComp(rest, newScopes.result())
    case (spec @ IfSpec(offset, expr)) :: rest =>
      visitComp(
        rest,
        scopes.filter(visitExpr(expr)(_) match {
          case Val.True(_)  => true
          case Val.False(_) => false
          case other        =>
            Error.fail(
              "Condition must be boolean, got " + other.prettyName,
              spec
            )
        })
      )
    case Nil => scopes
  }

  private def compareTypeMismatch(x: Val, y: Val): Nothing =
    Error.fail("Cannot compare " + x.prettyName + " with " + y.prettyName, x.pos)

  // Tuple match keeps the method compact for JIT inlining. Scala 2.13+ pattern matcher lowers
  // this to direct instanceof/checkcast without Tuple2 allocation. The inner array loop uses nested
  // match for the per-element numeric fast path. Error path is extracted to keep the happy path small.
  def compare(x: Val, y: Val): Int = (x, y) match {
    case (x: Val.Num, y: Val.Num) => java.lang.Double.compare(x.asDouble, y.asDouble)
    case (x: Val.Str, y: Val.Str) => Util.compareStringsByCodepoint(x.str, y.str)
    case (x: Val.Arr, y: Val.Arr) =>
      // Use eval(i) to access raw Eval without materializing ConcatViews.
      // Combined with sharedConcatPrefixLength(), this turns O(n) comparison
      // into O(right_len) for patterns like `big_array + [x] < big_array + [y]`.
      val xLen = x.length
      val yLen = y.length
      val len = math.min(xLen, yLen)
      // Fast path: skip shared ConcatView prefix entirely
      var i = x.sharedConcatPrefixLength(y)
      while (i < len) {
        val xe = x.eval(i)
        val ye = y.eval(i)
        // For shared Eval refs that are already-evaluated primitives (Str, Num, Bool,
        // Null), compare(x, x) is trivially 0 — safe to skip. Non-primitive shared
        // refs (Arr, Obj, Func) must still recurse or error, so they fall through.
        if ((xe eq ye) && xe.isInstanceOf[Val] && xe.asInstanceOf[Val].valTag <= Val.TAG_NULL) {
          // Shared primitive Val: self-comparison is always 0
        } else {
          val xi = xe.value
          val yi = ye.value
          // Inline numeric fast path avoids recursive compare() dispatch per element
          val cmp = xi match {
            case xn: Val.Num =>
              yi match {
                case yn: Val.Num => java.lang.Double.compare(xn.asDouble, yn.asDouble)
                case _           => compare(xi, yi)
              }
            case _ => compare(xi, yi)
          }
          if (cmp != 0) return cmp
        }
        i += 1
      }
      Integer.compare(xLen, yLen)
    case (x: Val.Bool, y: Val.Bool) => java.lang.Boolean.compare(x.asBoolean, y.asBoolean)
    case (_: Val.Null, _: Val.Null) => 0
    case _                          => compareTypeMismatch(x, y)
  }

  def equal(x: Val, y: Val): Boolean = (x eq y) || (x match {
    case _: Val.True  => y.isInstanceOf[Val.True]
    case _: Val.False => y.isInstanceOf[Val.False]
    case _: Val.Null  => y.isInstanceOf[Val.Null]
    case x: Val.Str   =>
      y match {
        case y: Val.Str => x.str == y.str
        case _          => false
      }
    case x: Val.Num =>
      y match {
        case y: Val.Num => x.asDouble == y.asDouble
        case _          => false
      }
    case x: Val.Arr =>
      y match {
        case y: Val.Arr =>
          val xlen = x.length
          if (xlen != y.length) return false
          // Skip shared ConcatView prefix — elements are reference-identical
          var i = x.sharedConcatPrefixLength(y)
          while (i < xlen) {
            val xe = x.eval(i)
            val ye = y.eval(i)
            // Skip forcing when Eval references match (shared backing elements)
            if (!(xe eq ye)) {
              if (!equal(xe.value, ye.value)) return false
            } else if (!xe.isInstanceOf[Val]) {
              // Invariant: Eval = Val | Lazy. Val is pure; Lazy may error on force.
              xe.value // Force shared Lazy thunks for error semantics
            }
            i += 1
          }
          true
        case _ => false
      }
    case x: Val.Obj =>
      y match {
        case y: Val.Obj =>
          val k1 = x.visibleKeyNames
          val k2 = y.visibleKeyNames
          val k1len = k1.length
          if (k1len != k2.length) return false
          x.triggerAllAsserts(settings.brokenAssertionLogic)
          y.triggerAllAsserts(settings.brokenAssertionLogic)
          var i = 0
          while (i < k1len) {
            val k = k1(i)
            if (!y.containsKey(k)) return false
            val v1 = x.value(k, emptyMaterializeFileScopePos)
            val v2 = y.value(k, emptyMaterializeFileScopePos)
            if (!equal(v1, v2)) return false
            i += 1
          }
          true
        case _ => false
      }
    case _ => false
  })
}

class NewEvaluator(
    private val r: CachedResolver,
    private val e: String => Option[Expr],
    private val w: Path,
    private val s: Settings,
    private val wa: Evaluator.Logger = null,
    ds: DebugStats = null,
    fc: FormatCache = FormatCache.SharedDefault)
    extends Evaluator(r, e, w, s, wa, ds, fc) {

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = try {
    (e.tag: @switch) match {
      case ExprTags.ValidId       => visitValidId(e.asInstanceOf[ValidId])
      case ExprTags.BinaryOp      => visitBinaryOp(e.asInstanceOf[BinaryOp])
      case ExprTags.Select        => visitSelect(e.asInstanceOf[Select])
      case ExprTags.`Val.Func`    => e.asInstanceOf[Val.Func]
      case ExprTags.`Val.Literal` => e.asInstanceOf[Val.Literal]
      case ExprTags.ApplyBuiltin0 => visitApplyBuiltin0(e.asInstanceOf[ApplyBuiltin0])
      case ExprTags.ApplyBuiltin1 => visitApplyBuiltin1(e.asInstanceOf[ApplyBuiltin1])
      case ExprTags.ApplyBuiltin2 => visitApplyBuiltin2(e.asInstanceOf[ApplyBuiltin2])
      case ExprTags.ApplyBuiltin3 => visitApplyBuiltin3(e.asInstanceOf[ApplyBuiltin3])
      case ExprTags.ApplyBuiltin4 => visitApplyBuiltin4(e.asInstanceOf[ApplyBuiltin4])
      case ExprTags.And           => visitAnd(e.asInstanceOf[And])
      case ExprTags.Or            => visitOr(e.asInstanceOf[Or])
      case ExprTags.UnaryOp       => visitUnaryOp(e.asInstanceOf[UnaryOp])
      case ExprTags.Apply1        => visitApply1(e.asInstanceOf[Apply1])
      case ExprTags.Lookup        => visitLookup(e.asInstanceOf[Lookup])
      case ExprTags.Function      =>
        val f = e.asInstanceOf[Function]
        visitMethod(f.body, f.params, f.pos)
      case ExprTags.LocalExpr            => visitLocalExpr(e.asInstanceOf[LocalExpr])
      case ExprTags.Apply                => visitApply(e.asInstanceOf[Apply])
      case ExprTags.IfElse               => visitIfElse(e.asInstanceOf[IfElse])
      case ExprTags.Apply3               => visitApply3(e.asInstanceOf[Apply3])
      case ExprTags.`ObjBody.MemberList` =>
        val oml = e.asInstanceOf[ObjBody.MemberList]
        visitMemberList(oml.pos, oml, null)
      case ExprTags.Apply2            => visitApply2(e.asInstanceOf[Apply2])
      case ExprTags.AssertExpr        => visitAssert(e.asInstanceOf[AssertExpr])
      case ExprTags.ApplyBuiltin      => visitApplyBuiltin(e.asInstanceOf[ApplyBuiltin])
      case ExprTags.Comp              => visitComp(e.asInstanceOf[Comp])
      case ExprTags.Arr               => visitArr(e.asInstanceOf[Arr])
      case ExprTags.SelectSuper       => visitSelectSuper(e.asInstanceOf[SelectSuper])
      case ExprTags.LookupSuper       => visitLookupSuper(e.asInstanceOf[LookupSuper])
      case ExprTags.InSuper           => visitInSuper(e.asInstanceOf[InSuper])
      case ExprTags.ObjExtend         => visitObjExtend(e.asInstanceOf[ObjExtend])
      case ExprTags.`ObjBody.ObjComp` => visitObjComp(e.asInstanceOf[ObjBody.ObjComp], null)
      case ExprTags.Slice             => visitSlice(e.asInstanceOf[Slice])
      case ExprTags.Import            => visitImport(e.asInstanceOf[Import])
      case ExprTags.Apply0            => visitApply0(e.asInstanceOf[Apply0])
      case ExprTags.ImportStr         => visitImportStr(e.asInstanceOf[ImportStr])
      case ExprTags.ImportBin         => visitImportBin(e.asInstanceOf[ImportBin])
      case ExprTags.Error             => visitError(e.asInstanceOf[Expr.Error])
      case _                          => visitInvalid(e)
    }
  } catch {
    Error.withStackFrame(e)
  }
  // This is only needed for --no-static-errors, otherwise these expression types do not make it past the optimizer
  override def visitInvalid(e: Expr): Nothing = (e.tag: @switch) match {
    case ExprTags.Id =>
      val id = e.asInstanceOf[Id]
      Error.fail("Unknown variable: " + id.name, id.pos)
    case ExprTags.Self =>
      Error.fail("Can't use self outside of an object", e.pos)
    case ExprTags.`$` =>
      Error.fail("Can't use $ outside of an object", e.pos)
    case ExprTags.Super =>
      Error.fail("Can't use super outside of an object", e.pos)
  }
}

object Evaluator {

  implicit class SafeDoubleOps(private val d: Double) extends AnyVal {
    @inline def toSafeLong(pos: Position)(implicit ev: EvalErrorScope): Long = {
      if (d < Val.DOUBLE_MIN_SAFE_INTEGER || d > Val.DOUBLE_MAX_SAFE_INTEGER)
        Error.fail("numeric value outside safe integer range for bitwise operation", pos)
      d.toLong
    }
  }

  /**
   * Logger, used for warnings and trace. The first argument is true if the message is a trace
   * emitted by std.trace
   */
  type Logger = (Boolean, String) => Unit
  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Eval](0)
}

/**
 * Shared scope factory for object field evaluation. Encapsulates the scope-caching logic that was
 * previously embedded in `visitMemberList`'s `makeNewScope` closure. All [[ExprFieldMember]] and
 * [[MethodFieldMember]] instances from the same object literal share one factory, preserving the
 * per-object scope cache.
 */
private[sjsonnet] final class ObjectScopeFactory(
    private val enclosingScope: ValScope,
    private val binds: Array[Expr.Bind],
    private[sjsonnet] val evaluator: Evaluator) {
  private var cachedScope: ValScope = ValScope.empty
  private var cachedScopeSet: Boolean = false
  private[sjsonnet] var cachedObj: Val.Obj = _

  def makeScope(self: Val.Obj, sup: Val.Obj): ValScope = {
    if ((sup eq null) && (self eq cachedObj)) {
      if (cachedScopeSet) cachedScope
      else {
        val s = createScope(self, sup)
        cachedScope = s
        cachedScopeSet = true
        s
      }
    } else createScope(self, sup)
  }

  private def createScope(self: Val.Obj, sup: Val.Obj): ValScope = {
    val scopeLen = enclosingScope.length
    val by = if (binds == null) 2 else 2 + binds.length
    val newScope = enclosingScope.extendBy(by)
    newScope.bindings(scopeLen) = self
    newScope.bindings(scopeLen + 1) = sup
    if (binds != null) {
      val arrF = newScope.bindings
      var i = 0
      var j = scopeLen + 2
      while (i < binds.length) {
        val b = binds(i)
        arrF(j) = b.args match {
          case null =>
            evaluator.visitAsLazy(b.rhs)(newScope)
          case argSpec =>
            if (evaluator.debugStats != null) evaluator.debugStats.lazyCreated += 1
            new LazyFunc(() => evaluator.visitMethod(b.rhs, argSpec, b.pos)(newScope))
        }
        i += 1
        j += 1
      }
    }
    newScope
  }
}

/**
 * Concrete [[Val.Obj.Member]] for expression-body fields (no argSpec). Replaces the anonymous inner
 * class in `visitMemberList`, giving the JIT a single monomorphic type at `invoke` call sites for
 * better inlining.
 */
private[sjsonnet] final class ExprFieldMember(
    add0: Boolean,
    vis0: Visibility,
    private val rhs: Expr,
    private val fieldKey: String,
    private val factory: ObjectScopeFactory)
    extends Val.Obj.Member(add0, vis0) {
  def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
    factory.evaluator.checkStackDepth(rhs.pos, fieldKey)
    try factory.evaluator.visitExpr(rhs)(factory.makeScope(self, sup))
    finally factory.evaluator.decrementStackDepth()
  }
}

/**
 * Concrete [[Val.Obj.Member]] for method-body fields (has argSpec). Same monomorphic-dispatch
 * benefit as [[ExprFieldMember]].
 */
private[sjsonnet] final class MethodFieldMember(
    vis0: Visibility,
    private val rhs: Expr,
    private val argSpec: Expr.Params,
    private val methodPos: Position,
    private val fieldKey: String,
    private val factory: ObjectScopeFactory)
    extends Val.Obj.Member(false, vis0) {
  def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
    factory.evaluator.checkStackDepth(rhs.pos, fieldKey)
    try factory.evaluator.visitMethod(rhs, argSpec, methodPos)(factory.makeScope(self, sup))
    finally factory.evaluator.decrementStackDepth()
  }
}
