package sjsonnet

import scala.annotation.switch
import scala.collection.mutable

import Expr.*
import Evaluator.SafeDoubleOps
import ScopedExprTransform.*

/**
 * StaticOptimizer performs necessary transformations for the evaluator (assigning ValScope indices)
 * plus additional optimizations (post-order) and static checking (pre-order).
 *
 * The optimizer performs:
 *   - Constant folding for arithmetic (+, -, *, /, %), comparison (<, >, <=, >=, ==, !=), bitwise
 *     (&, ^, |), shift (<<, >>), and unary (!, -, ~, +) operators.
 *   - Branch elimination for if-else with constant conditions.
 *   - Short-circuit elimination for And/Or with constant lhs operands.
 *
 * @param variableResolver
 *   a function that resolves variable names to expressions, only called if the variable is not
 *   found in the scope.
 */
class StaticOptimizer(
    ev: EvalScope,
    variableResolver: String => Option[Expr],
    std: Val.Obj,
    internedStrings: mutable.HashMap[String, String],
    internedStaticFieldSets: mutable.HashMap[
      Val.StaticObjectFieldSet,
      java.util.LinkedHashMap[String, java.lang.Boolean]
    ])
    extends ScopedExprTransform {
  def optimize(e: Expr): Expr = transform(e)

  override def transform(_e: Expr): Expr = {
    // Fast path: fold pure numeric literal chains as raw doubles before the bottom-up transform.
    // This avoids intermediate Val.Num + BinaryOp allocations for chains like `60 * 60 * 24`.
    _e match {
      case _: BinaryOp | _: UnaryOp =>
        val d = tryFoldAsDouble(_e)
        if (!d.isNaN) return Val.Num(_e.pos, d)
      case _ =>
    }
    super.transform(check(_e)) match {
      case a: Apply => transformApply(a)

      case e @ Select(p, obj: Val.Obj, name) if obj.containsKey(name) =>
        try obj.value(name, p)(ev).asInstanceOf[Expr]
        catch { case _: Exception => e }

      case Select(pos, ValidSuper(_, selfIdx), name) =>
        SelectSuper(pos, selfIdx, name)

      case Lookup(pos, ValidSuper(_, selfIdx), index) =>
        LookupSuper(pos, selfIdx, index)

      case BinaryOp(pos, lhs, BinaryOp.OP_in, ValidSuper(_, selfIdx)) =>
        InSuper(pos, lhs, selfIdx)
      case b2 @ BinaryOp(pos, lhs: Val.Str, BinaryOp.OP_%, rhs) =>
        try {
          rhs match {
            case r: Val =>
              val partial = new Format.PartialApplyFmt(lhs.str)
              try partial.evalRhs(r, ev, pos).asInstanceOf[Expr]
              catch {
                case _: Exception =>
                  ApplyBuiltin1(pos, partial, rhs, tailstrict = false)
              }
            case _ =>
              ApplyBuiltin1(pos, new Format.PartialApplyFmt(lhs.str), rhs, tailstrict = false)
          }
        } catch { case _: Exception => b2 }

      case e @ Id(pos, name) =>
        scope.get(name) match {
          case ScopedVal(v: Val with Expr, _, _) => v
          case ScopedVal(_, _, idx)              => ValidId(pos, name, idx)
          case null if name == f"$$std"          => std
          case null if name == "std"             => std
          case null                              =>
            variableResolver(name) match {
              case Some(v) => v // additional variable resolution
              case None    =>
                StaticError.fail(
                  "Unknown variable: " + name,
                  e
                )(ev)
            }
        }

      case e @ Self(pos) =>
        scope.get("self") match {
          case ScopedVal(v, _, idx) if v != null => ValidId(pos, "self", idx)
          case _ => StaticError.fail("Can't use self outside of an object", e)(ev)
        }

      case e @ $(pos) =>
        scope.get("$") match {
          case ScopedVal(v, _, idx) if v != null => ValidId(pos, "$", idx)
          case _ => StaticError.fail("Can't use $ outside of an object", e)(ev)
        }

      case e @ Super(_) if !scope.contains("super") =>
        StaticError.fail("Can't use super outside of an object", e)(ev)

      case a: Arr if a.value.forall(_.isInstanceOf[Val]) =>
        Val.Arr(a.pos, a.value.map(e => e.asInstanceOf[Val]))

      case m @ ObjBody.MemberList(pos, binds, fields, asserts) =>
        // If static optimization has constant-folded originally-dynamic field names
        // into fixed names, it's possible that we might now have duplicate names.
        // In that case, we keep the object as a MemberList and leave it to the
        // Evaluator to throw an error if/when the object is evaluated (in order
        // to preserve proper laziness semantics).
        def allFieldsStaticAndUniquelyNamed: Boolean = {
          val seen = mutable.Set.empty[String]
          fields.forall { f =>
            f.isStatic && seen.add(f.fieldName.asInstanceOf[FieldName.Fixed].value)
          }
        }

        if (binds == null && asserts == null && allFieldsStaticAndUniquelyNamed)
          Val.staticObject(pos, fields, internedStaticFieldSets, internedStrings)
        else if (binds == null && asserts == null && isEagerObjCandidate(fields))
          makeEagerObjBody(pos, fields)
        else m
      // Aggressive optimizations: constant folding, branch elimination, short-circuit elimination.
      // These reduce AST node count at parse time, benefiting long-running Jsonnet programs.
      // Constant folding: BinaryOp with two constant operands (most common case first)
      case e @ BinaryOp(pos, lhs: Val, op, rhs: Val) => tryFoldBinaryOp(pos, lhs, op, rhs, e)

      // Constant folding: UnaryOp with constant operand
      case e @ UnaryOp(pos, op, v: Val) => tryFoldUnaryOp(pos, op, v, e)

      // Branch elimination: constant condition in if-else
      case IfElse(pos, _: Val.True, thenExpr, _) =>
        thenExpr.pos = pos; thenExpr
      case IfElse(pos, _: Val.False, _, elseExpr) =>
        if (elseExpr == null) Val.Null(pos)
        else { elseExpr.pos = pos; elseExpr }

      // Short-circuit elimination for And/Or with constant lhs.
      //
      // IMPORTANT: rhs MUST be guarded as `Val.Bool` — do NOT relax this to arbitrary Expr.
      // The Evaluator's visitAnd/visitOr enforces that rhs evaluates to Bool, throwing
      // "binary operator && does not operate on <type>s" otherwise. If we fold `true && rhs`
      // into just `rhs` without the Bool guard, we silently remove that runtime type check,
      // causing programs like `true && "hello"` to return "hello" instead of erroring.
      // See: Evaluator.visitAnd / Evaluator.visitOr for the authoritative runtime semantics.
      case And(pos, _: Val.True, rhs: Val.Bool) => rhs.pos = pos; rhs
      case And(pos, _: Val.False, _)            => Val.False(pos)
      case Or(pos, _: Val.True, _)              => Val.True(pos)
      case Or(pos, _: Val.False, rhs: Val.Bool) => rhs.pos = pos; rhs
      case e                                    => e
    }
  }

  /**
   * Try to fold a pure constant numeric expression chain as a raw double, bypassing the bottom-up
   * tree transformer. Only handles trees of BinaryOp/UnaryOp/Val.Num with numeric-only ops.
   *
   * Returns `NaN` if the expression cannot be folded (non-numeric leaf, polymorphic op, error).
   * This avoids intermediate `Val.Num` and `BinaryOp` allocations in chains like `60 * 60 * 24`.
   */
  private def tryFoldAsDouble(e: Expr): Double =
    try {
      e match {
        case Val.Num(_, n)               => n
        case BinaryOp(pos, lhs, op, rhs) =>
          val l = tryFoldAsDouble(lhs)
          if (l.isNaN) return Double.NaN
          val r = tryFoldAsDouble(rhs)
          if (r.isNaN) return Double.NaN
          (op: @switch) match {
            case BinaryOp.OP_+ =>
              val res = l + r; if (res.isInfinite) return Double.NaN; res
            case BinaryOp.OP_- =>
              val res = l - r; if (res.isInfinite) return Double.NaN; res
            case BinaryOp.OP_* =>
              val res = l * r; if (res.isInfinite) return Double.NaN; res
            case BinaryOp.OP_/ =>
              if (r == 0) return Double.NaN
              val res = l / r; if (res.isInfinite) return Double.NaN; res
            case BinaryOp.OP_%  => l % r
            case BinaryOp.OP_<< =>
              val ll = l.toSafeLong(pos)(ev); val rr = r.toSafeLong(pos)(ev)
              if (rr < 0) return Double.NaN
              if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr))) return Double.NaN
              (ll << rr).toDouble
            case BinaryOp.OP_>> =>
              val ll = l.toSafeLong(pos)(ev); val rr = r.toSafeLong(pos)(ev)
              if (rr < 0) return Double.NaN
              (ll >> rr).toDouble
            case BinaryOp.OP_& =>
              (l.toSafeLong(pos)(ev) & r.toSafeLong(pos)(ev)).toDouble
            case BinaryOp.OP_^ =>
              (l.toSafeLong(pos)(ev) ^ r.toSafeLong(pos)(ev)).toDouble
            case BinaryOp.OP_| =>
              (l.toSafeLong(pos)(ev) | r.toSafeLong(pos)(ev)).toDouble
            case _ => Double.NaN // non-numeric op (comparison, equality, etc.)
          }
        case UnaryOp(pos, op, v) =>
          val d = tryFoldAsDouble(v)
          if (d.isNaN) return Double.NaN
          (op: @switch) match {
            case Expr.UnaryOp.OP_- => -d
            case Expr.UnaryOp.OP_+ => d
            case Expr.UnaryOp.OP_~ => (~d.toSafeLong(pos)(ev)).toDouble
            case _                 => Double.NaN
          }
        case _ => Double.NaN
      }
    } catch { case _: Exception => Double.NaN }

  private object ValidSuper {
    def unapply(s: Super): Option[(Position, Int)] =
      scope.get("self") match {
        case ScopedVal(v, _, idx) if v != null => Some((s.pos, idx))
        case _                                 => None
      }
  }

  private def check(e: Expr): Expr = {
    e match {
      case ObjExtend(_, base, _) if ev.settings.strict && isObjLiteral(base) =>
        StaticError.fail(
          "Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects",
          e
        )(ev)
      case _ =>
    }
    e
  }

  private def isObjLiteral(expr: Expr): Boolean = expr match {
    case _: ObjBody.MemberList => true
    case _: ObjBody.ObjComp    => true
    case _: ObjExtend          => true
    case _: Val.Obj            => true
    case _                     => false
  }

  override protected def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      transform(x) match {
        case x2: Val.Str =>
          // println(s"----- Fixing FieldName: "+x2.value)
          FieldName.Fixed(x2.str)
        case x2 if x2 eq x => f
        case x2            => FieldName.Dyn(x2)
      }
    case _ => f
  }

  private def transformApply(a: Apply): Expr = {
    val rebound = rebindApply(a.pos, a.value, a.args, a.namedNames, a.tailstrict) match {
      case null => a
      case a    => a
    }
    rebound match {
      case a2: Apply => specializeApplyArity(a2)
      case e         => e
    }
  }

  private def tryStaticApply(
      pos: Position,
      f: Val.Builtin,
      args: Array[Expr],
      tailstrict: Boolean): Expr = {
    if (f.staticSafe && args.forall(_.isInstanceOf[Val])) {
      val vargs = args.map(_.asInstanceOf[Val])
      val tailstrictMode = if (tailstrict) TailstrictModeEnabled else TailstrictModeDisabled
      try f.apply(vargs, null, pos)(ev, tailstrictMode).asInstanceOf[Expr]
      catch { case _: Exception => null }
    } else null
  }

  private def specializeApplyArity(a: Apply): Expr = {
    if (a.namedNames != null) a
    else
      a.args.length match {
        case 0 => Apply0(a.pos, a.value, a.tailstrict, a.strict)
        case 1 => Apply1(a.pos, a.value, a.args(0), a.tailstrict, a.strict)
        case 2 => Apply2(a.pos, a.value, a.args(0), a.args(1), a.tailstrict, a.strict)
        case 3 =>
          Apply3(a.pos, a.value, a.args(0), a.args(1), a.args(2), a.tailstrict, a.strict)
        case _ => a
      }
  }

  private def rebindApply(
      pos: Position,
      lhs: Expr,
      args: Array[Expr],
      names: Array[String],
      tailstrict: Boolean): Expr = lhs match {
    case f: Val.Builtin =>
      rebind(args, names, f.params) match {
        case null    => null
        case newArgs =>
          tryStaticApply(pos, f, newArgs, tailstrict) match {
            case null =>
              val (f2, rargs) = f.specialize(newArgs, tailstrict) match {
                case null     => (f, newArgs)
                case (f2, a2) => (f2, a2)
              }
              val alen = rargs.length
              f2 match {
                case f2: Val.Builtin0 if alen == 0 => Expr.ApplyBuiltin0(pos, f2, tailstrict)
                case f2: Val.Builtin1 if alen == 1 =>
                  Expr.ApplyBuiltin1(pos, f2, rargs(0), tailstrict)
                case f2: Val.Builtin2 if alen == 2 =>
                  Expr.ApplyBuiltin2(pos, f2, rargs(0), rargs(1), tailstrict)
                case f2: Val.Builtin3 if alen == 3 =>
                  Expr.ApplyBuiltin3(pos, f2, rargs(0), rargs(1), rargs(2), tailstrict)
                case f2: Val.Builtin4 if alen == 4 =>
                  Expr.ApplyBuiltin4(pos, f2, rargs(0), rargs(1), rargs(2), rargs(3), tailstrict)
                case _ if f2.params.names.length == alen =>
                  Expr.ApplyBuiltin(pos, f2, rargs, tailstrict)
                case _ => null
              }
            case e => e
          }
      }

    case ValidId(_, name, _) =>
      scope.get(name) match {
        case ScopedVal(Function(_, params, _), _, _) =>
          rebind(args, names, params) match {
            case null    => null
            case newArgs => Apply(pos, lhs, newArgs, null, tailstrict)
          }
        case ScopedVal(Bind(_, _, params, _), _, _) =>
          rebind(args, names, params) match {
            case null    => null
            case newArgs => Apply(pos, lhs, newArgs, null, tailstrict)
          }
        case _ => null
      }

    case _ => null
  }

  private def rebind(args: Array[Expr], argNames: Array[String], params: Params): Array[Expr] = {
    if (args.length == params.names.length && argNames == null) return args
    if (args.length > params.names.length) return null // too many args
    val positional = if (argNames != null) args.length - argNames.length else args.length
    val target = new Array[Expr](params.names.length)
    System.arraycopy(args, 0, target, 0, positional)
    if (argNames != null) {
      var i = 0
      var j = args.length - argNames.length
      while (i < argNames.length) {
        val pos = params.paramMap.getOrElse(argNames(i), -1)
        if (pos == -1) return null // unknown arg name
        if (target(pos) != null) return null // duplicate arg
        target(pos) = args(j)
        i += 1
        j += 1
      }
    }
    var i = positional
    while (i < target.length) {
      if (target(i) == null) {
        params.defaultExprs(i) match {
          case v: Val with Expr => target(i) = v
          case _                => return null // no default or non-constant
        }
      }
      i += 1
    }
    target
  }

  private def tryFoldUnaryOp(pos: Position, op: Int, v: Val, fallback: Expr): Expr =
    try {
      (op: @switch) match {
        case Expr.UnaryOp.OP_! =>
          v match {
            case _: Val.True  => Val.False(pos)
            case _: Val.False => Val.True(pos)
            case _            => fallback
          }
        case Expr.UnaryOp.OP_- =>
          v match {
            case Val.Num(_, n) => Val.Num(pos, -n)
            case _             => fallback
          }
        case Expr.UnaryOp.OP_~ =>
          v match {
            case n: Val.Num => Val.Num(pos, (~n.asSafeLong).toDouble)
            case _          => fallback
          }
        case Expr.UnaryOp.OP_+ =>
          v match {
            case n: Val.Num => Val.Num(pos, n.asDouble)
            case _          => fallback
          }
        case _ => fallback
      }
    } catch { case _: Exception => fallback }

  private def tryFoldBinaryOp(pos: Position, lhs: Val, op: Int, rhs: Val, fallback: Expr): Expr =
    try {
      (op: @switch) match {
        case BinaryOp.OP_+ =>
          (lhs, rhs) match {
            case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l + r)
            case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
            case (l: Val.Arr, r: Val.Arr)       => l.concat(pos, r)
            case _                              => fallback
          }
        case BinaryOp.OP_- =>
          (lhs, rhs) match {
            case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l - r)
            case _                              => fallback
          }
        case BinaryOp.OP_* =>
          (lhs, rhs) match {
            case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l * r)
            case _                              => fallback
          }
        case BinaryOp.OP_/ =>
          (lhs, rhs) match {
            case (Val.Num(_, l), Val.Num(_, r)) if r != 0 => Val.Num(pos, l / r)
            case _                                        => fallback
          }
        case BinaryOp.OP_% =>
          (lhs, rhs) match {
            case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l % r)
            case _                              => fallback
          }
        case BinaryOp.OP_< =>
          tryFoldComparison(pos, lhs, BinaryOp.OP_<, rhs, fallback)
        case BinaryOp.OP_> =>
          tryFoldComparison(pos, lhs, BinaryOp.OP_>, rhs, fallback)
        case BinaryOp.OP_<= =>
          tryFoldComparison(pos, lhs, BinaryOp.OP_<=, rhs, fallback)
        case BinaryOp.OP_>= =>
          tryFoldComparison(pos, lhs, BinaryOp.OP_>=, rhs, fallback)
        case BinaryOp.OP_== =>
          tryFoldEquality(pos, lhs, rhs, negate = false, fallback)
        case BinaryOp.OP_!= =>
          tryFoldEquality(pos, lhs, rhs, negate = true, fallback)
        case BinaryOp.OP_in =>
          (lhs, rhs) match {
            case (Val.Str(_, l), o: Val.Obj) => Val.bool(pos, o.containsKey(l))
            case _                           => fallback
          }
        case BinaryOp.OP_<< =>
          (lhs, rhs) match {
            case (l: Val.Num, r: Val.Num) =>
              val ll = l.asSafeLong
              val rr = r.asSafeLong
              if (rr < 0) fallback // negative shift → runtime error
              else if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr)))
                fallback // overflow → runtime error
              else Val.Num(pos, (ll << rr).toDouble)
            case _ => fallback
          }
        case BinaryOp.OP_>> =>
          (lhs, rhs) match {
            case (l: Val.Num, r: Val.Num) =>
              val ll = l.asSafeLong
              val rr = r.asSafeLong
              if (rr < 0) fallback // negative shift → runtime error
              else Val.Num(pos, (ll >> rr).toDouble)
            case _ => fallback
          }
        case BinaryOp.OP_& =>
          (lhs, rhs) match {
            case (l: Val.Num, r: Val.Num) =>
              Val.Num(pos, (l.asSafeLong & r.asSafeLong).toDouble)
            case _ => fallback
          }
        case BinaryOp.OP_^ =>
          (lhs, rhs) match {
            case (l: Val.Num, r: Val.Num) =>
              Val.Num(pos, (l.asSafeLong ^ r.asSafeLong).toDouble)
            case _ => fallback
          }
        case BinaryOp.OP_| =>
          (lhs, rhs) match {
            case (l: Val.Num, r: Val.Num) =>
              Val.Num(pos, (l.asSafeLong | r.asSafeLong).toDouble)
            case _ => fallback
          }
        case _ => fallback
      }
    } catch { case _: Exception => fallback }

  private def tryFoldComparison(
      pos: Position,
      lhs: Val,
      op: Int,
      rhs: Val,
      fallback: Expr): Expr = {
    // Use IEEE 754 operators directly for Num, not java.lang.Double.compare,
    // because compare(-0.0, 0.0) == -1 while IEEE 754 treats -0.0 == 0.0.
    (lhs, rhs) match {
      case (Val.Num(_, l), Val.Num(_, r)) if !l.isNaN && !r.isNaN =>
        val result = (op: @switch) match {
          case BinaryOp.OP_<  => l < r
          case BinaryOp.OP_>  => l > r
          case BinaryOp.OP_<= => l <= r
          case BinaryOp.OP_>= => l >= r
          case _              => return fallback
        }
        Val.bool(pos, result)
      case (Val.Str(_, l), Val.Str(_, r)) =>
        val cmp = Util.compareStringsByCodepoint(l, r)
        val result = (op: @switch) match {
          case BinaryOp.OP_<  => cmp < 0
          case BinaryOp.OP_>  => cmp > 0
          case BinaryOp.OP_<= => cmp <= 0
          case BinaryOp.OP_>= => cmp >= 0
          case _              => return fallback
        }
        Val.bool(pos, result)
      case _ => fallback
    }
  }

  private def tryFoldEquality(
      pos: Position,
      lhs: Val,
      rhs: Val,
      negate: Boolean,
      fallback: Expr): Expr = {
    def isSimpleLiteral(v: Val): Boolean = v match {
      case _: Val.Bool | _: Val.Null | _: Val.Str | _: Val.Num => true
      case _                                                   => false
    }
    if (!isSimpleLiteral(lhs) || !isSimpleLiteral(rhs)) return fallback
    val result = (lhs, rhs) match {
      case (_: Val.True, _: Val.True) | (_: Val.False, _: Val.False) | (_: Val.Null, _: Val.Null) =>
        true
      case (Val.Num(_, l), Val.Num(_, r)) if !l.isNaN && !r.isNaN =>
        l == r
      case (Val.Str(_, l), Val.Str(_, r)) =>
        l == r
      case _ => false // different simple types are never equal
    }
    Val.bool(pos, if (negate) !result else result)
  }

  /**
   * Auto-TCO: override transformBind to detect self-recursive tail calls in function bodies and
   * mark them as `tailstrict = true`. This enables the evaluator's TailCall trampoline for those
   * calls, preventing JVM stack overflow on deep recursion — without requiring the user to annotate
   * every recursive call site with the `tailstrict` keyword.
   *
   * Safety: we only mark a call as tailstrict when the call provides exactly as many positional
   * arguments as the function declares parameters (no named args). This ensures the evaluator takes
   * the "simple" apply path where only the already-evaluated passed arguments are forced, avoiding
   * the eager evaluation of default argument expressions that `tailstrict` would otherwise trigger.
   *
   * Handles both binding forms:
   *   - `local sum(n, acc) = ...` → Bind(args = Params(...), rhs = body)
   *   - `local sum = function(n, acc) ...` → Bind(args = null, rhs = Function(params, body))
   *
   * @see
   *   [[TailCall]] for the sentinel value used in the TCO protocol
   * @see
   *   https://github.com/databricks/sjsonnet/issues/623
   */
  override def transformBind(b: Bind): Bind = {
    val b2 = super.transformBind(b)
    val sv = scope.get(b2.name)
    if (sv == null) return b2

    if (b2.args != null) {
      // Direct function binding: local sum(n, acc) = body
      // Only auto-TCO if the function has at least one non-recursive exit path.
      // This prevents turning trivially infinite recursions (e.g. `f(x) = f(x)`) into
      // infinite trampoline loops; without a base case the TailCall trampoline would never
      // produce a non-TailCall result.
      if (!hasNonRecursiveExit(b2.rhs, b2.name, sv.idx, b2.args.names.length)) return b2
      val newRhs = markTailCalls(b2.rhs, b2.name, sv.idx, b2.args.names.length)
      if (newRhs ne b2.rhs) b2.copy(rhs = newRhs) else b2
    } else
      b2.rhs match {
        case f: Function =>
          // Function literal binding: local sum = function(n, acc) body
          if (!hasNonRecursiveExit(f.body, b2.name, sv.idx, f.params.names.length)) return b2
          val newBody = markTailCalls(f.body, b2.name, sv.idx, f.params.names.length)
          if (newBody ne f.body) b2.copy(rhs = f.copy(body = newBody)) else b2
        case _ => b2
      }
  }

  /**
   * Check whether a function body has at least one code path that does NOT end in a self-recursive
   * tail call. Functions with no non-recursive exit (e.g. `f(x) = f(x)`) should NOT be auto-TCO'd
   * because the TailCall trampoline would loop forever — the function never produces a base-case
   * result.
   *
   * The analysis mirrors `markTailCalls` traversal: it propagates through IfElse, LocalExpr,
   * AssertExpr, And/Or (rhs only), and Expr.Error (value) — the same positions where tail calls are
   * detected, returning `true` as soon as any leaf expression is found that is NOT a self-recursive
   * call.
   */
  private def hasNonRecursiveExit(
      body: Expr,
      selfName: String,
      selfIdx: Int,
      paramCount: Int
  ): Boolean = {
    def isSelfTailCall(e: Expr): Boolean = e match {
      case a: Apply0 =>
        a.value match {
          case ValidId(_, n, i) => n == selfName && i == selfIdx && paramCount == 0
          case _                => false
        }
      case a: Apply1 =>
        a.value match {
          case ValidId(_, n, i) => n == selfName && i == selfIdx && paramCount == 1
          case _                => false
        }
      case a: Apply2 =>
        a.value match {
          case ValidId(_, n, i) => n == selfName && i == selfIdx && paramCount == 2
          case _                => false
        }
      case a: Apply3 =>
        a.value match {
          case ValidId(_, n, i) => n == selfName && i == selfIdx && paramCount == 3
          case _                => false
        }
      case a: Apply =>
        a.value match {
          case ValidId(_, n, i) =>
            n == selfName && i == selfIdx && a.namedNames == null && a.args.length == paramCount
          case _ => false
        }
      case _ => false
    }

    body match {
      case e: IfElse =>
        // Either branch being non-recursive is sufficient
        hasNonRecursiveExit(e.`then`, selfName, selfIdx, paramCount) ||
        (e.`else` != null && hasNonRecursiveExit(e.`else`, selfName, selfIdx, paramCount)) ||
        e.`else` == null // missing else returns Val.Null when condition is false — a non-recursive exit
      case e: LocalExpr =>
        hasNonRecursiveExit(e.returned, selfName, selfIdx, paramCount)
      case e: AssertExpr =>
        hasNonRecursiveExit(e.returned, selfName, selfIdx, paramCount)
      case e: And =>
        // rhs of && is in tail position (when lhs is true, rhs result is returned directly).
        // lhs is NOT in tail position, but provides a control-flow exit path (returns false
        // when lhs is false). We must still check rhs for non-recursive exits to prevent
        // auto-TCO on functions with no base case (e.g. `f() = true && f()`).
        hasNonRecursiveExit(e.rhs, selfName, selfIdx, paramCount)
      case e: Or =>
        // rhs of || is in tail position (when lhs is false, rhs result is returned directly).
        // lhs is NOT in tail position, but provides a control-flow exit path (returns true
        // when lhs is true). We must still check rhs for non-recursive exits.
        hasNonRecursiveExit(e.rhs, selfName, selfIdx, paramCount)
      case _: Expr.Error =>
        // error value is the last thing evaluated before throwing → non-recursive exit
        true
      case e if isSelfTailCall(e) =>
        false // this path IS a self-recursive tail call → not a non-recursive exit
      case _ =>
        true // any other expression (literal, non-self call, binary op, etc.) is a base case
    }
  }

  /**
   * Check if a MemberList's fields are eligible for EagerObjBody transformation: all field names
   * are Fixed, no plus, no argSpec, field values don't reference self/super, unique field names,
   * AND all field value expressions are "eager-safe" (provably total — can't error or diverge).
   *
   * The purity guard ensures that eager evaluation produces the same observable behavior as lazy
   * evaluation, preserving Jsonnet's lazy field semantics.
   */
  private def isEagerObjCandidate(fields: Array[Expr.Member.Field]): Boolean = {
    // scope.size is the outer scope size (self/super were added then restored by nestedObject)
    val selfIdx = scope.size
    val seen = mutable.Set.empty[String]
    fields.forall { f =>
      f.fieldName.isInstanceOf[FieldName.Fixed] &&
      !f.plus &&
      f.args == null &&
      f.sep == Expr.Member.Visibility.Normal &&
      seen.add(f.fieldName.asInstanceOf[FieldName.Fixed].value) &&
      !exprReferencesScopeAtOrAbove(f.rhs, selfIdx) &&
      isEagerSafeExpr(f.rhs)
    }
  }

  /**
   * Walk an expression tree looking for self-recursive calls in tail position. A call is in tail
   * position if it is the last expression evaluated before the function returns — i.e. its result
   * becomes the function's return value without further transformation.
   *
   * Tail position propagates through:
   *   - Both branches of `if-else`
   *   - The `returned` expression of `local ... ; returned`
   *   - The `returned` expression of `assert ... ; returned`
   *   - The `rhs` of `lhs && rhs` (when lhs is true, rhs is the result)
   *   - The `rhs` of `lhs || rhs` (when lhs is false, rhs is the result)
   *   - The `value` of `error value` (last thing evaluated before throwing)
   *
   * This matches the evaluator's `visitExprWithTailCallSupport` propagation rules exactly.
   *
   * @param body
   *   the expression to scan (already transformed by the base optimizer)
   * @param selfName
   *   the name of the function being defined
   * @param selfIdx
   *   the ValScope index of the function binding
   * @param paramCount
   *   the number of parameters the function declares; only calls with exactly this many positional
   *   args are marked (avoids forcing default arg expressions)
   * @return
   *   the expression with matching tail calls marked `tailstrict = true`, or `body` unchanged if no
   *   matches found (reference equality preserved)
   */
  private def markTailCalls(body: Expr, selfName: String, selfIdx: Int, paramCount: Int): Expr = {
    def isSelfCall(value: Expr, callArity: Int): Boolean = value match {
      case ValidId(_, name, idx) => name == selfName && idx == selfIdx && callArity == paramCount
      case _                     => false
    }

    body match {
      case e: IfElse =>
        val t = markTailCalls(e.`then`, selfName, selfIdx, paramCount)
        val el =
          if (e.`else` != null) markTailCalls(e.`else`, selfName, selfIdx, paramCount) else null
        if ((t eq e.`then`) && (el eq e.`else`)) body
        else IfElse(e.pos, e.cond, t, el)

      case e: LocalExpr =>
        val ret = markTailCalls(e.returned, selfName, selfIdx, paramCount)
        if (ret eq e.returned) body
        else LocalExpr(e.pos, e.bindings, ret)

      case e: AssertExpr =>
        val ret = markTailCalls(e.returned, selfName, selfIdx, paramCount)
        if (ret eq e.returned) body
        else AssertExpr(e.pos, e.asserted, ret)

      case e: And =>
        // rhs of && is in tail position: when lhs evaluates to true, rhs is returned directly
        val rhs2 = markTailCalls(e.rhs, selfName, selfIdx, paramCount)
        if (rhs2 eq e.rhs) body
        else And(e.pos, e.lhs, rhs2)

      case e: Or =>
        // rhs of || is in tail position: when lhs evaluates to false, rhs is returned directly
        val rhs2 = markTailCalls(e.rhs, selfName, selfIdx, paramCount)
        if (rhs2 eq e.rhs) body
        else Or(e.pos, e.lhs, rhs2)

      case e: Expr.Error =>
        // error value is in tail position (last thing evaluated before throwing)
        val v = markTailCalls(e.value, selfName, selfIdx, paramCount)
        if (v eq e.value) body
        else Expr.Error(e.pos, v)

      // Self-recursive tail calls: mark as tailstrict + strict=false to enable the TailCall trampoline
      // while preserving lazy argument semantics (auto-TCO uses TailstrictModeAutoTCO, not
      // TailstrictModeEnabled, so arguments are not eagerly forced).
      // Only match when call arity == param count (no named args, no default args involved).
      case a: Apply0 if !a.tailstrict && isSelfCall(a.value, 0) =>
        Apply0(a.pos, a.value, tailstrict = true, strict = false)
      case a: Apply1 if !a.tailstrict && isSelfCall(a.value, 1) =>
        Apply1(a.pos, a.value, a.a1, tailstrict = true, strict = false)
      case a: Apply2 if !a.tailstrict && isSelfCall(a.value, 2) =>
        Apply2(a.pos, a.value, a.a1, a.a2, tailstrict = true, strict = false)
      case a: Apply3 if !a.tailstrict && isSelfCall(a.value, 3) =>
        Apply3(a.pos, a.value, a.a1, a.a2, a.a3, tailstrict = true, strict = false)
      case a: Apply
          if !a.tailstrict && a.namedNames == null && isSelfCall(a.value, a.args.length) =>
        Apply(a.pos, a.value, a.args, null, tailstrict = true, strict = false)

      case _ => body
    }
  }

  /**
   * Check if an expression is "eager-safe": evaluating it eagerly produces the same result as
   * evaluating it lazily. This requires the expression to be provably total (always terminates
   * without error). Allowed: literals, variable references, already-optimized EagerObjBody/static
   * objects. Disallowed: function calls, error, assert, binary ops that can fail, etc.
   */
  private def isEagerSafeExpr(e: Expr): Boolean = e match {
    case _: Val.Literal => true // constants — already a value
    case _: ValidId     => true // variable reference — scope binding exists (optimizer validated)
    case _: ObjBody.EagerObjBody => true // recursively already verified
    case _                       => false
  }

  /** Check if an expression tree contains any ValidId with nameIdx >= threshold. */
  private def exprReferencesScopeAtOrAbove(e: Expr, threshold: Int): Boolean = {
    e match {
      case ValidId(_, _, idx) => idx >= threshold
      case _                  =>
        var found = false
        val checker = new ExprTransform {
          def transform(expr: Expr): Expr = {
            if (found) return expr
            expr match {
              case ValidId(_, _, idx) if idx >= threshold =>
                found = true; expr
              case _ => rec(expr)
            }
          }
        }
        checker.transform(e)
        found
    }
  }

  /** Convert eligible MemberList fields into an EagerObjBody node. */
  private def makeEagerObjBody(
      pos: Position,
      fields: Array[Expr.Member.Field]): ObjBody.EagerObjBody = {
    val n = fields.length
    val names = new Array[String](n)
    val values = new Array[Expr](n)
    var i = 0
    while (i < n) {
      val f = fields(i)
      names(i) = internedStrings.getOrElseUpdate(
        f.fieldName.asInstanceOf[FieldName.Fixed].value,
        f.fieldName.asInstanceOf[FieldName.Fixed].value
      )
      values(i) = f.rhs
      i += 1
    }
    ObjBody.EagerObjBody(pos, names, values)
  }
}
