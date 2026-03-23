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
        case 0 => Apply0(a.pos, a.value, a.tailstrict)
        case 1 => Apply1(a.pos, a.value, a.args(0), a.tailstrict)
        case 2 => Apply2(a.pos, a.value, a.args(0), a.args(1), a.tailstrict)
        case 3 => Apply3(a.pos, a.value, a.args(0), a.args(1), a.args(2), a.tailstrict)
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
            case n: Val.Num => n.pos = pos; n.asInstanceOf[Expr]
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
}
