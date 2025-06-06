package sjsonnet

import scala.collection.mutable

import Expr.*
import ScopedExprTransform.*

/**
 * StaticOptimizer performs necessary transformations for the evaluator (assigning ValScope indices)
 * plus additional optimizations (post-order) and static checking (pre-order).
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

  override def transform(_e: Expr): Expr = super.transform(check(_e)) match {
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
      try ApplyBuiltin1(pos, new Format.PartialApplyFmt(lhs.value), rhs, tailstrict = false)
      catch { case _: Exception => b2 }

    case e @ Id(pos, name) =>
      scope.get(name) match {
        case ScopedVal(v: Val with Expr, _, _) => v
        case ScopedVal(_, _, idx)              => ValidId(pos, name, idx)
        case null if name == f"$$std"          => std
        case null if name == "std"             => std
        case null =>
          variableResolver(name) match {
            case Some(v) => v // additional variable resolution
            case None =>
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
      if (binds == null && asserts == null && fields.forall(_.isStatic))
        Val.staticObject(pos, fields, internedStaticFieldSets, internedStrings)
      else m

    case e => e
  }

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
          FieldName.Fixed(x2.value)
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
        case null => null
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
}
