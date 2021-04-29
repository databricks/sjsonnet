package sjsonnet

import Expr._
import ScopedExprTransform._

class StaticOptimizer extends ScopedExprTransform {
  def optimize(e: Expr): Expr = transform(e)

  override def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, "std"), name), args, null) if(scope.get("std") == null) =>
      //println(s"----- std.$name(#${args.length}) call")
      Std.functions.getOrElse(name, null) match {
        case f: Val.Builtin =>
          var rargs = transformArr(args)
          val f2 = f.specialize(rargs) match {
            case null => f
            case (f2, a2) => rargs = a2; f2
          }
          val alen = rargs.length
          f2 match {
            case f2: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f2, rargs(0))
            case f2: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f2, rargs(0), rargs(1))
            case _ if f2.params.names.length == alen => Expr.ApplyBuiltin(pos, f2, rargs)
            case _ => rec(e)
          }
        case _ => rec(e)
      }

    case a: Apply => transformApply(a)

    case Select(_, Id(_, "std"), name) if(scope.get("std") == null) =>
      Std.functions.getOrElse(name, null) match {
        case null => rec(e)
        case f => f
      }

    case s: Select =>
      super.transform(s) match {
        case Select(pos, ValidSuper(_, selfIdx), name) =>
          SelectSuper(pos, selfIdx, name)
        case s2 => s2
      }

    case l: Lookup =>
      super.transform(l) match {
        case Lookup(pos, ValidSuper(_, selfIdx), index) =>
          LookupSuper(pos, selfIdx, index)
        case l2 => l2
      }

    case b : BinaryOp =>
      super.transform(b) match {
        case b2 @ BinaryOp(pos, lhs, BinaryOp.OP_in, ValidSuper(_, selfIdx)) =>
          InSuper(pos, lhs, selfIdx)
        case b2 @ BinaryOp(pos, lhs: Val.Str, BinaryOp.OP_%, rhs) =>
          try ApplyBuiltin1(pos, new Format.PartialApplyFmt(lhs.value), rhs)
          catch { case _: Exception => b2 }
        case b2 => b2
      }

    case Id(pos, name) =>
      val v = scope.get(name)
      v match {
        case ScopedVal(v: Val with Expr, _, _) => v
        case ScopedVal(e, _, idx) => ValidId(pos, name, idx)
        case null if name == "std" => Std.Std
        case _ => e
      }

    case Self(pos) =>
      scope.get("self") match {
        case ScopedVal(v, _, idx) if v != null => ValidId(pos, "self", idx)
        case _ => e
      }

    case $(pos) =>
      scope.get("$") match {
        case ScopedVal(v, _, idx) if v != null => ValidId(pos, "$", idx)
        case _ => e
      }

    case a: Arr =>
      super.transform(a) match {
        case a: Arr if a.value.forall(_.isInstanceOf[Val]) =>
          new Val.Arr(a.pos, a.value.map(e => e.asInstanceOf[Val]))
        case other => other
      }

    case m: ObjBody.MemberList =>
      super.transform(m) match {
        case m @ ObjBody.MemberList(pos, binds, fields, asserts) =>
          if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields)
          else m
        case other => other
      }

    case e => super.transform(e)
  }

  object ValidSuper {
    def unapply(s: Super): Option[(Position, Int)] =
      scope.get("self") match {
        case ScopedVal(v, _, idx) if v != null => Some((s.pos, idx))
        case _ => None
      }
  }

  override protected[this] def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      transform(x) match {
        case x2: Val.Str =>
          //println(s"----- Fixing FieldName: "+x2.value)
          FieldName.Fixed(x2.value)
        case x2 if x2 eq x => f
        case x2 => FieldName.Dyn(x2)
      }
    case _ => f
  }

  private def transformApply(a: Apply): Expr = {
    val rargs = transformArr(a.args)
    val rlhs = transform(a.value)
    val rebound = rebindApply(a.pos, rlhs, rargs, a.namedNames) match {
      case null => if((rargs eq a.args) && (rlhs eq a.value)) a else Apply(a.pos, rlhs, rargs, a.namedNames)
      case a => a
    }
    specializeApplyArity(rebound)
  }

  private def specializeApplyArity(a: Apply): Expr = {
    if(a.namedNames != null) a
    else a.args.length match {
      case 0 => Apply0(a.pos, a.value)
      case 1 => Apply1(a.pos, a.value, a.args(0))
      case 2 => Apply2(a.pos, a.value, a.args(0), a.args(1))
      case 3 => Apply3(a.pos, a.value, a.args(0), a.args(1), a.args(2))
      case _ => a
    }
  }

  private def rebindApply(pos: Position, lhs: Expr, args: Array[Expr], names: Array[String]): Apply = lhs match {
      case ValidId(_, name, nameIdx) =>
        scope.get(name) match {
          case ScopedVal(Function(_, params, _), _, _) =>
            rebind(args, names, params) match {
              case null => null
              case newArgs => Apply(pos, lhs, newArgs, null)
            }
          case ScopedVal(Bind(_, _, params, _), _, _) =>
            rebind(args, names, params) match {
              case null => null
              case newArgs => Apply(pos, lhs, newArgs, null)
            }
          case _ => null
        }
      case _ => null
  }

  private def rebind(args: Array[Expr], argNames: Array[String], params: Params): Array[Expr] = {
    if(args.length > params.names.length) return null // too many args
    val positional = if(argNames != null) args.length - argNames.length else args.length
    val target = new Array[Expr](params.names.length)
    System.arraycopy(args, 0, target, 0, positional)
    if(argNames != null) {
      var i = 0
      var j = args.length - argNames.length
      while(i < argNames.length) {
        val pos = params.paramMap.getOrElse(argNames(i), -1)
        if(pos == -1) return null // unknown arg name
        if(target(pos) != null) return null // duplicate arg
        target(pos) = args(j)
        i += 1
        j += 1
      }
    }
    var i = positional
    while(i < target.length) {
      if(target(i) == null) {
        params.defaultExprs(i) match {
          case v: Val with Expr => target(i) = v
          case _ => return null // no default or non-constant
        }
      }
      i += 1
    }
    target
  }
}
