package sjsonnet

import Expr._
import ScopedExprTransform._

class StaticOptimizer(rootFileScope: FileScope)(implicit eval: EvalErrorScope)
  extends ScopedExprTransform(rootFileScope) {

  override def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, "std", _), name), args, null) if(scope.get("std") == null) =>
      //println(s"----- std.$name(#${args.length}) call")
      Std.functions.getOrElse(name, null) match {
        case f: Val.Builtin =>
          val rargs = transformArr(args)
          val alen = rargs.length
          f match {
            case f: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f, rargs(0))
            case f: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f, rargs(0), rargs(1))
            case _ if f.params.names.length == alen => Expr.ApplyBuiltin(pos, f, rargs)
            case _ => rec(e)
          }
        case _ => rec(e)
      }

    case Select(_, Id(_, "std", _), name) if(scope.get("std") == null) =>
      Std.functions.getOrElse(name, null) match {
        case null => rec(e)
        case f => f
      }

    case Id(pos, name, _) =>
      val v = scope.get(name)
      v match {
        case ScopedVal(v: Val with Expr, _, _) => v
        case ScopedVal(e, _, idx) => ValidId(pos, idx)
        case null if name == "std" => Std.Std
        case _ => e
      }

    case a: Arr =>
      super.transform(a) match {
        case a: Arr if a.value.forall(_.isInstanceOf[Val]) =>
          new Val.Arr(a.pos, a.value.map(e => new Val.Strict(e.asInstanceOf[Val])))
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
}
