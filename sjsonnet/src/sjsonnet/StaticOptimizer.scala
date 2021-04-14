package sjsonnet

import Expr._

class StaticOptimizer(scopeSize: Int)(implicit eval: EvalErrorScope) extends ScopedExprTransform(scopeSize) {
  //println(s"----- scopeSize: $scopeSize")

  override def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, 0), name), null, args) if(scope(0) == null) =>
      //println(s"----- std.$name(#${args.length}) call")
      Std.functions.getOrElse(name, null) match {
        case f: Val.Builtin =>
          val rargs = transformArr(args)
          val alen = rargs.length
          f match {
            case f: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f, rargs(0))
            case f: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f, rargs(0), rargs(1))
            case _ if f.params.indices.length == alen => Expr.ApplyBuiltin(pos, f, rargs)
            case _ => rec(e)
          }
        case _ => rec(e)
      }

    case Id(pos, name) =>
      val v = scope(name)
      v match {
        case v: Val with Expr =>
          //println(s"----- Id($pos, $name) -> $v")
          v
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
