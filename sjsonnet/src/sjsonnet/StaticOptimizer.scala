package sjsonnet

import Expr._
import sjsonnet.Expr.ObjBody.{MemberList, ObjComp}

class StaticOptimizer(implicit eval: EvalErrorScope) extends ExprTransform {
  var scope = new java.util.BitSet()

  def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, 0), name), null, args) if(!scope.get(0)) =>
      //println(s"----- std.$name(#${argExprs.length}) call")
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

//    case Id(pos, name) =>
//      if(!scope.get(name) && name != 0)
//        sjsonnet.Error.fail("Unknown variable " + pos.fileScope.indexNames(name) + s": $scope", pos)
//      e

    case LocalExpr(pos, bindings, returned) =>
      nestedBindings(bindings)(rec(e))

    case MemberList(pos, binds, fields, asserts) =>
      nestedBindings(binds)(rec(e))
      //TODO methods

    case Function(pos, params, body) =>
      nestedIndices(params.indices)(rec(e))

    case ObjComp(pos, preLocals, key, value, postLocals, first, rest) =>
      val (f2 :: r2, (pre2, post2, k2, v2)) = compSpecs(first :: rest.toList, { () =>
        nestedBindings(preLocals ++ postLocals) {
          (transformBinds(preLocals), transformBinds(postLocals), transform(key), transform(value))
        }
      })
      if((f2 eq first) && (k2 eq key) && (v2 eq value) && (pre2 eq preLocals) && (post2 eq postLocals) && (r2, rest).zipped.forall(_ eq _)) e
      else ObjComp(pos, pre2, k2, v2, post2, f2.asInstanceOf[ForSpec], r2)

    case Comp(pos, value, first, rest) =>
      val (f2 :: r2, v2) = compSpecs(first :: rest.toList, () => transform(value))
      if((f2 eq first) && (v2 eq value) && (r2, rest).zipped.forall(_ eq _)) e
      else Comp(pos, v2, f2.asInstanceOf[ForSpec], r2.toArray)

    case e => rec(e)
  }

  override protected[this] def transformBind(b: Bind): Bind = {
    val args = b.args
    val rhs = b.rhs
    nestedIndices(if(args == null) null else args.indices) {
      val args2 = transformParams(args)
      val rhs2 = transform(rhs)
      if((args2 eq args) && (rhs2 eq rhs)) b
      else b.copy(args = args2, rhs = rhs2)
    }
  }

  override protected[this] def transformField(f: Member.Field): Member.Field = {
    if(f.args == null) super.transformField(f)
    else nestedIndices(f.args.indices)(super.transformField(f))
  }

  private def compSpecs[T](a: List[CompSpec], value: () => T): (List[CompSpec], T) = a match {
    case (c @ ForSpec(pos, name, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[ForSpec]
      nested {
        scope.set(c2.name)
        val (cs2, value2) = compSpecs(cs, value)
        (c2 :: cs2, value2)
      }
    case (c @ IfSpec(pos, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[CompSpec]
      val (cs2, value2) = compSpecs(cs, value)
      (c2 :: cs2, value2)
    case Nil =>
      (Nil, value())
  }

  private def nested[T](f: => T): T = {
    val oldScope = scope
    scope = scope.clone().asInstanceOf[java.util.BitSet]
    try f finally { scope = oldScope }
  }

  private def nestedBindings[T](a: Array[Bind])(f: => T): T = {
    if(a == null || a.length == 0) f
    else nested {
      a.foreach(b => scope.set(b.name))
      f
    }
  }

  private def nestedIndices[T](a: Array[Int])(f: => T): T = {
    if(a == null || a.length == 0) f
    else nested {
      a.foreach(b => scope.set(b))
      f
    }
  }
}
