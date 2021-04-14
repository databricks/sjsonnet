package sjsonnet

import sjsonnet.Expr.ObjBody.{MemberList, ObjComp}
import sjsonnet.Expr._

class ScopedExprTransform(scopeSize: Int) extends ExprTransform {
  var scope = new Array[AnyRef](scopeSize)

  // Marker for Exprs in the scope that should not be used because they need to be evaluated in a different scope
  val dynamicExpr = new Expr { def pos: Position = ??? }

  def transform(e: Expr): Expr = e match {
    case LocalExpr(pos, bindings, returned) =>
      nestedBindings(bindings)(rec(e))

    case MemberList(pos, binds, fields, asserts) =>
      val fields2 = transformGenericArr(fields)(transformFieldNameOnly)
      nestedBindings(binds) {
        val fields3 = transformGenericArr(fields2)(transformFieldNoName)
        val binds2 = transformBinds(binds)
        val asserts2 = transformAsserts(asserts)
        if((binds2 eq binds) && (fields3 eq fields) && (asserts2 eq asserts)) e
        else ObjBody.MemberList(pos, binds2, fields3, asserts2)
      }

    case Function(pos, params, body) =>
      nestedIndices(params.indices)(rec(e))

    case ObjComp(pos, preLocals, key, value, postLocals, first, rest) =>
      val (f2 :: r2, (k2, (pre2, post2, v2))) = compSpecs(first :: rest, { () =>
        (transform(key), nestedBindings(preLocals ++ postLocals) {
          (transformBinds(preLocals), transformBinds(postLocals), transform(value))
        })
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

  protected[this] def transformFieldNameOnly(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val x2 = transformFieldName(x)
    if(x2 eq x) f else f.copy(fieldName = x2)
  }

  protected[this] def transformFieldNoName(f: Member.Field): Member.Field = {
    def g = {
      val y = f.args
      val z = f.rhs
      val y2 = transformParams(y)
      val z2 = transform(z)
      if((y2 eq y) && (z2 eq z)) f else f.copy(args = y2, rhs = z2)
    }
    if(f.args == null) g
    else nestedIndices(f.args.indices)(g)
  }

  override protected[this] def transformField(f: Member.Field): Member.Field = ???

  protected[this] def compSpecs[T](a: List[CompSpec], value: () => T): (List[CompSpec], T) = a match {
    case (c @ ForSpec(pos, name, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[ForSpec]
      nested {
        scope(c2.name) = dynamicExpr
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

  protected[this] def nested[T](f: => T): T = {
    val oldScope = scope
    scope = scope.clone()
    try f finally { scope = oldScope }
  }

  protected[this] def nestedBindings[T](a: Array[Bind])(f: => T): T = {
    if(a == null || a.length == 0) f
    else nested {
      a.foreach(b => scope(b.name) = if(b.args == null) b.rhs else b)
      f
    }
  }

  protected[this] def nestedIndices[T](a: Array[Int])(f: => T): T = {
    if(a == null || a.length == 0) f
    else nested {
      a.foreach(b => scope(b) = dynamicExpr)
      f
    }
  }
}
