package sjsonnet

import Expr._

import scala.annotation.switch

/** Simple tree transformer for the AST. */
abstract class ExprTransform {

  def transform(expr: Expr): Expr

  def rec(expr: Expr): Expr = {
    if (expr == null) null else (expr._tag: @switch) match {
      case ExprTags.Select =>
        val Select(pos, x, name) = expr.asInstanceOf[Select]
        val x2 = transform(x)
        if(x2 eq x) expr
        else Select(pos, x2, name)

      case ExprTags.Apply =>
        val Apply(pos, x, y, namedNames, tailstrict) = expr.asInstanceOf[Apply]
        val x2 = transform(x)
        val y2 = transformArr(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Apply(pos, x2, y2, namedNames, tailstrict)

      case ExprTags.Apply0 =>
        val Apply0(pos, x, tailstrict) = expr.asInstanceOf[Apply0]
        val x2 = transform(x)
        if((x2 eq x)) expr
        else Apply0(pos, x2, tailstrict)

      case ExprTags.Apply1 =>
        val Apply1(pos, x, y, tailstrict) = expr.asInstanceOf[Apply1]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Apply1(pos, x2, y2, tailstrict)

      case ExprTags.Apply2 =>
        val Apply2(pos, x, y, z, tailstrict) = expr.asInstanceOf[Apply2]
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else Apply2(pos, x2, y2, z2, tailstrict)

      case ExprTags.Apply3 =>
        val Apply3(pos, x, y, z, a, tailstrict) = expr.asInstanceOf[Apply3]
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        val a2 = transform(a)
        if((x2 eq x) && (y2 eq y) && (z2 eq z) && (a2 eq a)) expr
        else Apply3(pos, x2, y2, z2, a2, tailstrict)

      case ExprTags.ApplyBuiltin =>
        val ApplyBuiltin(pos, func, x, tailstrict) = expr.asInstanceOf[ApplyBuiltin]
        val x2 = transformArr(x)
        if(x2 eq x) expr
        else ApplyBuiltin(pos, func, x2, tailstrict)

      case ExprTags.ApplyBuiltin1 =>
        val ApplyBuiltin1(pos, func, x, tailstrict) = expr.asInstanceOf[ApplyBuiltin1]
        val x2 = transform(x)
        if(x2 eq x) expr
        else ApplyBuiltin1(pos, func, x2, tailstrict)

      case ExprTags.ApplyBuiltin2 =>
        val ApplyBuiltin2(pos, func, x, y, tailstrict) = expr.asInstanceOf[ApplyBuiltin2]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else ApplyBuiltin2(pos, func, x2, y2, tailstrict)

      case ExprTags.ApplyBuiltin3 =>
        val ApplyBuiltin3(pos, func, x, y, z, tailstrict) = expr.asInstanceOf[ApplyBuiltin3]
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else ApplyBuiltin3(pos, func, x2, y2, z2, tailstrict)

      case ExprTags.ApplyBuiltin4 =>
        val ApplyBuiltin4(pos, func, x, y, z, a, tailstrict) = expr.asInstanceOf[ApplyBuiltin4]
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        val a2 = transform(a)
        if((x2 eq x) && (y2 eq y) && (z2 eq z) && (a2 eq a)) expr
        else ApplyBuiltin4(pos, func, x2, y2, z2, a2, tailstrict)

      case ExprTags.UnaryOp =>
        val UnaryOp(pos, op, x) = expr.asInstanceOf[UnaryOp]
        val x2 = transform(x)
        if(x2 eq x) expr
        else UnaryOp(pos, op, x2)

      case ExprTags.BinaryOp =>
        val BinaryOp(pos, x, op, y) = expr.asInstanceOf[BinaryOp]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else BinaryOp(pos, x2, op, y2)

      case ExprTags.And =>
        val And(pos, x, y) = expr.asInstanceOf[And]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else And(pos, x2, y2)

      case ExprTags.Or =>
        val Or(pos, x, y) = expr.asInstanceOf[Or]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Or(pos, x2, y2)

      case ExprTags.InSuper =>
        val InSuper(pos, x, selfIdx) = expr.asInstanceOf[InSuper]
        val x2 = transform(x)
        if(x2 eq x) expr
        else InSuper(pos, x2, selfIdx)

      case ExprTags.Lookup =>
        val Lookup(pos, x, y) = expr.asInstanceOf[Lookup]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Lookup(pos, x2, y2)

      case ExprTags.LookupSuper =>
        val LookupSuper(pos, selfIdx, x) = expr.asInstanceOf[LookupSuper]
        val x2 = transform(x)
        if(x2 eq x) expr
        else LookupSuper(pos, selfIdx, x2)

      case ExprTags.Function =>
        val Function(pos, x, y) = expr.asInstanceOf[Function]
        val x2 = transformParams(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Function(pos, x2, y2)

      case ExprTags.LocalExpr =>
        val LocalExpr(pos, x, y) = expr.asInstanceOf[LocalExpr]
        val x2 = transformBinds(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else LocalExpr(pos, x2, y2)

      case ExprTags.IfElse =>
        val IfElse(pos, x, y, z) = expr.asInstanceOf[IfElse]
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else IfElse(pos, x2, y2, z2)

      case ExprTags.`ObjBody.MemberList` =>
        val ObjBody.MemberList(pos, x, y, z) = expr.asInstanceOf[ObjBody.MemberList]
        val x2 = transformBinds(x)
        val y2 = transformFields(y)
        val z2 = transformAsserts(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else ObjBody.MemberList(pos, x2, y2, z2)

      case ExprTags.AssertExpr =>
        val AssertExpr(pos, x, y) = expr.asInstanceOf[AssertExpr]
        val x2 = transformAssert(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else AssertExpr(pos, x2, y2)

      case ExprTags.Comp =>
        val Comp(pos, x, y, z) = expr.asInstanceOf[Comp]
        val x2 = transform(x)
        val y2 = transform(y).asInstanceOf[ForSpec]
        val z2 = transformArr(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else Comp(pos, x2, y2, z2)

      case ExprTags.Arr =>
        val Arr(pos, x) = expr.asInstanceOf[Arr]
        val x2 = transformArr(x)
        if(x2 eq x) expr
        else Arr(pos, x2)

      case ExprTags.ObjExtend =>
        val ObjExtend(superPos, x, y) = expr.asInstanceOf[ObjExtend]
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else ObjExtend(superPos, x2, y2.asInstanceOf[ObjBody])

      case ExprTags.`ObjBody.ObjComp` =>
        val ObjBody.ObjComp(pos, p, k, v, pl, o, f, r) = expr.asInstanceOf[ObjBody.ObjComp]
        val p2 = transformBinds(p)
        val k2 = transform(k)
        val v2 = transform(v)
        val o2 = transformBinds(o)
        val f2 = transform(f).asInstanceOf[ForSpec]
        val r2 = transformList(r).asInstanceOf[List[CompSpec]]
        if((p2 eq p) && (k2 eq k) && (v2 eq v) && (o2 eq o) && (f2 eq f) && (r2 eq r)) expr
        else ObjBody.ObjComp(pos, p2, k2, v2, pl, o2, f2, r2)

      case ExprTags.Slice =>
        val Slice(pos, v, x, y, z) = expr.asInstanceOf[Slice]
        val v2 = transform(v)
        val x2 = transformOption(x)
        val y2 = transformOption(y)
        val z2 = transformOption(z)
        if((v2 eq v) && (x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else Slice(pos, v2, x2, y2, z2)

      case ExprTags.IfSpec =>
        val IfSpec(pos, x) = expr.asInstanceOf[IfSpec]
        val x2 = transform(x)
        if(x2 eq x) expr
        else IfSpec(pos,  x2)

      case ExprTags.ForSpec =>
        val ForSpec(pos, name, x) = expr.asInstanceOf[ForSpec]
        val x2 = transform(x)
        if(x2 eq x) expr
        else ForSpec(pos, name, x2)

      case ExprTags.Error =>
        val Expr.Error(pos, x) = expr.asInstanceOf[Expr.Error]
        val x2 = transform(x)
        if(x2 eq x) expr
        else Expr.Error(pos, x2)

      case _ => expr
    }
  }

  protected def transformArr[T <: Expr](a: Array[T]): Array[T] =
    transformGenericArr(a)((transform(_)).asInstanceOf[T => T])

  protected def transformParams(p: Params): Params = {
    if(p == null) return null
    val defs = p.defaultExprs
    if(defs == null) p
    else {
      val defs2 = transformArr(defs)
      if(defs2 eq defs) p
      else p.copy(defaultExprs = defs2)
    }
  }

  protected def transformBinds(a: Array[Bind]): Array[Bind] =
    transformGenericArr(a)(transformBind)

  protected def transformFields(a: Array[Member.Field]): Array[Member.Field] =
    transformGenericArr(a)(transformField)

  protected def transformAsserts(a: Array[Member.AssertStmt]): Array[Member.AssertStmt] =
    transformGenericArr(a)(transformAssert)

  protected def transformBind(b: Bind): Bind = {
    val args = b.args
    val rhs = b.rhs
    val args2 = transformParams(args)
    val rhs2 = transform(rhs)
    if((args2 eq args) && (rhs2 eq rhs)) b
    else b.copy(args = args2, rhs = rhs2)
  }

  protected def transformField(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val y = f.args
    val z = f.rhs
    val x2 = transformFieldName(x)
    val y2 = transformParams(y)
    val z2 = transform(z)
    if((x2 eq x) && (y2 eq y) && (z2 eq z)) f
    else f.copy(fieldName = x2, args = y2, rhs = z2)
  }

  protected def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      val x2 = transform(x)
      if(x2 eq x) f else FieldName.Dyn(x2)
    case _ => f
  }

  protected def transformAssert(a: Member.AssertStmt): Member.AssertStmt = {
    val x = a.value
    val y = a.msg
    val x2 = transform(x)
    val y2 = transform(y)
    if((x2 eq x) && (y2 eq y)) a
    else a.copy(value = x2, msg = y2)
  }

  protected def transformOption(o: Option[Expr]): Option[Expr] = o match {
    case Some(e) =>
      val e2 = transform(e)
      if(e2 eq e) o else Some(e2)
    case None => o
  }

  protected def transformList(l: List[Expr]): List[Expr] = {
    val lb = List.newBuilder[Expr]
    var diff = false
    l.foreach { e =>
      val e2 = transform(e)
      lb.+=(e2)
      if(e2 ne e) diff = true
    }
    if(diff) lb.result() else l
  }

  protected def transformGenericArr[T <: AnyRef](a: Array[T])(f: T => T): Array[T] = {
    if(a == null) return null
    var i = 0
    while(i < a.length) {
      val x1 = a(i)
      val x2 = f(x1)
      if(x1 ne x2) {
        val a2 = a.clone()
        a2(i) = x2
        i += 1
        while(i < a2.length) {
          a2(i) = f(a2(i))
          i += 1
        }
        return a2
      }
      i += 1
    }
    a
  }
}
