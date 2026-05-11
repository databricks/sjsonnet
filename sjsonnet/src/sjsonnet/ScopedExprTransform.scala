package sjsonnet

import sjsonnet.Expr.ObjBody.{MemberList, ObjComp}
import sjsonnet.Expr._

import scala.annotation.nowarn
import scala.collection.immutable.HashMap

/** Tree transformer that keeps track of the bindings in the static scope. */
class ScopedExprTransform extends ExprTransform {
  import ScopedExprTransform._
  var scope: Scope = emptyScope

  // Marker for Exprs in the scope that should not be used because they need to be evaluated in a different scope
  val dynamicExpr: Expr = new Expr {
    var pos: Position = null; override def toString = "dynamicExpr"
  }

  def transform(e: Expr): Expr = e match {
    case LocalExpr(pos, bindings, returned) =>
      val (b2, r2) = nestedConsecutiveBindings(bindings)(transformBind)(transform(returned))
      if ((b2 eq bindings) && (r2 eq returned)) e
      else LocalExpr(pos, b2, r2)

    case MemberList(pos, binds, fields, asserts) =>
      val fields2 = transformGenericArr(fields)(transformFieldNameOnly)
      val (binds2, (fields3, asserts2)) = nestedObject(dynamicExpr, dynamicExpr) {
        nestedConsecutiveBindings(binds)(transformBind) {
          val fields3 = transformGenericArr(fields2)(transformFieldNoName)
          val asserts2 = transformAsserts(asserts)
          (fields3, asserts2)
        }
      }
      if ((binds2 eq binds) && (fields3 eq fields) && (asserts2 eq asserts)) e
      else ObjBody.MemberList(pos, binds2, fields3, asserts2)

    case Function(pos, params, body) =>
      nestedNames(params.names)(rec(e))

    case ObjComp(pos, preLocals, key, value, plus, postLocals, first, rest) =>
      val (f2 :: r2, (k2, (pre2, post2, v2))) = compSpecs(
        first :: rest,
        { () =>
          (
            transform(key),
            nestedBindings(dynamicExpr, dynamicExpr, preLocals ++ postLocals) {
              (transformBinds(preLocals), transformBinds(postLocals), transform(value))
            }
          )
        }
      ): @unchecked
      if (
        (f2 eq first) && (k2 eq key) && (v2 eq value) && (pre2 eq preLocals) && (post2 eq postLocals) && (
          r2,
          rest
        ).zipped.forall(_ eq _): @nowarn
      ) e
      else ObjComp(pos, pre2, k2, v2, plus, post2, f2.asInstanceOf[ForSpec], r2)

    case Comp(pos, value, first, rest) =>
      val (f2 :: r2, v2) = compSpecs(first :: rest.toList, () => transform(value)): @unchecked
      if ((f2 eq first) && (v2 eq value) && (r2, rest).zipped.forall(_ eq _): @nowarn) e
      else Comp(pos, v2, f2.asInstanceOf[ForSpec], r2.toArray)

    case e => rec(e)
  }

  override def transformBind(b: Bind): Bind = {
    val args = b.args
    val rhs = b.rhs
    nestedNames(if (args == null) null else args.names) {
      val args2 = transformParams(args)
      val rhs2 = transform(rhs)
      if ((args2 eq args) && (rhs2 eq rhs)) b
      else b.copy(args = args2, rhs = rhs2)
    }
  }

  protected def transformFieldNameOnly(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val x2 = transformFieldName(x)
    if (x2 eq x) f else f.copy(fieldName = x2)
  }

  protected def transformFieldNoName(f: Member.Field): Member.Field = {
    def g = {
      val y = f.args
      val z = f.rhs
      val y2 = transformParams(y)
      val z2 = transform(z)
      if ((y2 eq y) && (z2 eq z)) f else f.copy(args = y2, rhs = z2)
    }
    if (f.args == null) g
    else nestedNames(f.args.names)(g)
  }

  override protected def transformField(f: Member.Field): Member.Field = ???

  protected def compSpecs[T](a: List[CompSpec], value: () => T): (List[CompSpec], T) = a match {
    case (c @ ForSpec(pos, name, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[ForSpec]
      nestedWith(c2.name, dynamicExpr) {
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

  protected def nestedNew[T](sc: Scope)(f: => T): T = {
    val oldScope = scope
    scope = sc
    try f
    finally { scope = oldScope }
  }

  protected def nestedWith[T](n: String, e: Expr)(f: => T): T =
    nestedNew(
      new Scope(scope.mappings.updated(n, ScopedVal(e, scope, scope.size)), scope.size + 1)
    )(f)

  protected def nestedFileScope[T](fs: FileScope)(f: => T): T =
    nestedNew(emptyScope)(f)

  protected def nestedConsecutiveBindings[T](a: Array[Bind])(f: => Bind => Bind)(
      g: => T): (Array[Bind], T) = {
    if (a == null || a.length == 0) (a, g)
    else {
      val oldScope = scope
      try {
        val baseSize = oldScope.size
        val scopedVals = new Array[ScopedVal](a.length)
        var mappings = oldScope.mappings
        var idx = 0
        while (idx < a.length) {
          val b = a(idx)
          val sv = ScopedVal(if (b.args == null) b.rhs else b, oldScope, baseSize + idx)
          scopedVals(idx) = sv
          mappings = mappings.updated(b.name, sv)
          idx += 1
        }
        val scopeSize = baseSize + a.length
        scope = new Scope(mappings, scopeSize)
        var a2: Array[Bind] = null
        idx = 0
        while (idx < a.length) {
          val b = a(idx)
          val b2 = f(b)
          if ((a2 eq null) && (b2 ne b)) a2 = a.clone()
          if (a2 ne null) a2(idx) = b2
          val sv0 = scopedVals(idx)
          val sv =
            if (b2 eq b) sv0
            else sv0.copy(v = if (b2.args == null) b2.rhs else b2)
          mappings = mappings.updated(b.name, sv)
          scope = new Scope(mappings, scopeSize)
          idx += 1
        }
        (if (a2 eq null) a else a2, g)
      } finally { scope = oldScope }
    }
  }

  protected def nestedBindings[T](a: Array[Bind])(f: => T): T = {
    if (a == null || a.length == 0) f
    else {
      val oldScope = scope
      val baseSize = oldScope.size
      var newMappings = oldScope.mappings
      var idx = 0
      while (idx < a.length) {
        val b = a(idx)
        newMappings = newMappings.updated(
          b.name,
          ScopedVal(if (b.args == null) b.rhs else b, oldScope, baseSize + idx)
        )
        idx += 1
      }
      nestedNew(new Scope(newMappings, baseSize + a.length))(f)
    }
  }

  protected def nestedObject[T](self0: Expr, super0: Expr)(f: => T): T = {
    val self = ScopedVal(self0, scope, scope.size)
    val sup = ScopedVal(super0, scope, scope.size + 1)
    val newm = {
      val m1 = scope.mappings.updated("self", self).updated("super", sup)
      if (scope.contains("self")) m1 else m1.updated("$", self)
    }
    nestedNew(new Scope(newm, scope.size + 2))(f)
  }

  protected def nestedBindings[T](self0: Expr, super0: Expr, a: Array[Bind])(f: => T): T =
    nestedObject(self0, super0)(nestedBindings(a)(f))

  protected def nestedNames[T](a: Array[String])(f: => T): T = {
    if (a == null || a.length == 0) f
    else {
      val oldScope = scope
      val baseSize = oldScope.size
      var newMappings = oldScope.mappings
      var idx = 0
      while (idx < a.length) {
        newMappings = newMappings.updated(a(idx), ScopedVal(dynamicExpr, oldScope, baseSize + idx))
        idx += 1
      }
      nestedNew(new Scope(newMappings, baseSize + a.length))(f)
    }
  }
}

object ScopedExprTransform {
  final case class ScopedVal(v: AnyRef, sc: Scope, idx: Int)
  final class Scope(val mappings: HashMap[String, ScopedVal], val size: Int) {
    def get(s: String): ScopedVal = mappings.getOrElse(s, null)
    def contains(s: String): Boolean = mappings.contains(s)
  }
  def emptyScope: Scope = new Scope(HashMap.empty, 0)
}
