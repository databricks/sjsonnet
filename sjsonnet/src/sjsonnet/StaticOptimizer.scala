package sjsonnet

import scala.annotation.{nowarn, switch}
import scala.collection.immutable.HashMap
import scala.collection.mutable

import Expr.*
import Evaluator.SafeDoubleOps

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
    ]) {
  import StaticOptimizer.*

  private[this] var scope: Scope = emptyScope

  // Marker for Exprs in the scope that should not be used because they need to be evaluated in a different scope
  private[this] val dynamicExpr: Expr = new Expr {
    var pos: Position = null
    override def toString = "dynamicExpr"
  }

  def optimize(e: Expr): Expr = transform(e)

  def transform(e: Expr): Expr = {
    if (e == null) return null
    val tag = e.tag
    if (tag == ExprTags.BinaryOp || tag == ExprTags.UnaryOp) {
      val d = tryFoldAsDouble(e)
      if (!d.isNaN) return Val.Num(e.pos, d)
    }
    optimizeTransformed(transformChildren(check(e)))
  }

  private def transformChildren(e: Expr): Expr = (e.tag: @switch) match {
    case ExprTags.LocalExpr =>
      val local = e.asInstanceOf[LocalExpr]
      val bindings = local.bindings
      val returned = local.returned
      val (bindings2, returned2) =
        nestedConsecutiveBindings(bindings)(transformBind)(transform(returned))
      if ((bindings2 eq bindings) && (returned2 eq returned)) e
      else LocalExpr(local.pos, bindings2, returned2)

    case ExprTags.`ObjBody.MemberList` =>
      val memberList = e.asInstanceOf[ObjBody.MemberList]
      val binds = memberList.binds
      val fields = memberList.fields
      val asserts = memberList.asserts
      val fields2 = transformGenericArr(fields)(transformFieldNameOnly)
      val (binds2, (fields3, asserts2)) = nestedObject(dynamicExpr, dynamicExpr) {
        nestedConsecutiveBindings(binds)(transformBind) {
          val fields3 = transformGenericArr(fields2)(transformFieldNoName)
          val asserts2 = transformAsserts(asserts)
          (fields3, asserts2)
        }
      }
      if ((binds2 eq binds) && (fields3 eq fields) && (asserts2 eq asserts)) e
      else ObjBody.MemberList(memberList.pos, binds2, fields3, asserts2)

    case ExprTags.Function =>
      transformFunction(e.asInstanceOf[Function], -1)

    case ExprTags.`ObjBody.ObjComp` =>
      val objComp = e.asInstanceOf[ObjBody.ObjComp]
      val specs = objComp.first :: objComp.rest
      val (first2 :: rest2, (key2, (pre2, post2, value2))) = compSpecs(
        specs,
        { () =>
          (
            transform(objComp.key),
            nestedBindings(dynamicExpr, dynamicExpr, objComp.preLocals ++ objComp.postLocals) {
              (
                transformBinds(objComp.preLocals),
                transformBinds(objComp.postLocals),
                transform(objComp.value)
              )
            }
          )
        }
      ): @unchecked
      if (
        (first2 eq objComp.first) && (key2 eq objComp.key) && (value2 eq objComp.value) &&
        (pre2 eq objComp.preLocals) && (post2 eq objComp.postLocals) &&
        (rest2, objComp.rest).zipped.forall(_ eq _): @nowarn
      ) e
      else
        ObjBody.ObjComp(
          objComp.pos,
          pre2,
          key2,
          value2,
          objComp.plus,
          post2,
          first2.asInstanceOf[ForSpec],
          rest2
        )

    case ExprTags.Comp =>
      val comp = e.asInstanceOf[Comp]
      val (first2 :: rest2, value2) =
        compSpecs(comp.first :: comp.rest.toList, () => transform(comp.value)): @unchecked
      if (
        (first2 eq comp.first) && (value2 eq comp.value) && (rest2, comp.rest).zipped.forall(
          _ eq _
        ): @nowarn
      ) e
      else Comp(comp.pos, value2, first2.asInstanceOf[ForSpec], rest2.toArray)

    case _ =>
      rec(e)
  }

  private def optimizeTransformed(e: Expr): Expr = {
    if (e == null) return null
    (e.tag: @switch) match {
      case ExprTags.UNTAGGED =>
        e match {
          case id @ Id(pos, name) =>
            scope.get(name) match {
              case ScopedVal(v: Val with Expr, _, _) => v
              case ScopedVal(_, _, idx)              => ValidId(pos, name, idx)
              case null if name == f"$$std"          => std
              case null if name == "std"             => std
              case null                              =>
                variableResolver(name) match {
                  case Some(v) => v
                  case None    =>
                    StaticError.fail(
                      "Unknown variable: " + name,
                      id
                    )(ev)
                }
            }
          case _ => e
        }

      case ExprTags.ValidId =>
        e match {
          case self @ Self(pos) =>
            scope.get("self") match {
              case ScopedVal(v, _, idx) if v != null => ValidId(pos, "self", idx)
              case _ => StaticError.fail("Can't use self outside of an object", self)(ev)
            }
          case _ => e
        }

      case ExprTags.BinaryOp =>
        e match {
          case root @ $(pos) =>
            scope.get("$") match {
              case ScopedVal(v, _, idx) if v != null => ValidId(pos, "$", idx)
              case _ => StaticError.fail("Can't use $ outside of an object", root)(ev)
            }
          case binary: BinaryOp =>
            binary.rhs match {
              case ValidSuper(_, selfIdx) if binary.op == BinaryOp.OP_in =>
                InSuper(binary.pos, binary.lhs, selfIdx)
              case rhs if binary.op == BinaryOp.OP_% =>
                binary.lhs match {
                  case lhs: Val.Str =>
                    try {
                      rhs match {
                        case r: Val =>
                          val partial = new Format.PartialApplyFmt(lhs.str)
                          try partial.evalRhs(r, ev, binary.pos).asInstanceOf[Expr]
                          catch {
                            case _: Exception =>
                              ApplyBuiltin1(binary.pos, partial, rhs, tailstrict = false)
                          }
                        case _ =>
                          ApplyBuiltin1(
                            binary.pos,
                            new Format.PartialApplyFmt(lhs.str),
                            rhs,
                            tailstrict = false
                          )
                      }
                    } catch { case _: Exception => binary }
                  case lhs: Val =>
                    rhs match {
                      case r: Val => tryFoldBinaryOp(binary.pos, lhs, binary.op, r, binary)
                      case _      => binary
                    }
                  case _ => binary
                }
              case rhs: Val =>
                binary.lhs match {
                  case lhs: Val => tryFoldBinaryOp(binary.pos, lhs, binary.op, rhs, binary)
                  case _        => binary
                }
              case _ => binary
            }
          case _ => e
        }

      case ExprTags.Select =>
        e match {
          case sup @ Super(_) if !scope.contains("super") =>
            StaticError.fail("Can't use super outside of an object", sup)(ev)
          case select: Select =>
            select.value match {
              case obj: Val.Obj if obj.containsKey(select.name) =>
                try obj.value(select.name, select.pos)(ev).asInstanceOf[Expr]
                catch { case _: Exception => select }
              case ValidSuper(_, selfIdx) =>
                SelectSuper(select.pos, selfIdx, select.name)
              case _ =>
                select
            }
          case _ => e
        }

      case ExprTags.Apply =>
        transformApply(e.asInstanceOf[Apply])

      case ExprTags.Lookup =>
        val lookup = e.asInstanceOf[Lookup]
        lookup.value match {
          case ValidSuper(_, selfIdx) => LookupSuper(lookup.pos, selfIdx, lookup.index)
          case _                      => lookup
        }

      case ExprTags.Arr =>
        val arr = e.asInstanceOf[Arr]
        if (arr.value.forall(_.isInstanceOf[Val]))
          Val.Arr(arr.pos, arr.value.map(_.asInstanceOf[Val]))
        else e

      case ExprTags.`ObjBody.MemberList` =>
        optimizeMemberList(e.asInstanceOf[ObjBody.MemberList])

      case ExprTags.UnaryOp =>
        val unary = e.asInstanceOf[UnaryOp]
        unary.value match {
          case v: Val => tryFoldUnaryOp(unary.pos, unary.op, v, e)
          case _      => e
        }

      case ExprTags.IfElse =>
        val ifElse = e.asInstanceOf[IfElse]
        ifElse.cond match {
          case _: Val.True =>
            ifElse.`then`.pos = ifElse.pos
            ifElse.`then`
          case _: Val.False =>
            val elseExpr = ifElse.`else`
            if (elseExpr == null) Val.Null(ifElse.pos)
            else {
              elseExpr.pos = ifElse.pos
              elseExpr
            }
          case _ => e
        }

      case ExprTags.And =>
        val and = e.asInstanceOf[And]
        and.lhs match {
          case _: Val.True =>
            and.rhs match {
              case rhs: Val.Bool =>
                rhs.pos = and.pos
                rhs
              case _ => e
            }
          case _: Val.False => Val.False(and.pos)
          case _            => e
        }

      case ExprTags.Or =>
        val or = e.asInstanceOf[Or]
        or.lhs match {
          case _: Val.True  => Val.True(or.pos)
          case _: Val.False =>
            or.rhs match {
              case rhs: Val.Bool =>
                rhs.pos = or.pos
                rhs
              case _ => e
            }
          case _ => e
        }

      case _ =>
        e
    }
  }

  private def optimizeMemberList(m: ObjBody.MemberList): Expr = {
    def allFieldsStaticAndUniquelyNamed: Boolean = {
      val seen = mutable.Set.empty[String]
      m.fields.forall { f =>
        f.isStatic && seen.add(f.fieldName.asInstanceOf[FieldName.Fixed].value)
      }
    }

    if (m.binds == null && m.asserts == null && allFieldsStaticAndUniquelyNamed)
      Val.staticObject(m.pos, m.fields, internedStaticFieldSets, internedStrings)
    else m
  }

  private def rec(expr: Expr): Expr = expr match {
    case Select(pos, x, name) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else Select(pos, x2, name)

    case Apply(pos, x, y, namedNames, tailstrict, tailrec) =>
      val x2 = transform(x)
      val y2 = transformArr(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else Apply(pos, x2, y2, namedNames, tailstrict, tailrec)

    case Apply0(pos, x, tailstrict, tailrec) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else Apply0(pos, x2, tailstrict, tailrec)

    case Apply1(pos, x, y, tailstrict, tailrec) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else Apply1(pos, x2, y2, tailstrict, tailrec)

    case Apply2(pos, x, y, z, tailstrict, tailrec) =>
      val x2 = transform(x)
      val y2 = transform(y)
      val z2 = transform(z)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else Apply2(pos, x2, y2, z2, tailstrict, tailrec)

    case Apply3(pos, x, y, z, a, tailstrict, tailrec) =>
      val x2 = transform(x)
      val y2 = transform(y)
      val z2 = transform(z)
      val a2 = transform(a)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z) && (a2 eq a)) expr
      else Apply3(pos, x2, y2, z2, a2, tailstrict, tailrec)

    case ApplyBuiltin(pos, func, x, tailstrict) =>
      val x2 = transformArr(x)
      if (x2 eq x) expr
      else ApplyBuiltin(pos, func, x2, tailstrict)

    case ApplyBuiltin1(pos, func, x, tailstrict) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else ApplyBuiltin1(pos, func, x2, tailstrict)

    case ApplyBuiltin2(pos, func, x, y, tailstrict) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else ApplyBuiltin2(pos, func, x2, y2, tailstrict)

    case ApplyBuiltin3(pos, func, x, y, z, tailstrict) =>
      val x2 = transform(x)
      val y2 = transform(y)
      val z2 = transform(z)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else ApplyBuiltin3(pos, func, x2, y2, z2, tailstrict)

    case ApplyBuiltin4(pos, func, x, y, z, a, tailstrict) =>
      val x2 = transform(x)
      val y2 = transform(y)
      val z2 = transform(z)
      val a2 = transform(a)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z) && (a2 eq a)) expr
      else ApplyBuiltin4(pos, func, x2, y2, z2, a2, tailstrict)

    case UnaryOp(pos, op, x) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else UnaryOp(pos, op, x2)

    case BinaryOp(pos, x, op, y) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else BinaryOp(pos, x2, op, y2)

    case And(pos, x, y) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else And(pos, x2, y2)

    case Or(pos, x, y) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else Or(pos, x2, y2)

    case InSuper(pos, x, selfIdx) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else InSuper(pos, x2, selfIdx)

    case Lookup(pos, x, y) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else Lookup(pos, x2, y2)

    case LookupSuper(pos, selfIdx, x) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else LookupSuper(pos, selfIdx, x2)

    case Function(pos, x, y) =>
      val x2 = transformParams(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else Function(pos, x2, y2)

    case LocalExpr(pos, x, y) =>
      val x2 = transformBinds(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else LocalExpr(pos, x2, y2)

    case IfElse(pos, x, y, z) =>
      val x2 = transform(x)
      val y2 = transform(y)
      val z2 = transform(z)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else IfElse(pos, x2, y2, z2)

    case ObjBody.MemberList(pos, x, y, z) =>
      val x2 = transformBinds(x)
      val y2 = transformFields(y)
      val z2 = transformAsserts(z)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else ObjBody.MemberList(pos, x2, y2, z2)

    case AssertExpr(pos, x, y) =>
      val x2 = transformAssert(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else AssertExpr(pos, x2, y2)

    case Comp(pos, x, y, z) =>
      val x2 = transform(x)
      val y2 = transform(y).asInstanceOf[ForSpec]
      val z2 = transformArr(z)
      if ((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else Comp(pos, x2, y2, z2)

    case Arr(pos, x) =>
      val x2 = transformArr(x)
      if (x2 eq x) expr
      else Arr(pos, x2)

    case ObjExtend(pos, x, y) =>
      val x2 = transform(x)
      val y2 = transform(y)
      if ((x2 eq x) && (y2 eq y)) expr
      else ObjExtend(pos, x2, y2.asInstanceOf[ObjBody])

    case ObjBody.ObjComp(pos, p, k, v, pl, o, f, r) =>
      val p2 = transformBinds(p)
      val k2 = transform(k)
      val v2 = transform(v)
      val o2 = transformBinds(o)
      val f2 = transform(f).asInstanceOf[ForSpec]
      val r2 = transformList(r).asInstanceOf[List[CompSpec]]
      if ((p2 eq p) && (k2 eq k) && (v2 eq v) && (o2 eq o) && (f2 eq f) && (r2 eq r)) expr
      else ObjBody.ObjComp(pos, p2, k2, v2, pl, o2, f2, r2)

    case Slice(pos, v, x, y, z) =>
      val v2 = transform(v)
      val x2 = transformOption(x)
      val y2 = transformOption(y)
      val z2 = transformOption(z)
      if ((v2 eq v) && (x2 eq x) && (y2 eq y) && (z2 eq z)) expr
      else Slice(pos, v2, x2, y2, z2)

    case IfSpec(pos, x) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else IfSpec(pos, x2)

    case ForSpec(pos, name, x) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else ForSpec(pos, name, x2)

    case Expr.Error(pos, x) =>
      val x2 = transform(x)
      if (x2 eq x) expr
      else Expr.Error(pos, x2)

    case other => other
  }

  private def transformArr[T <: Expr](a: Array[T]): Array[T] =
    transformGenericArr(a)((transform(_)).asInstanceOf[T => T])

  private def transformParams(p: Params): Params = {
    if (p == null) return null
    val defs = p.defaultExprs
    if (defs == null) p
    else {
      val defs2 = transformArr(defs)
      if (defs2 eq defs) p
      else p.copy(defaultExprs = defs2)
    }
  }

  private def transformBinds(a: Array[Bind]): Array[Bind] =
    transformGenericArr(a)(transformBind)

  private def transformFields(a: Array[Member.Field]): Array[Member.Field] =
    transformGenericArr(a)(transformField)

  private def transformAsserts(a: Array[Member.AssertStmt]): Array[Member.AssertStmt] =
    transformGenericArr(a)(transformAssert)

  private def transformBind(b: Bind): Bind = {
    val selfTailrecNameIdx = scope.get(b.name).idx
    b.args match {
      case null =>
        b.rhs match {
          case function: Function =>
            val rhs2 = transformFunction(function, selfTailrecNameIdx)
            if (rhs2 eq function) b
            else b.copy(rhs = rhs2)
          case rhs =>
            val rhs2 = transform(rhs)
            if (rhs2 eq rhs) b
            else b.copy(rhs = rhs2)
        }
      case args =>
        val rhs = b.rhs
        nestedNames(args.names) {
          val args2 = transformParams(args)
          val rhs2 = markDirectSelfTailCalls(transform(rhs), selfTailrecNameIdx)
          if ((args2 eq args) && (rhs2 eq rhs)) b
          else b.copy(args = args2, rhs = rhs2)
        }
    }
  }

  private def transformField(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val y = f.args
    val z = f.rhs
    val x2 = transformFieldName(x)
    val y2 = transformParams(y)
    val z2 = transform(z)
    if ((x2 eq x) && (y2 eq y) && (z2 eq z)) f
    else f.copy(fieldName = x2, args = y2, rhs = z2)
  }

  private def transformFieldNameOnly(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val x2 = transformFieldName(x)
    if (x2 eq x) f else f.copy(fieldName = x2)
  }

  private def transformFieldNoName(f: Member.Field): Member.Field = {
    def transformed = {
      val y = f.args
      val z = f.rhs
      val y2 = transformParams(y)
      val z2 = transform(z)
      if ((y2 eq y) && (z2 eq z)) f else f.copy(args = y2, rhs = z2)
    }
    if (f.args == null) transformed
    else nestedNames(f.args.names)(transformed)
  }

  private def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      transform(x) match {
        case x2: Val.Str =>
          FieldName.Fixed(x2.str)
        case x2 if x2 eq x => f
        case x2            => FieldName.Dyn(x2)
      }
    case _ => f
  }

  private def transformAssert(a: Member.AssertStmt): Member.AssertStmt = {
    val x = a.value
    val y = a.msg
    val x2 = transform(x)
    val y2 = transform(y)
    if ((x2 eq x) && (y2 eq y)) a
    else a.copy(value = x2, msg = y2)
  }

  private def transformOption(o: Option[Expr]): Option[Expr] = o match {
    case Some(e) =>
      val e2 = transform(e)
      if (e2 eq e) o else Some(e2)
    case None => o
  }

  private def transformList(l: List[Expr]): List[Expr] = {
    val lb = List.newBuilder[Expr]
    var diff = false
    l.foreach { e =>
      val e2 = transform(e)
      lb += e2
      if (e2 ne e) diff = true
    }
    if (diff) lb.result() else l
  }

  private def transformGenericArr[T <: AnyRef](a: Array[T])(f: T => T): Array[T] = {
    if (a == null) return null
    var i = 0
    while (i < a.length) {
      val x1 = a(i)
      val x2 = f(x1)
      if (x1 ne x2) {
        val a2 = a.clone()
        a2(i) = x2
        i += 1
        while (i < a2.length) {
          a2(i) = f(a2(i))
          i += 1
        }
        return a2
      }
      i += 1
    }
    a
  }

  private def compSpecs[T](a: List[CompSpec], value: () => T): (List[CompSpec], T) = a match {
    case (c @ ForSpec(_, _, _)) :: cs =>
      val c2 = rec(c).asInstanceOf[ForSpec]
      nestedWith(c2.name, dynamicExpr) {
        val (cs2, value2) = compSpecs(cs, value)
        (c2 :: cs2, value2)
      }
    case (c @ IfSpec(_, _)) :: cs =>
      val c2 = rec(c).asInstanceOf[CompSpec]
      val (cs2, value2) = compSpecs(cs, value)
      (c2 :: cs2, value2)
    case Nil =>
      (Nil, value())
  }

  private def nestedNew[T](sc: Scope)(f: => T): T = {
    val oldScope = scope
    scope = sc
    try f
    finally scope = oldScope
  }

  private def nestedWith[T](n: String, e: Expr)(f: => T): T =
    nestedNew(
      new Scope(scope.mappings.updated(n, ScopedVal(e, scope, scope.size)), scope.size + 1)
    )(f)

  private def nestedFileScope[T](fs: FileScope)(f: => T): T =
    nestedNew(emptyScope)(f)

  private def nestedConsecutiveBindings[T](a: Array[Bind])(f: => Bind => Bind)(
      g: => T): (Array[Bind], T) = {
    if (a == null || a.length == 0) (a, g)
    else {
      val oldScope = scope
      try {
        val mappings = a.zipWithIndex.map { case (b, idx) =>
          (b.name, ScopedVal(if (b.args == null) b.rhs else b, scope, scope.size + idx))
        }
        scope = new Scope(oldScope.mappings ++ mappings, oldScope.size + a.length)
        var changed = false
        val a2 = a.zipWithIndex.map { case (b, idx) =>
          val b2 = f(b)
          val sv = mappings(idx)._2.copy(v = if (b2.args == null) b2.rhs else b2)
          scope = new Scope(scope.mappings.updated(b.name, sv), scope.size)
          if (b2 ne b) changed = true
          b2
        }
        (if (changed) a2 else a, g)
      } finally scope = oldScope
    }
  }

  private def nestedBindings[T](a: Array[Bind])(f: => T): T = {
    if (a == null || a.length == 0) f
    else {
      val newMappings = a.zipWithIndex.map { case (b, idx) =>
        (b.name, ScopedVal(if (b.args == null) b.rhs else b, scope, scope.size + idx))
      }
      nestedNew(new Scope(scope.mappings ++ newMappings, scope.size + a.length))(f)
    }
  }

  private def nestedObject[T](self0: Expr, super0: Expr)(f: => T): T = {
    val self = ScopedVal(self0, scope, scope.size)
    val sup = ScopedVal(super0, scope, scope.size + 1)
    val newMappings = {
      val withSelf = scope.mappings + (("self", self)) + (("super", sup))
      if (scope.contains("self")) withSelf else withSelf + (("$", self))
    }
    nestedNew(new Scope(newMappings, scope.size + 2))(f)
  }

  private def nestedBindings[T](self0: Expr, super0: Expr, a: Array[Bind])(f: => T): T =
    nestedObject(self0, super0)(nestedBindings(a)(f))

  private def nestedNames[T](a: Array[String])(f: => T): T = {
    if (a == null || a.length == 0) f
    else {
      val newMappings = a.zipWithIndex.map { case (n, idx) =>
        (n, ScopedVal(dynamicExpr, scope, scope.size + idx))
      }
      nestedNew(new Scope(scope.mappings ++ newMappings, scope.size + a.length))(f)
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
      val tag = e.tag
      if (tag == ExprTags.`Val.Literal`) {
        e match {
          case Val.Num(_, n) => n
          case _             => Double.NaN
        }
      } else if (tag == ExprTags.BinaryOp && e.isInstanceOf[BinaryOp]) {
        val binary = e.asInstanceOf[BinaryOp]
        val l = tryFoldAsDouble(binary.lhs)
        if (l.isNaN) return Double.NaN
        val r = tryFoldAsDouble(binary.rhs)
        if (r.isNaN) return Double.NaN
        (binary.op: @switch) match {
          case BinaryOp.OP_+ =>
            val res = l + r; if (res.isInfinite) return Double.NaN; res
          case BinaryOp.OP_- =>
            val res = l - r; if (res.isInfinite) return Double.NaN; res
          case BinaryOp.OP_* =>
            val res = l * r; if (res.isInfinite) return Double.NaN; res
          case BinaryOp.OP_/ =>
            if (r == 0) return Double.NaN
            val res = l / r; if (res.isInfinite) return Double.NaN; res
          case BinaryOp.OP_% =>
            l % r
          case BinaryOp.OP_<< =>
            val ll = l.toSafeLong(binary.pos)(ev); val rr = r.toSafeLong(binary.pos)(ev)
            if (rr < 0) return Double.NaN
            if (rr >= 1 && math.abs(ll) >= (1L << (63 - rr))) return Double.NaN
            (ll << rr).toDouble
          case BinaryOp.OP_>> =>
            val ll = l.toSafeLong(binary.pos)(ev); val rr = r.toSafeLong(binary.pos)(ev)
            if (rr < 0) return Double.NaN
            (ll >> rr).toDouble
          case BinaryOp.OP_& =>
            (l.toSafeLong(binary.pos)(ev) & r.toSafeLong(binary.pos)(ev)).toDouble
          case BinaryOp.OP_^ =>
            (l.toSafeLong(binary.pos)(ev) ^ r.toSafeLong(binary.pos)(ev)).toDouble
          case BinaryOp.OP_| =>
            (l.toSafeLong(binary.pos)(ev) | r.toSafeLong(binary.pos)(ev)).toDouble
          case _ => Double.NaN
        }
      } else if (tag == ExprTags.UnaryOp) {
        val unary = e.asInstanceOf[UnaryOp]
        val d = tryFoldAsDouble(unary.value)
        if (d.isNaN) return Double.NaN
        (unary.op: @switch) match {
          case Expr.UnaryOp.OP_- => -d
          case Expr.UnaryOp.OP_+ => d
          case Expr.UnaryOp.OP_~ => (~d.toLong).toDouble
          case _                 => Double.NaN
        }
      } else {
        Double.NaN
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

  private def transformApply(a: Apply): Expr = {
    val rebound = rebindApply(a.pos, a.value, a.args, a.namedNames, a.tailstrict, a.tailrec) match {
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
        case 0 => Apply0(a.pos, a.value, a.tailstrict, a.tailrec)
        case 1 => Apply1(a.pos, a.value, a.args(0), a.tailstrict, a.tailrec)
        case 2 => Apply2(a.pos, a.value, a.args(0), a.args(1), a.tailstrict, a.tailrec)
        case 3 => Apply3(a.pos, a.value, a.args(0), a.args(1), a.args(2), a.tailstrict, a.tailrec)
        case _ => a
      }
  }

  private def rebindApply(
      pos: Position,
      lhs: Expr,
      args: Array[Expr],
      names: Array[String],
      tailstrict: Boolean,
      tailrec: Boolean): Expr = lhs match {
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
            case newArgs => Apply(pos, lhs, newArgs, null, tailstrict, tailrec)
          }
        case ScopedVal(Bind(_, _, params, _), _, _) =>
          rebind(args, names, params) match {
            case null    => null
            case newArgs => Apply(pos, lhs, newArgs, null, tailstrict, tailrec)
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

  private def transformFunction(function: Function, selfTailrecNameIdx: Int): Function =
    nestedNames(function.params.names) {
      val params = function.params
      val body = function.body
      val params2 = transformParams(params)
      val body2 =
        if (selfTailrecNameIdx == -1) transform(body)
        else markDirectSelfTailCalls(transform(body), selfTailrecNameIdx)
      if ((params2 eq params) && (body2 eq body)) function
      else Function(function.pos, params2, body2)
    }

  private def markDirectSelfTailCalls(e: Expr, selfTailrecNameIdx: Int): Expr = e match {
    case IfElse(pos, cond, thenExpr, elseExpr) =>
      val thenExpr2 = markDirectSelfTailCalls(thenExpr, selfTailrecNameIdx)
      val elseExpr2 =
        if (elseExpr == null) null
        else markDirectSelfTailCalls(elseExpr, selfTailrecNameIdx)
      if ((thenExpr2 eq thenExpr) && (elseExpr2 eq elseExpr)) e
      else IfElse(pos, cond, thenExpr2, elseExpr2)

    case LocalExpr(pos, bindings, returned) =>
      val returned2 = markDirectSelfTailCalls(returned, selfTailrecNameIdx)
      if (returned2 eq returned) e
      else LocalExpr(pos, bindings, returned2)

    case AssertExpr(pos, asserted, returned) =>
      val returned2 = markDirectSelfTailCalls(returned, selfTailrecNameIdx)
      if (returned2 eq returned) e
      else AssertExpr(pos, asserted, returned2)

    case apply @ Apply(_, value, _, _, tailstrict, _)
        if !tailstrict && isDirectSelfCall(value, selfTailrecNameIdx) =>
      apply.copy(tailrec = true)
    case apply @ Apply0(_, value, tailstrict, _)
        if !tailstrict && isDirectSelfCall(value, selfTailrecNameIdx) =>
      apply.copy(tailrec = true)
    case apply @ Apply1(_, value, _, tailstrict, _)
        if !tailstrict && isDirectSelfCall(value, selfTailrecNameIdx) =>
      apply.copy(tailrec = true)
    case apply @ Apply2(_, value, _, _, tailstrict, _)
        if !tailstrict && isDirectSelfCall(value, selfTailrecNameIdx) =>
      apply.copy(tailrec = true)
    case apply @ Apply3(_, value, _, _, _, tailstrict, _)
        if !tailstrict && isDirectSelfCall(value, selfTailrecNameIdx) =>
      apply.copy(tailrec = true)

    case _ => e
  }

  private def isDirectSelfCall(value: Expr, selfTailrecNameIdx: Int): Boolean = value match {
    case ValidId(_, _, nameIdx) => nameIdx == selfTailrecNameIdx
    case _                      => false
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
            case n: Val.Num => Val.Num(pos, (~n.rawDouble.toLong).toDouble)
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
            case (Val.Num(_, l), Val.Num(_, r)) if r != 0 => Val.Num(pos, l % r)
            case _                                        => fallback
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

object StaticOptimizer {
  final case class ScopedVal(v: AnyRef, sc: Scope, idx: Int)

  final class Scope(val mappings: HashMap[String, ScopedVal], val size: Int) {
    def get(s: String): ScopedVal = mappings.getOrElse(s, null)
    def contains(s: String): Boolean = mappings.contains(s)
  }

  val emptyScope: Scope = new Scope(HashMap.empty, 0)
}
