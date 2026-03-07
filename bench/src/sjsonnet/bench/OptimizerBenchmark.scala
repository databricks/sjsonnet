package sjsonnet.bench

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.util.concurrent.TimeUnit
import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class OptimizerBenchmark {

  private var inputs: Iterable[(Expr, FileScope)] = _
  private var ev: EvalScope = _

  @Setup
  def setup(): Unit = {
    val (allFiles, ev) = MainBenchmark.findFiles()
    this.inputs = allFiles.map { case (p, s) =>
      fastparse.parse(
        s,
        new Parser(p, mutable.HashMap.empty, mutable.HashMap.empty).document(_)
      ) match {
        case Success(v, _)               => v
        case f: fastparse.Parsed.Failure =>
          throw new Exception(s"Failed to parse $p: ${f.msg}")
      }
    }
    this.ev = ev
    val static = inputs.map { case (expr, fs) =>
      (
        new StaticOptimizer(
          ev,
          _ => None,
          sjsonnet.stdlib.StdLibModule.Default.module,
          mutable.HashMap.empty,
          mutable.HashMap.empty
        ).optimize(expr),
        fs
      )
    }
    val countBefore, countStatic = new Counter
    inputs.foreach(t => assert(countBefore.transform(t._1) eq t._1))
    static.foreach(t => assert(countStatic.transform(t._1) eq t._1))
    System.err.println(s"Documents: total=${inputs.size}")
    System.err.println(s"Before: $countBefore")
    System.err.println(s"Static: $countStatic")
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (expr, fs) =>
      bh.consume(
        new StaticOptimizer(
          ev,
          _ => None,
          sjsonnet.stdlib.StdLibModule.Default.module,
          mutable.HashMap.empty,
          mutable.HashMap.empty
        ).optimize(expr)
      )
    })
  }

  class Counter {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
        otherObjs, namedApplies, applies, arityApplies, builtin = 0
    val applyArities = new mutable.LongMap[Int]()
    val ifElseChains = new mutable.LongMap[Int]()
    val selectChains = new mutable.LongMap[Int]()

    def transform(e: Expr): Expr = {
      total += 1
      if (e.isInstanceOf[Val]) vals += 1
      else exprs += 1
      e match {
        case _: Val.Arr  => arrVals += 1
        case a: Expr.Arr =>
          if (a.value.forall(_.isInstanceOf[Val])) staticArrExprs += 1
          else otherArrExprs += 1
        case _: Val.Obj                 => staticObjs += 1
        case e: Expr.ObjBody.MemberList =>
          if (e.binds == null && e.asserts == null && e.fields.forall(_.isStatic))
            missedStaticObjs += 1
          else otherObjs += 1
        case e: Expr.Apply =>
          if (e.namedNames == null) {
            applies += 1
            val a = e.args.length
            applyArities.put(a.toLong, applyArities.getOrElse(a.toLong, 0) + 1)
          } else namedApplies += 1
        case _: Expr.Apply0 | _: Expr.Apply1 | _: Expr.Apply2 | _: Expr.Apply3 => arityApplies += 1
        case _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin1 | _: Expr.ApplyBuiltin2 => builtin += 1
        case _                                                                    =>
      }
      val ifElseCount = countIfElse(e)
      if (ifElseCount > 0) {
        ifElseChains.put(ifElseCount.toLong, ifElseChains.getOrElse(ifElseCount.toLong, 0) + 1)
        if (ifElseCount > 1)
          ifElseChains.put(
            ifElseCount.toLong - 1L,
            ifElseChains.getOrElse(ifElseCount.toLong - 1L, 0) - 1
          )
      }
      val selectCount = countSelectOnId(e)
      if (selectCount >= 0) {
        selectChains.put(selectCount.toLong, selectChains.getOrElse(selectCount.toLong, 0) + 1)
        if (selectCount > 0)
          selectChains.put(
            selectCount.toLong - 1L,
            selectChains.getOrElse(selectCount.toLong - 1L, 0) - 1
          )
      }
      rec(e)
      e
    }

    private def rec(e: Expr): Unit = e match {
      case Expr.Select(_, x, _)      => transform(x)
      case Expr.Apply(_, x, y, _, _) =>
        transform(x)
        transformArr(y)
      case Expr.Apply0(_, x, _) =>
        transform(x)
      case Expr.Apply1(_, x, y, _) =>
        transform(x)
        transform(y)
      case Expr.Apply2(_, x, y, z, _) =>
        transform(x)
        transform(y)
        transform(z)
      case Expr.Apply3(_, x, y, z, a, _) =>
        transform(x)
        transform(y)
        transform(z)
        transform(a)
      case Expr.ApplyBuiltin(_, _, x, _) =>
        transformArr(x)
      case Expr.ApplyBuiltin1(_, _, x, _) =>
        transform(x)
      case Expr.ApplyBuiltin2(_, _, x, y, _) =>
        transform(x)
        transform(y)
      case Expr.ApplyBuiltin3(_, _, x, y, z, _) =>
        transform(x)
        transform(y)
        transform(z)
      case Expr.ApplyBuiltin4(_, _, x, y, z, a, _) =>
        transform(x)
        transform(y)
        transform(z)
        transform(a)
      case Expr.UnaryOp(_, _, x) =>
        transform(x)
      case Expr.BinaryOp(_, x, _, y) =>
        transform(x)
        transform(y)
      case Expr.And(_, x, y) =>
        transform(x)
        transform(y)
      case Expr.Or(_, x, y) =>
        transform(x)
        transform(y)
      case Expr.InSuper(_, x, _) =>
        transform(x)
      case Expr.Lookup(_, x, y) =>
        transform(x)
        transform(y)
      case Expr.LookupSuper(_, _, x) =>
        transform(x)
      case Expr.Function(_, params, body) =>
        transformParams(params)
        transform(body)
      case Expr.LocalExpr(_, binds, returned) =>
        transformBinds(binds)
        transform(returned)
      case Expr.IfElse(_, cond, thenExpr, elseExpr) =>
        transform(cond)
        transform(thenExpr)
        transform(elseExpr)
      case Expr.ObjBody.MemberList(_, binds, fields, asserts) =>
        transformBinds(binds)
        transformFields(fields)
        transformAsserts(asserts)
      case Expr.AssertExpr(_, assertion, returned) =>
        transform(assertion.value)
        if (assertion.msg != null) transform(assertion.msg)
        transform(returned)
      case Expr.Comp(_, value, first, rest) =>
        transform(value)
        transform(first)
        transformArr(rest)
      case Expr.Arr(_, values) =>
        transformArr(values)
      case Expr.ObjExtend(_, base, ext) =>
        transform(base)
        transform(ext)
      case Expr.ObjBody.ObjComp(_, preLocals, key, value, _, postLocals, first, rest) =>
        transformBinds(preLocals)
        transform(key)
        transform(value)
        transformBinds(postLocals)
        transform(first)
        transformList(rest)
      case Expr.Slice(_, value, start, end, stride) =>
        transform(value)
        transformOption(start)
        transformOption(end)
        transformOption(stride)
      case Expr.IfSpec(_, cond) =>
        transform(cond)
      case Expr.ForSpec(_, _, cond) =>
        transform(cond)
      case Expr.Error(_, value) =>
        transform(value)
      case _ =>
    }

    private def transformArr[T <: Expr](values: Array[T]): Unit = {
      if (values != null) values.foreach(transform)
    }

    private def transformOption(value: Option[Expr]): Unit = value.foreach(transform)

    private def transformList(values: List[Expr]): Unit = values.foreach(transform)

    private def transformParams(params: Expr.Params): Unit = {
      if (params != null && params.defaultExprs != null) transformArr(params.defaultExprs)
    }

    private def transformBinds(binds: Array[Expr.Bind]): Unit = {
      if (binds != null) binds.foreach { bind =>
        transformParams(bind.args)
        transform(bind.rhs)
      }
    }

    private def transformFieldName(fieldName: Expr.FieldName): Unit = fieldName match {
      case Expr.FieldName.Dyn(expr) => transform(expr)
      case _                        =>
    }

    private def transformFields(fields: Array[Expr.Member.Field]): Unit = {
      if (fields != null) fields.foreach { field =>
        transformFieldName(field.fieldName)
        transformParams(field.args)
        transform(field.rhs)
      }
    }

    private def transformAsserts(asserts: Array[Expr.Member.AssertStmt]): Unit = {
      if (asserts != null) asserts.foreach { assertion =>
        transform(assertion.value)
        if (assertion.msg != null) transform(assertion.msg)
      }
    }

    def countIfElse(e: Expr): Int = e match {
      case Expr.IfElse(_, _, _, else0) =>
        countIfElse(else0) + 1
      case _ => 0
    }
    def countSelectOnId(e: Expr): Int = e match {
      case Expr.Select(_, x, _) =>
        val c = countSelectOnId(x)
        if (c == -1) -1 else c + 1
      case _: Expr.ValidId => 0
      case _               => -1
    }
    override def toString: String = {
      val arities = applyArities.toSeq.sortBy(_._1).map { case (a, b) => s"$a: $b" }.mkString(", ")
      val chains = ifElseChains.toSeq.sortBy(_._1).map { case (a, b) => s"$a: $b" }.mkString(", ")
      val selChains =
        selectChains.toSeq.sortBy(_._1).map { case (a, b) => s"$a: $b" }.mkString(", ")
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, " +
      s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, " +
      s"other MemberList: $otherObjs, named Apply: $namedApplies, other Apply: $applies, " +
      s"ApplyN: $arityApplies, ApplyBuiltin*: $builtin; Apply arities: {$arities}, " +
      s"if/else chains: $chains, Select/ValidId chains: $selChains"
    }
  }
}
