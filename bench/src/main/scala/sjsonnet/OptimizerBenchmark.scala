package sjsonnet

/*-
 * Changed:
 * - 80f58d4e2d5e4d4ea94ef828962ef5d8cba1a625: implements support for safe select operator ?.
 */

import java.io.StringWriter
import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

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
  private var allFiles: IndexedSeq[(Path, String)] = _
  private var ev: EvalScope = _

  @Setup
  def setup(): Unit = {
    val (allFiles, ev) = MainBenchmark.findFiles()
    this.inputs = allFiles.map { case (p, s) =>
      fastparse.parse(s, new Parser(p, true).document(_)) match {
        case Success(v, _) => v
      }
    }
    this.ev = ev
    val static = inputs.map {
      case (expr, fs) => ((new StaticOptimizer(ev, new Std().Std)).optimize(expr), fs)
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
      bh.consume((new StaticOptimizer(ev, new Std().Std)).optimize(expr))
    })
  }

  class Counter extends ExprTransform {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
      otherObjs, namedApplies, applies, arityApplies, builtin = 0
    val applyArities = new mutable.LongMap[Int]()
    val ifElseChains = new mutable.LongMap[Int]()
    val selectChains = new mutable.LongMap[Int]()
    def transform(e: Expr) = {
      total += 1
      if(e.isInstanceOf[Val]) vals += 1
      else exprs += 1
      e match {
        case _: Val.Arr => arrVals += 1
        case a: Expr.Arr =>
          if(a.value.forall(_.isInstanceOf[Val])) staticArrExprs += 1
          else otherArrExprs += 1
        case _: Val.Obj => staticObjs += 1
        case e: Expr.ObjBody.MemberList =>
          if(e.binds == null && e.asserts == null && e.fields.forall(_.isStatic)) missedStaticObjs += 1
          else otherObjs += 1
        case e: Expr.Apply =>
          if(e.namedNames == null) {
            applies += 1
            val a = e.args.length
            applyArities.put(a.toLong, applyArities.getOrElse(a.toLong, 0) + 1)
          } else namedApplies += 1

        case _: Expr.Apply0 | _: Expr.Apply1 | _: Expr.Apply2 | _: Expr.Apply3 => arityApplies += 1
        case _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin1 | _: Expr.ApplyBuiltin2 => builtin += 1
        case _ =>
      }
      val ifElseCount = countIfElse(e)
      if(ifElseCount > 0) {
        ifElseChains.put(ifElseCount.toLong, ifElseChains.getOrElse(ifElseCount.toLong, 0) + 1)
        if(ifElseCount > 1)
          ifElseChains.put(ifElseCount.toLong-1L, ifElseChains.getOrElse(ifElseCount.toLong-1L, 0) - 1)
      }
      val selectCount = countSelectOnId(e)
      if(selectCount >= 0) {
        selectChains.put(selectCount.toLong, selectChains.getOrElse(selectCount.toLong, 0) + 1)
        if(selectCount > 0)
          selectChains.put(selectCount.toLong-1L, selectChains.getOrElse(selectCount.toLong-1L, 0) - 1)
      }
      rec(e)
    }
    def countIfElse(e: Expr): Int = e match {
      case Expr.IfElse(_, _, _, else0) =>
        countIfElse(else0) + 1
      case _ => 0
    }
    def countSelectOnId(e: Expr): Int = e match {
      case Expr.Select(_, x, _, _) =>
       val c = countSelectOnId(x)
        if(c == -1) -1 else c + 1
      case _: Expr.ValidId => 0
      case _ => -1
    }
    override def toString = {
      val arities = applyArities.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      val chains = ifElseChains.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      val selChains = selectChains.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, "+
        s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, "+
        s"other MemberList: $otherObjs, named Apply: $namedApplies, other Apply: $applies, "+
        s"ApplyN: $arityApplies, ApplyBuiltin*: $builtin; Apply arities: {$arities}, "+
        s"if/else chains: $chains, Select/ValidId chains: $selChains"
    }
  }
}
