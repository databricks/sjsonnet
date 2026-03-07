package sjsonnet

import utest._

object AstVisitProfilerTests extends TestSuite {
  private val dummyPos = new Position(new FileScope(DummyPath("ast-visit-profiler-tests")), 0)
  private val emptyVars: String => Option[ExternalVariable[_]] = _ => None

  private def runExpr(expr: Expr, useNewEvaluator: Boolean): AstVisitProfileSnapshot = {
    val stats = new DebugStats
    val profiler = new AstVisitProfiler
    stats.astVisitProfiler = profiler
    val interpreter = new Interpreter(
      emptyVars,
      emptyVars,
      DummyPath("ast-visit-profiler-tests"),
      Importer.empty,
      new DefaultParseCache,
      new Settings(useNewEvaluator = useNewEvaluator),
      storePos = null,
      logger = null,
      std = sjsonnet.stdlib.StdLibModule.Default.module,
      variableResolver = _ => None,
      debugStats = stats
    )
    try interpreter.evaluator.visitExpr(expr)(ValScope.empty)
    catch {
      case _: Error => ()
    }
    profiler.snapshot()
  }

  private def assertSimpleBinary(snapshot: AstVisitProfileSnapshot): Unit = {
    assert(snapshot.totalVisits == 3)
    assert(snapshot.oldDispatchArmCounts(AstVisitProfiler.DispatchArm.BinaryOp) == 1)
    assert(snapshot.oldDispatchArmCounts(AstVisitProfiler.DispatchArm.Val) == 2)
    assert(snapshot.normalTagCounts(ExprTags.BinaryOp) == 1)
    assert(snapshot.normalTagCounts(ExprTags.`Val.Literal`) == 2)
    assert(snapshot.invalidNodeCounts(AstVisitProfiler.InvalidNode.Self) == 0)
  }

  private def assertInvalidSelf(snapshot: AstVisitProfileSnapshot): Unit = {
    assert(snapshot.totalVisits == 1)
    assert(snapshot.oldDispatchArmCounts(AstVisitProfiler.DispatchArm.Invalid) == 1)
    assert(snapshot.invalidNodeCounts(AstVisitProfiler.InvalidNode.Self) == 1)
    assert(snapshot.normalTagCounts(ExprTags.ValidId) == 0)
    assert(snapshot.normalTagCounts(ExprTags.BinaryOp) == 0)
  }

  val tests: Tests = Tests {
    test("old evaluator counts normal tags and dispatch arms") {
      val expr = Expr.BinaryOp(
        dummyPos,
        Val.Num(dummyPos, 1),
        Expr.BinaryOp.OP_+,
        Val.Num(dummyPos, 2)
      )
      assertSimpleBinary(runExpr(expr, useNewEvaluator = false))
    }

    test("new evaluator counts normal tags and dispatch arms") {
      val expr = Expr.BinaryOp(
        dummyPos,
        Val.Num(dummyPos, 1),
        Expr.BinaryOp.OP_+,
        Val.Num(dummyPos, 2)
      )
      assertSimpleBinary(runExpr(expr, useNewEvaluator = true))
    }

    test("old evaluator counts invalid node namespace separately") {
      assertInvalidSelf(runExpr(Expr.Self(dummyPos), useNewEvaluator = false))
    }

    test("new evaluator counts invalid node namespace separately") {
      assertInvalidSelf(runExpr(Expr.Self(dummyPos), useNewEvaluator = true))
    }
  }
}
