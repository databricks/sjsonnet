package sjsonnet

import java.util

class ProfilingEvaluator(resolver: CachedResolver,
                         extVars: Map[String, ujson.Value],
                         wd: Path,
                         preserveOrder: Boolean = false,
                         strict: Boolean)
  extends Evaluator(resolver, extVars, wd, preserveOrder, strict) {

  class Box {
    var time: Long = 0
    var count: Long = 0
  }

  val data = new util.IdentityHashMap[Expr, Box]()
  var parent: Box = null

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = {
    var box = data.get(e)
    if(box == null) {
      box = new Box
      data.put(e, box)
    }
    val prevParent = parent
    parent = box
    val t0 = System.nanoTime()
    try super.visitExpr(e) finally {
      val t1 = System.nanoTime()
      val t = t1 - t0
      parent = prevParent
      box.time += t
      box.count += 1
      if(parent != null) parent.time -= t
    }
  }
}
