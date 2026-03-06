package sjsonnet

import java.io.{BufferedWriter, FileWriter}
import java.util

import scala.collection.mutable.ListBuffer

final case class BoxId(name: String, fileName: String, offset: Int)

final case class Box(id: BoxId, lineCol: () => String, count: Long, selfTimeNs: Long)

final case class ProfilingResult(boxes: Seq[Box])

object ProfilingResult {
  def merge(results: Seq[ProfilingResult]): ProfilingResult = {
    if (results.size == 1) return results.head
    val mergedBoxes = results
      .flatMap(_.boxes)
      .groupBy(_.id)
      .values
      .map { boxes =>
        Box(
          id = boxes.head.id,
          lineCol = boxes.head.lineCol,
          count = boxes.map(_.count).sum,
          selfTimeNs = boxes.map(_.selfTimeNs).sum
        )
      }
      .toSeq
    ProfilingResult(mergedBoxes)
  }
}

sealed abstract class ProfileOutputFormat
object ProfileOutputFormat {
  case object Text extends ProfileOutputFormat
  case object FlameGraph extends ProfileOutputFormat
}

/**
 * Profiler that measures self-time for every evaluated expression, and optionally records call
 * stacks for flame graph output.
 *
 * Hooks into `Evaluator.visitExpr` to time each expression. The profiler is single-threaded and not
 * thread-safe.
 */
final class Profiler(val format: ProfileOutputFormat, wd: Path) {

  private class ExprBox(val expr: Expr) {
    val id: BoxId = {
      val fileName = Option(expr.pos)
        .map(_.currentFile.relativeToString(wd))
        .getOrElse("<unknown>")

      val name = {
        val exprName = expr.getClass.getName.split('.').last.split('$').last
        val detail: Option[String] = expr match {
          case a: Expr.Apply           => Some(Expr.callTargetName(a.value))
          case a: Expr.Apply0          => Some(Expr.callTargetName(a.value))
          case a: Expr.Apply1          => Some(Expr.callTargetName(a.value))
          case a: Expr.Apply2          => Some(Expr.callTargetName(a.value))
          case a: Expr.Apply3          => Some(Expr.callTargetName(a.value))
          case a: Expr.ApplyBuiltin    => Some(a.func.functionName)
          case a: Expr.ApplyBuiltin0   => Some(a.func.functionName)
          case a: Expr.ApplyBuiltin1   => Some(a.func.functionName)
          case a: Expr.ApplyBuiltin2   => Some(a.func.functionName)
          case a: Expr.ApplyBuiltin3   => Some(a.func.functionName)
          case a: Expr.ApplyBuiltin4   => Some(a.func.functionName)
          case u: Expr.UnaryOp         => Some(Expr.UnaryOp.name(u.op))
          case b: Expr.BinaryOp        => Some(Expr.BinaryOp.name(b.op))
          case _: Expr.Comp            => Some("array comprehension")
          case _: Expr.ObjBody.ObjComp => Some("object comprehension")
          case _                       => None
        }
        exprName + detail.map(n => s" ($n)").getOrElse("")
      }

      BoxId(name, fileName, if (expr.pos == null) 0 else expr.pos.offset)
    }

    var lineCol: () => String = _
    var count: Long = 0
    var selfTimeNs: Long = 0

    def toBox: Box = Box(id, lineCol, count, selfTimeNs)
  }

  private val data = new util.IdentityHashMap[Expr, ExprBox]
  private var current: ExprBox = _

  // Flame graph state (only used when format == FlameGraph)
  private val fgStack = new util.ArrayDeque[String]()
  private val fgCounts = new util.HashMap[String, java.lang.Long]()
  private var fgDepth: Int = 0

  private def isFlameGraphFrame(e: Expr): Boolean = e match {
    case _: Expr.Apply | _: Expr.Apply0 | _: Expr.Apply1 | _: Expr.Apply2 | _: Expr.Apply3 |
        _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin0 | _: Expr.ApplyBuiltin1 |
        _: Expr.ApplyBuiltin2 | _: Expr.ApplyBuiltin3 | _: Expr.ApplyBuiltin4 | _: Expr.Comp |
        _: Expr.ObjBody.ObjComp =>
      true
    case _ => false
  }

  private def isBuiltin(e: Expr): Boolean = e match {
    case _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin0 | _: Expr.ApplyBuiltin1 |
        _: Expr.ApplyBuiltin2 | _: Expr.ApplyBuiltin3 | _: Expr.ApplyBuiltin4 =>
      true
    case _ => false
  }

  private def flameGraphFrameName(e: Expr): String = {
    val name = e.exprErrorString
    if (isBuiltin(e) || e.pos == null) name
    else {
      val file = e.pos.currentFile.relativeToString(wd)
      s"$name ($file)"
    }
  }

  private def getOrCreate(e: Expr): ExprBox = {
    var box = data.get(e)
    if (box == null) {
      box = new ExprBox(e)
      data.put(e, box)
    }
    box
  }

  /**
   * Must be called before evaluating `e`. Returns an opaque token for [[exit]]. The low bit of the
   * returned long encodes whether a flamegraph frame was pushed; the upper bits store the saved
   * fgDepth.
   */
  def enter(e: Expr): (AnyRef, Int) = {
    val box = getOrCreate(e)
    box.count += 1
    val parent = current
    current = box

    val now = System.nanoTime()
    box.selfTimeNs -= now
    if (parent != null) parent.selfTimeNs += now

    val prevFgDepth = fgDepth
    if (format == ProfileOutputFormat.FlameGraph && isFlameGraphFrame(e)) {
      fgStack.push(flameGraphFrameName(e))
      fgDepth += 1
      val key = foldedStack()
      val prev = fgCounts.get(key)
      fgCounts.put(key, if (prev == null) 1L else prev + 1L)
    }

    (parent, prevFgDepth)
  }

  /** Must be called after evaluating `e`, with the token returned by [[enter]]. */
  def exit(saved: (AnyRef, Int)): Unit = {
    val now = System.nanoTime()
    val box = current
    if (box != null) box.selfTimeNs += now

    val (parentRef, prevFgDepth) = saved
    if (parentRef != null) {
      val parent = parentRef.asInstanceOf[ExprBox]
      current = parent
      parent.selfTimeNs -= now
    } else {
      current = null
    }

    while (fgDepth > prevFgDepth) {
      fgStack.pop()
      fgDepth -= 1
    }
  }

  def initLineColLookup(prettyIndex: Position => Option[(Int, Int)]): Unit = {
    val it = data.values().iterator()
    while (it.hasNext) {
      val box = it.next()
      box.lineCol = () => {
        Option(box.expr.pos)
          .flatMap(prettyIndex)
          .map { case (l, c) => s"${l + 1}:$c" }
          .getOrElse("?:?")
      }
    }
  }

  def collectResult(): ProfilingResult = {
    val boxes = new scala.collection.mutable.ArrayBuffer[Box](data.size())
    val it = data.values().iterator()
    while (it.hasNext) boxes += it.next().toBox
    new ProfilingResult(boxes.toSeq)
  }

  def writeTo(path: String, prettyIndex: Position => Option[(Int, Int)]): Unit = {
    initLineColLookup(prettyIndex)
    format match {
      case ProfileOutputFormat.Text =>
        val result = collectResult()
        val lines = ProfilePrinter.formatResult(result)
        val w = new BufferedWriter(new FileWriter(path))
        try
          lines.foreach { line =>
            w.write(line); w.newLine()
          }
        finally w.close()

      case ProfileOutputFormat.FlameGraph =>
        val w = new BufferedWriter(new FileWriter(path))
        try {
          val it = fgCounts.entrySet().iterator()
          while (it.hasNext) {
            val e = it.next()
            w.write(e.getKey)
            w.write(' ')
            w.write(e.getValue.toString)
            w.newLine()
          }
        } finally w.close()
    }
  }

  private def foldedStack(): String = {
    val sb = new StringBuilder
    val it = fgStack.descendingIterator()
    var first = true
    while (it.hasNext) {
      if (!first) sb.append(';')
      sb.append(it.next())
      first = false
    }
    sb.toString
  }
}

object ProfilePrinter {
  private val TopExpressionsCount = 50
  private val TopAggregatedCount = 10

  def formatResult(result: ProfilingResult): Seq[String] = {
    val lines = new ListBuffer[String]

    val overhead = estimateSystemNanoTimeOverhead
    val boxes = result.boxes.map(box =>
      box.copy(selfTimeNs = math.max(0, box.selfTimeNs - box.count * overhead))
    )

    val totalTimeNs = boxes.map(_.selfTimeNs).sum
    lines += s"Total evaluation time: ${nsToSec(totalTimeNs)}"

    val evaluations = boxes.map(_.count).sum
    lines += s"Unique expressions: ${result.boxes.size}, expression evaluations: $evaluations"
    lines += s"Times adjusted based on estimated System.nanoTime overhead: $overhead ns"

    lines += ""
    lines += s"Top $TopExpressionsCount by self time:"
    lines += ""

    lines += formatRow("count", "self", "%", "cumul", "%", "location")
    lines ++= format(boxes, totalTimeNs, agg = None, TopExpressionsCount)

    lines += ""
    lines += s"Top $TopAggregatedCount aggregated by file name:"
    lines += ""

    lines += formatRow("count", "self", "%", "cumul", "%", "file")
    lines ++= format(boxes, totalTimeNs, agg = Some(box => box.id.fileName), TopAggregatedCount)

    lines += ""
    lines += s"Top $TopAggregatedCount aggregated by expression:"
    lines += ""

    lines += formatRow("count", "self", "%", "cumul", "%", "expression")
    lines ++= format(boxes, totalTimeNs, agg = Some(box => box.id.name), TopAggregatedCount)

    lines.toList
  }

  private def format(
      boxes: Seq[Box],
      totalTimeNs: Long,
      agg: Option[Box => String],
      top: Int): Seq[String] = {
    case class Stats(name: () => String, count: Long, selfTimeNs: Long)

    val aggregated = agg match {
      case Some(agg) =>
        boxes
          .groupBy(agg)
          .map { case (aggregatedBy, boxes) =>
            Stats(
              name = () => aggregatedBy,
              count = boxes.map(_.count).sum,
              selfTimeNs = boxes.map(_.selfTimeNs).sum
            )
          }

      case _ =>
        boxes.map { box =>
          Stats(
            () => f"${box.id.fileName}:${box.lineCol()} ${box.id.name}",
            box.count,
            box.selfTimeNs
          )
        }
    }

    val totalNs = math.max(1, totalTimeNs)
    var sum = 0L

    aggregated.toSeq
      .sortBy(-_.selfTimeNs)
      .take(top)
      .map { stats =>
        sum = sum + stats.selfTimeNs
        formatRow(
          count = stats.count.toString,
          selfTime = nsToSec(stats.selfTimeNs),
          selfPerc = f"${stats.selfTimeNs * 100.0 / totalNs}%.1f",
          selfSum = nsToSec(sum),
          sumPerc = f"${sum * 100.0 / totalNs}%.1f",
          name = stats.name()
        )
      }
  }

  private def estimateSystemNanoTimeOverhead: Long = {
    val samples: Array[Long] = new Array[Long](1000 * 1000)
    for (i <- samples.indices) {
      val start = System.nanoTime()
      samples(i) = System.nanoTime() - start
    }
    samples.sorted.apply(samples.length / 2)
  }

  private def formatRow(
      count: String,
      selfTime: String,
      selfPerc: String,
      selfSum: String,
      sumPerc: String,
      name: String): String =
    f"$count%12s  $selfTime%8s $selfPerc%4s  $selfSum%8s $sumPerc%4s  $name"

  private def nsToSec(ns: Long): String = {
    val sec = ns / 1000000000.0
    f"$sec%.2fs"
  }
}
