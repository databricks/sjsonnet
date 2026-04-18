package sjsonnet.bench

import sjsonnet.*

import java.io.StringWriter

/**
 * Profile the frequency of each ExprTag in visitExpr calls across benchmark workloads. Run with:
 * ./mill bench.runMain sjsonnet.bench.ExprTagProfile [file1.jsonnet file2.jsonnet ...]
 *
 * If no files are given, profiles ALL .jsonnet files under bench/resources/.
 */
object ExprTagProfile {

  private val tagNames = Array(
    "UNTAGGED", // 0
    "ValidId", // 1
    "BinaryOp", // 2
    "Select", // 3
    "Val.Literal", // 4
    "Val.Func", // 5
    "ApplyBuiltin0", // 6
    "ApplyBuiltin1", // 7
    "ApplyBuiltin2", // 8
    "ApplyBuiltin3", // 9
    "ApplyBuiltin4", // 10
    "And", // 11
    "Or", // 12
    "UnaryOp", // 13
    "Apply1", // 14
    "Lookup", // 15
    "Function", // 16
    "LocalExpr", // 17
    "Apply", // 18
    "IfElse", // 19
    "Apply3", // 20
    "ObjBody.MemberList", // 21
    "Apply2", // 22
    "AssertExpr", // 23
    "ApplyBuiltin", // 24
    "Comp", // 25
    "Arr", // 26
    "SelectSuper", // 27
    "LookupSuper", // 28
    "InSuper", // 29
    "ObjExtend", // 30
    "ObjBody.ObjComp", // 31
    "Slice", // 32
    "Import", // 33
    "Apply0", // 34
    "ImportStr", // 35
    "ImportBin", // 36
    "Error" // 37
  )

  def main(args: Array[String]): Unit = {
    val wd = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
    val benchRoot = wd / "bench" / "resources"

    val files =
      if (args.nonEmpty) args.map(os.RelPath(_)).toSeq
      else
        os.walk(benchRoot)
          .filter(_.ext == "jsonnet")
          .map(_.relativeTo(wd))
          .sorted

    val globalCounts = new Array[Long](40)
    var globalTotal = 0L
    val perFile = scala.collection.mutable.ArrayBuffer[(String, Long, Array[Long])]()

    for (rel <- files) {
      val counts = new Array[Long](40)
      val filePath = OsPath(wd / rel)
      val content =
        try os.read(wd / rel)
        catch { case _: Exception => System.err.println(s"SKIP (read error): $rel"); "" }
      if (content.nonEmpty) {
        val ok =
          try {
            val interp = new Interpreter(
              Map.empty[String, String],
              Map.empty[String, String],
              OsPath(wd),
              importer = new SjsonnetMainBase.SimpleImporter(
                Seq(OsPath(wd), OsPath(wd / "bench"), OsPath(wd / "bench" / "resources")),
                None
              ),
              parseCache = new DefaultParseCache,
              settings = new Settings(maxStack = 100000)
            ) {
              override def createEvaluator(
                  resolver: CachedResolver,
                  extVars: String => Option[Expr],
                  wd: Path,
                  settings: Settings): Evaluator =
                new Evaluator(resolver, extVars, wd, settings) {
                  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = {
                    val t = e.tag & 0xff
                    if (t < counts.length) counts(t) += 1
                    super.visitExpr(e)
                  }
                }
            }
            val writer = new StringWriter
            val renderer = new Renderer(writer, indent = 3)
            interp.interpret0(content, filePath, renderer) match {
              case Right(_) => true
              case Left(e)  =>
                System.err.println(s"ERROR: $rel: $e")
                false
            }
          } catch {
            case e: StackOverflowError =>
              System.err.println(s"SKIP (StackOverflow): $rel")
              false
            case e: Exception =>
              System.err.println(s"SKIP (${e.getClass.getSimpleName}): $rel")
              false
          }

        val total = counts.sum
        if (ok && total > 0) {
          perFile += ((rel.toString, total, counts.clone()))
          var i = 0
          while (i < counts.length) {
            globalCounts(i) += counts(i)
            i += 1
          }
          globalTotal += total
        }
      }
    }

    // Per-file summary
    println("\n" + "=" * 100)
    println("PER-FILE SUMMARY")
    println("=" * 100)
    for ((file, total, counts) <- perFile.sortBy(-_._2)) {
      val sorted = counts.zipWithIndex.filter(_._1 > 0).sortBy(-_._1)
      val top3 = sorted
        .take(3)
        .map { case (c, idx) =>
          val name = if (idx < tagNames.length) tagNames(idx) else s"tag=$idx"
          f"$name(${c * 100.0 / total}%.0f%%)"
        }
        .mkString(", ")
      println(f"  $file%-65s total=$total%10d  top3: $top3")
    }

    // Global aggregation
    println("\n" + "=" * 100)
    println(f"GLOBAL AGGREGATE (${perFile.size} files, $globalTotal%,d total visitExpr calls)")
    println("=" * 100)
    val globalSorted = globalCounts.zipWithIndex.filter(_._1 > 0).sortBy(-_._1)
    var cumPct = 0.0
    println(f"  ${"Rank"}%-5s ${"ExprTag"}%-20s ${"Count"}%12s ${"Pct"}%7s ${"Cumulative"}%10s")
    println("  " + "-" * 60)
    for (((count, idx), rank) <- globalSorted.zipWithIndex) {
      val name = if (idx < tagNames.length) tagNames(idx) else s"tag=$idx"
      val pct = count * 100.0 / globalTotal
      cumPct += pct
      println(f"  ${rank + 1}%-5d $name%-20s $count%,12d $pct%6.1f%% $cumPct%9.1f%%")
    }
  }
}
