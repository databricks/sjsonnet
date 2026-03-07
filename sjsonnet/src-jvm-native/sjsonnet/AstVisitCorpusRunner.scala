package sjsonnet

import java.time.LocalDate

import sjsonnet.stdlib.NativeRegex
import ujson.Value

object AstVisitCorpusRunner {
  private val workspaceRoot: os.Path =
    sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  private val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources"
  private val defaultOutputPath: os.Path = workspaceRoot / "bench" / "AST_VISIT_COUNTS.md"

  private val extVars = Map(
    "var1" -> "\"test\"",
    "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)""",
    "stringVar" -> "\"2 + 2\"",
    "codeVar" -> "3 + 3",
    "errorVar" -> "error 'xxx'",
    "staticErrorVar" -> ")",
    "UndeclaredX" -> "x",
    "selfRecursiveVar" -> """[42, std.extVar("selfRecursiveVar")[0] + 1]""",
    "mutuallyRecursiveVar1" -> """[42, std.extVar("mutuallyRecursiveVar2")[0] + 1]""",
    "mutuallyRecursiveVar2" -> """[42, std.extVar("mutuallyRecursiveVar1")[0] + 1]"""
  )
  private val tlaVars = Map(
    "var1" -> "\"test\"",
    "var2" -> """{"x": 1, "y": 2}"""
  )

  private val stdModule: Val.Obj = new sjsonnet.stdlib.StdLibModule(
    nativeFunctions = Map(
      "jsonToString" -> new Val.Builtin1("jsonToString", "x") {
        override def evalRhs(arg1: Eval, ev: EvalScope, pos: Position): Val = {
          Val.Str(
            pos,
            Materializer
              .apply0(
                arg1.value,
                MaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":")
              )(ev)
              .toString
          )
        }
      },
      "nativeError" -> new Val.Builtin0("nativeError") {
        override def evalRhs(ev: EvalScope, pos: Position): Val =
          Error.fail("native function error")
      },
      "nativePanic" -> new Val.Builtin0("nativePanic") {
        override def evalRhs(ev: EvalScope, pos: Position): Val =
          throw new RuntimeException("native function panic")
      }
    ) ++ NativeRegex.functions
  ).module

  private val goTestDataSkippedTests: Set[String] = Set(
    "builtinBase64_string_high_codepoint.jsonnet"
  )

  private val successGoldenPrefixes = Seq(
    "sjsonnet.Error",
    "sjsonnet.ParseError",
    "sjsonnet.StaticError",
    "RUNTIME ERROR"
  )

  final case class CorpusFile(suite: String, path: os.Path)
  final case class SuiteSummary(
      suite: String,
      successFiles: Int,
      expectedErrorFiles: Int,
      skippedFiles: Seq[String])
  final case class CountRow(name: String, count: Long)
  final case class CorpusRunSummary(
      oldSnapshot: AstVisitProfileSnapshot,
      newSnapshot: AstVisitProfileSnapshot,
      suiteSummaries: Seq[SuiteSummary],
      totalSuccessFiles: Int,
      totalExpectedErrorFiles: Int,
      totalSkippedFiles: Int,
      oldDurationMs: Long,
      newDurationMs: Long,
      countsMatch: Boolean)

  def main(args: Array[String]): Unit = {
    val outputPath = args.toList match {
      case Nil         => defaultOutputPath
      case path :: Nil => resolveOutputPath(path)
      case _           =>
        throw new IllegalArgumentException(
          "Usage: AstVisitCorpusRunner [output-markdown-path]"
        )
    }

    val (corpus, suiteSummaries) = discoverCorpus()
    val summary = executeCorpus(corpus, suiteSummaries)
    val markdown = renderMarkdown(summary, outputPath)
    os.write.over(outputPath, markdown)

    println(
      s"AST visit corpus run complete: ${summary.totalSuccessFiles} success files, ${summary.totalExpectedErrorFiles} expected-error files skipped, ${summary.totalSkippedFiles} additional skips."
    )
    println(
      s"Old evaluator total visits: ${summary.oldSnapshot.totalVisits}, new evaluator total visits: ${summary.newSnapshot.totalVisits}, counts match: ${summary.countsMatch}"
    )
    println(s"Wrote $outputPath")
  }

  private def resolveOutputPath(path: String): os.Path =
    os.Path(path, workspaceRoot)

  private def discoverCorpus(): (Seq[CorpusFile], Seq[SuiteSummary]) = {
    val suiteNames = Seq("test_suite", "go_test_suite", "new_test_suite")
    val summaries = scala.collection.mutable.ArrayBuffer.empty[SuiteSummary]
    val files = scala.collection.mutable.ArrayBuffer.empty[CorpusFile]

    suiteNames.foreach { suite =>
      val dir = testSuiteRoot / suite
      val skipped = scala.collection.mutable.ArrayBuffer.empty[String]
      var successFiles = 0
      var expectedErrorFiles = 0

      os.list(dir)
        .filter(p => p.ext == "jsonnet")
        .sortBy(_.last)
        .foreach { file =>
          val skip = skipReason(suite, file)
          if (skip != null) skipped += s"${file.last} (${skip})"
          else if (isExpectedErrorFile(file)) expectedErrorFiles += 1
          else {
            successFiles += 1
            files += CorpusFile(suite, file)
          }
        }

      summaries += SuiteSummary(suite, successFiles, expectedErrorFiles, skipped.toSeq)
    }

    (files.toSeq, summaries.toSeq)
  }

  private def skipReason(suite: String, file: os.Path): String = {
    if (suite == "go_test_suite" && goTestDataSkippedTests.contains(file.last)) {
      "excluded to match FileTests corpus"
    } else if (suite == "new_test_suite" && file.last.contains("-js")) {
      "JS-only corpus file"
    } else null
  }

  private def isExpectedErrorFile(file: os.Path): Boolean = {
    val goldenContent = os.read(os.Path(file.toString + ".golden")).stripLineEnd
    goldenContent.contains("java.lang.StackOverflowError") ||
    successGoldenPrefixes.exists(goldenContent.startsWith)
  }

  private def executeCorpus(
      corpus: Seq[CorpusFile],
      suiteSummaries: Seq[SuiteSummary]): CorpusRunSummary = {
    val oldProfiler = new AstVisitProfiler
    val newProfiler = new AstVisitProfiler
    val oldParseCache = new DefaultParseCache
    val newParseCache = new DefaultParseCache
    val oldStats = new DebugStats
    val newStats = new DebugStats
    oldStats.astVisitProfiler = oldProfiler
    newStats.astVisitProfiler = newProfiler

    var oldDurationNs = 0L
    var newDurationNs = 0L

    corpus.foreach { file =>
      println(s"Running ${file.suite}/${file.path.last}")

      val oldStart = System.nanoTime()
      val oldResult = eval(file, useNewEvaluator = false, oldParseCache, oldStats)
      oldDurationNs += System.nanoTime() - oldStart

      val newStart = System.nanoTime()
      val newResult = eval(file, useNewEvaluator = true, newParseCache, newStats)
      newDurationNs += System.nanoTime() - newStart

      if (oldResult != newResult) {
        throw new IllegalStateException(
          s"Evaluator mismatch for ${file.suite}/${file.path.last}: old=${preview(oldResult)}, new=${preview(newResult)}"
        )
      }
    }

    val oldSnapshot = oldProfiler.snapshot()
    val newSnapshot = newProfiler.snapshot()
    CorpusRunSummary(
      oldSnapshot,
      newSnapshot,
      suiteSummaries,
      totalSuccessFiles = corpus.length,
      totalExpectedErrorFiles = suiteSummaries.map(_.expectedErrorFiles).sum,
      totalSkippedFiles = suiteSummaries.map(_.skippedFiles.length).sum,
      oldDurationMs = oldDurationNs / 1000000L,
      newDurationMs = newDurationNs / 1000000L,
      countsMatch = snapshotsEqual(oldSnapshot, newSnapshot)
    )
  }

  private def eval(
      corpusFile: CorpusFile,
      useNewEvaluator: Boolean,
      parseCache: ParseCache,
      debugStats: DebugStats): Either[String, Value] = {
    val interp = new Interpreter(
      key => extVars.get(key).map(ExternalVariable.code),
      key => tlaVars.get(key).map(ExternalVariable.code),
      OsPath(testSuiteRoot / corpusFile.suite),
      importer = new sjsonnet.SjsonnetMainBase.SimpleImporter(Array.empty[Path].toIndexedSeq),
      parseCache = parseCache,
      settings = new Settings(useNewEvaluator = useNewEvaluator),
      storePos = null,
      logger = null,
      std = stdModule,
      variableResolver = _ => None,
      debugStats = debugStats
    )
    interp.interpret(os.read(corpusFile.path), OsPath(corpusFile.path))
  }

  private def snapshotsEqual(a: AstVisitProfileSnapshot, b: AstVisitProfileSnapshot): Boolean = {
    java.util.Arrays.equals(a.oldDispatchArmCounts, b.oldDispatchArmCounts) &&
    java.util.Arrays.equals(a.normalTagCounts, b.normalTagCounts) &&
    java.util.Arrays.equals(a.invalidNodeCounts, b.invalidNodeCounts)
  }

  private def preview(result: Either[String, Value]): String = result match {
    case Left(err) => err.linesIterator.take(1).mkString
    case Right(v)  => v.render().take(120)
  }

  private def renderMarkdown(summary: CorpusRunSummary, outputPath: os.Path): String = {
    val oldDispatchTop =
      topRows(summary.oldSnapshot.oldDispatchArmCounts, AstVisitProfiler.DispatchArmNames)
    val oldTagTop = topRows(summary.oldSnapshot.normalTagCounts, AstVisitProfiler.NormalTagNames, 1)
    val oldInvalidTop =
      topRows(summary.oldSnapshot.invalidNodeCounts, AstVisitProfiler.InvalidNodeNames)
    val newDispatchTop =
      topRows(summary.newSnapshot.oldDispatchArmCounts, AstVisitProfiler.DispatchArmNames)
    val newTagTop = topRows(summary.newSnapshot.normalTagCounts, AstVisitProfiler.NormalTagNames, 1)
    val newInvalidTop =
      topRows(summary.newSnapshot.invalidNodeCounts, AstVisitProfiler.InvalidNodeNames)

    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    lines += "# AST Visit Counts"
    lines += ""
    lines += s"Generated by `sjsonnet.AstVisitCorpusRunner` on ${LocalDate.now()}."
    lines += ""
    lines += "## Corpus"
    lines += ""
    lines += "Primary corpus: success-only files from the JVM/native test corpus under `sjsonnet/test/resources/{test_suite,go_test_suite,new_test_suite}`."
    lines += "Expected-error files were intentionally excluded from counting so the report reflects steady-state successful evaluation paths; JS-only files and the existing `go_test_suite/builtinBase64_string_high_codepoint.jsonnet` skip were also excluded."
    lines += ""
    lines += "### Corpus summary"
    lines += ""
    lines ++= Seq(
      "| Suite | Success files run | Expected-error files skipped | Other skipped |",
      "| --- | ---: | ---: | ---: |"
    )
    summary.suiteSummaries.foreach { suiteSummary =>
      lines += s"| `${suiteSummary.suite}` | ${suiteSummary.successFiles} | ${suiteSummary.expectedErrorFiles} | ${suiteSummary.skippedFiles.length} |"
    }
    lines += s"| **Total** | **${summary.totalSuccessFiles}** | **${summary.totalExpectedErrorFiles}** | **${summary.totalSkippedFiles}** |"
    lines += ""
    lines += "### Explicit skips"
    lines += ""
    summary.suiteSummaries.foreach { suiteSummary =>
      lines += s"- `${suiteSummary.suite}`: ${formatSkipped(suiteSummary.skippedFiles)}"
    }
    lines += ""
    lines += "### Cross-check"
    lines += ""
    lines += s"- Every success-only file was executed with both evaluators and required to produce identical successful output."
    lines += s"- Aggregate counter arrays match between evaluators: `${summary.countsMatch}`."
    lines += s"- Old evaluator elapsed time across the corpus: `${summary.oldDurationMs} ms`."
    lines += s"- New evaluator elapsed time across the corpus: `${summary.newDurationMs} ms`."
    lines += ""
    lines += "## Old evaluator summary"
    lines += ""
    lines += s"- Total `visitExpr` calls: `${summary.oldSnapshot.totalVisits}`"
    lines += s"- Top old-dispatch arm: `${formatTop(oldDispatchTop.headOption)}`"
    lines += s"- Top Expr tag: `${formatTop(oldTagTop.headOption)}`"
    lines += s"- Top invalid-node case: `${formatTop(oldInvalidTop.headOption)}`"
    lines += ""
    lines += renderSection(
      "Old evaluator dispatch-arm counts",
      summary.oldSnapshot.oldDispatchArmCounts,
      AstVisitProfiler.DispatchArmNames,
      0,
      summary.oldSnapshot.totalVisits
    )
    lines += ""
    lines += renderSection(
      "Old evaluator normal Expr tag counts",
      summary.oldSnapshot.normalTagCounts,
      AstVisitProfiler.NormalTagNames,
      1,
      summary.oldSnapshot.totalVisits
    )
    lines += ""
    lines += renderSection(
      "Old evaluator invalid-node counts",
      summary.oldSnapshot.invalidNodeCounts,
      AstVisitProfiler.InvalidNodeNames,
      0,
      summary.oldSnapshot.totalVisits
    )
    lines += ""
    lines += "## New evaluator summary"
    lines += ""
    lines += s"- Total `visitExpr` calls: `${summary.newSnapshot.totalVisits}`"
    lines += s"- Top old-dispatch arm: `${formatTop(newDispatchTop.headOption)}`"
    lines += s"- Top Expr tag: `${formatTop(newTagTop.headOption)}`"
    lines += s"- Top invalid-node case: `${formatTop(newInvalidTop.headOption)}`"
    lines += ""
    lines += renderSection(
      "New evaluator dispatch-arm counts",
      summary.newSnapshot.oldDispatchArmCounts,
      AstVisitProfiler.DispatchArmNames,
      0,
      summary.newSnapshot.totalVisits
    )
    lines += ""
    lines += renderSection(
      "New evaluator normal Expr tag counts",
      summary.newSnapshot.normalTagCounts,
      AstVisitProfiler.NormalTagNames,
      1,
      summary.newSnapshot.totalVisits
    )
    lines += ""
    lines += renderSection(
      "New evaluator invalid-node counts",
      summary.newSnapshot.invalidNodeCounts,
      AstVisitProfiler.InvalidNodeNames,
      0,
      summary.newSnapshot.totalVisits
    )
    lines += ""
    lines += "## Reproduction"
    lines += ""
    lines += s"- Command: `./mill sjsonnet.jvm[3.3.7].runMain sjsonnet.AstVisitCorpusRunner ${outputPath}`"
    lines += "- Notes: this todo only adds instrumentation, corpus execution, and markdown output. It intentionally does **not** reorder evaluator dispatch or renumber `Expr.tag` values yet."

    lines.mkString("\n") + "\n"
  }

  private def renderSection(
      title: String,
      counts: Array[Long],
      names: Array[String],
      startIndex: Int,
      totalVisits: Long): String = {
    val rows = topRows(counts, names, startIndex, counts.length)
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    lines += s"### ${title}"
    lines += ""
    lines += "| Rank | Name | Count | Share |"
    lines += "| ---: | --- | ---: | ---: |"
    rows.zipWithIndex.foreach { case (row, idx) =>
      lines += s"| ${idx + 1} | `${row.name}` | ${row.count} | ${formatShare(row.count, totalVisits)} |"
    }
    lines.mkString("\n")
  }

  private def topRows(
      counts: Array[Long],
      names: Array[String],
      startIndex: Int = 0,
      limit: Int = 10): Seq[CountRow] = {
    counts.indices
      .drop(startIndex)
      .filter(i => i < names.length)
      .map(i => CountRow(names(i), counts(i)))
      .sortBy(row => (-row.count, row.name))
      .take(limit)
      .toSeq
  }

  private def formatShare(count: Long, total: Long): String = {
    if (total == 0) "0.00%"
    else f"${count.toDouble * 100.0 / total}%.2f%%"
  }

  private def formatSkipped(skipped: Seq[String]): String = {
    if (skipped.isEmpty) "none"
    else skipped.map(s => s"`$s`").mkString(", ")
  }

  private def formatTop(row: Option[CountRow]): String = row match {
    case Some(value) => s"${value.name} (${value.count})"
    case None        => "none"
  }
}
