package sjsonnet

/**
 * Mutable counters and timing data for `--debug-stats` mode. A single instance is created when the
 * flag is set and threaded through the Interpreter, Evaluator, and ParseCache so that each
 * subsystem can increment its own counters.
 *
 * All fields are plain `var`s — evaluation is single-threaded, so no synchronisation is needed.
 */
final class DebugStats {

  // -- Lazy --
  var lazyCreated: Long = 0

  // -- Calls --
  var functionCalls: Long = 0
  var builtinCalls: Long = 0

  // -- Stack --
  var maxStackDepth: Int = 0

  // -- Comprehensions --
  var arrayCompIterations: Long = 0
  var objectCompIterations: Long = 0

  // -- Parse / import --
  var filesParsed: Long = 0
  var importCalls: Long = 0
  var importCacheHits: Long = 0

  // -- Timing (nanoseconds) --
  var parseTimeNs: Long = 0
  var evalTimeNs: Long = 0
  var materializeTimeNs: Long = 0

  def totalTimeNs: Long = evalTimeNs + materializeTimeNs

  def format(): String = {
    val sb = new StringBuilder
    sb.append("=== sjsonnet debug stats ===\n")

    sb.append(formatLine("lazy_created", lazyCreated))
    sb.append(formatLine("function_calls", functionCalls))
    sb.append(formatLine("builtin_calls", builtinCalls))
    sb.append(formatLine("max_stack_depth", maxStackDepth))
    sb.append(formatLine("array_comp_iterations", arrayCompIterations))
    sb.append(formatLine("object_comp_iterations", objectCompIterations))
    sb.append(formatLine("files_parsed", filesParsed))
    sb.append(formatLine("import_calls", importCalls))
    sb.append(formatLine("import_cache_hits", importCacheHits))
    sb.append(formatTimeLine("parse_time", parseTimeNs))
    sb.append(formatTimeLine("eval_time", evalTimeNs))
    sb.append(formatTimeLine("materialize_time", materializeTimeNs))
    sb.append(formatTimeLine("total_time", totalTimeNs))

    val rt = Runtime.getRuntime
    val usedBytes = rt.totalMemory() - rt.freeMemory()
    val maxBytes = rt.maxMemory()
    sb.append(formatBytesLine("heap_used", usedBytes))
    sb.append(formatBytesLine("heap_max", maxBytes))

    sb.toString
  }

  private def formatLine(label: String, value: Long): String =
    f"$label%-25s $value%d%n"

  private def formatTimeLine(label: String, ns: Long): String =
    f"$label%-25s ${formatNs(ns)}%s%n"

  private def formatBytesLine(label: String, bytes: Long): String =
    f"$label%-25s ${formatBytes(bytes)}%s%n"

  private def formatNs(ns: Long): String = {
    if (ns < 1000L) s"${ns}ns"
    else if (ns < 1000000L) f"${ns / 1000.0}%.1fμs"
    else if (ns < 1000000000L) f"${ns / 1000000.0}%.1fms"
    else f"${ns / 1000000000.0}%.3fs"
  }

  private def formatBytes(bytes: Long): String = {
    if (bytes < 1024L) s"${bytes}B"
    else if (bytes < 1024L * 1024) f"${bytes / 1024.0}%.1fKB"
    else if (bytes < 1024L * 1024 * 1024) f"${bytes / (1024.0 * 1024)}%.1fMB"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.2fGB"
  }
}

/**
 * A [[ParseCache]] wrapper that counts and times actual parse invocations (cache misses). The
 * `defaultValue` thunk in [[ParseCache.getOrElseUpdate]] is only evaluated on a cache miss, so we
 * increment [[DebugStats.filesParsed]] and accumulate [[DebugStats.parseTimeNs]] inside that thunk.
 */
final class CountingParseCache(delegate: ParseCache, stats: DebugStats) extends ParseCache {
  override def getOrElseUpdate(
      key: (Path, String),
      defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
    delegate.getOrElseUpdate(
      key, {
        stats.filesParsed += 1
        val t0 = System.nanoTime()
        val result = defaultValue
        stats.parseTimeNs += System.nanoTime() - t0
        result
      }
    )
  }
}
