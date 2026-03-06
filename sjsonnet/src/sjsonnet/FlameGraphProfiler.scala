package sjsonnet

import java.io.{BufferedWriter, FileWriter}

/**
 * Collects stack samples during Jsonnet evaluation and writes them in Brendan Gregg's folded stack
 * format, suitable for generating flame graphs with https://github.com/brendangregg/FlameGraph.
 *
 * Each call to [[push]] records a new frame on the current stack. Each call to [[pop]] removes the
 * top frame. A sample (incrementing the count for the current stack) is taken on every [[push]], so
 * deeper call trees contribute proportionally more samples.
 */
final class FlameGraphProfiler {
  private val stack = new java.util.ArrayDeque[String]()
  private val counts = new java.util.HashMap[String, java.lang.Long]()

  def push(name: String): Unit = {
    stack.push(name)
    val key = foldedStack()
    val prev = counts.get(key)
    counts.put(key, if (prev == null) 1L else prev + 1L)
  }

  def pop(): Unit =
    if (!stack.isEmpty) stack.pop()

  private def foldedStack(): String = {
    val sb = new StringBuilder
    val it = stack.descendingIterator()
    var first = true
    while (it.hasNext) {
      if (!first) sb.append(';')
      sb.append(it.next())
      first = false
    }
    sb.toString
  }

  def writeTo(path: String): Unit = {
    val w = new BufferedWriter(new FileWriter(path))
    try {
      val it = counts.entrySet().iterator()
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
