package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.{Position, Val}

import java.util.concurrent.TimeUnit

/**
 * Micro-benchmark isolating the `case Val.Str(pos, s)` extractor cost — the operation changed by
 * the zero-allocation `Str.unapply` rewrite (value-class extractor vs the old
 * `Some[(Position, String)]`).
 *
 * The end-to-end [[MainBenchmark]] dilutes this to noise; this loop does nothing but match Val.Str
 * (mixing the `AsciiSafeStr` subclass) and consume the bound `pos`/`str`, so any per-match
 * difference shows up directly in ns/op and gc.alloc.rate.norm.
 *
 * Run: ./mill bench.runJmh ".*StrMatchBenchmark.*" -f 4 -wi 10 -i 15 -r 2 -w 1 -prof gc
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 15, time = 2)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class StrMatchBenchmark {

  @Param(Array("1024"))
  var n: Int = _

  private var vals: Array[Val] = _

  @Setup
  def setup(): Unit = {
    val pos = new Position(null, 0)
    vals = Array.tabulate[Val](n) { i =>
      // Alternate plain Str and the AsciiSafeStr subclass so both flow through the same extractor.
      if ((i & 1) == 0) Val.Str(pos, "value_field_" + i)
      else Val.Str.asciiSafe(pos, "ascii_field_" + i)
    }
  }

  @Benchmark
  def matchStr(bh: Blackhole): Unit = {
    val arr = vals
    var i = 0
    while (i < arr.length) {
      arr(i) match {
        case Val.Str(p, s) =>
          bh.consume(p)
          bh.consume(s)
        case other =>
          bh.consume(other)
      }
      i += 1
    }
  }
}
