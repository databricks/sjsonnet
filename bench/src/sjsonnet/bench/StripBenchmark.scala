package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.stdlib.StringModule

import java.util.concurrent.TimeUnit

/**
 * Micro-benchmark isolating `std.stripChars` with an all-ASCII strip set — the case optimized by
 * the inline two-`long` mask in `StripUtils.strip` (issue #851), replacing a per-call
 * `java.util.BitSet`. The string has long leading/trailing runs of strip chars so the membership
 * check is exercised; `gc.alloc.rate.norm` shows the removed per-call BitSet allocation directly.
 *
 * Run: ./mill bench.runJmh ".*StripBenchmark.*" -f 4 -wi 10 -i 15 -r 2 -w 1 -prof gc
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 15, time = 2)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class StripBenchmark {

  // Multi-char ASCII strip set -> exercises the BitSet path (baseline) / two-long mask (this PR),
  // not the single-char fast path.
  private val chars = "ab"
  private var input: String = _

  @Setup
  def setup(): Unit = {
    val run = "ab" * 1000 // 2000 leading + 2000 trailing strip chars
    input = run + "MIDDLE" + run
  }

  @Benchmark
  def strip(bh: Blackhole): Unit =
    bh.consume(StringModule.StripUtils.strip(input, chars, left = true, right = true))
}
