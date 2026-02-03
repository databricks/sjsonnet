package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import org.graalvm.polyglot.{Context, Engine, Source}
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class TinyTruffleBenchmark {

  @Benchmark
  def runTheoreticalLimit(bh: Blackhole): Unit = {
    var i = 0
    val limit = 1000000
    while (i < limit) {
      i += 1
    }
    bh.consume(i)
  }
}
