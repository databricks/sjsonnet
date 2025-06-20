package sjsonnet.bench

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.{Parser, Path}

import java.util.concurrent.TimeUnit
import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ParserBenchmark {

  private var allFiles: IndexedSeq[(Path, String)] = _

  @Setup
  def setup(): Unit =
    allFiles = MainBenchmark.findFiles()._1

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(allFiles.foreach { case (p, s) =>
      val res = fastparse.parse(
        s,
        new Parser(p, mutable.HashMap.empty, mutable.HashMap.empty).document(_)
      )
      bh.consume(res.asInstanceOf[Success[?]])
    })
  }
}
