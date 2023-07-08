package sjsonnet

import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ParserBenchmark {

  private var allFiles: IndexedSeq[(Path, String)] = _
  private var interp: Interpreter = _

  @Setup
  def setup(): Unit =
    allFiles = MainBenchmark.findFiles()._1

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(allFiles.foreach { case (p, s) =>
      val res = fastparse.parse(s, new Parser(p, true).document(_))
      bh.consume(res.asInstanceOf[Success[_]])
    })
  }
}
