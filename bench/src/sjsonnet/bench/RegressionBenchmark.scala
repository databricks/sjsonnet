package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.io.{OutputStream, PrintStream, StringWriter}
import java.util.concurrent.TimeUnit

object RegressionBenchmark {
  private val testSuiteRoot: os.Path =
    sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)

  private def createDummyOut = new PrintStream(new OutputStream {
    def write(b: Int): Unit = ()
    override def write(b: Array[Byte]): Unit = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()
  })
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 1)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class RegressionBenchmark {

  @Param(Array("bench/resources/bug_suite/assertions.jsonnet"))
  var path: String = _

  private val dummyOut = RegressionBenchmark.createDummyOut

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(
      SjsonnetMainBase.main0(
        Array(path),
        new DefaultParseCache,
        System.in,
        dummyOut,
        System.err,
        RegressionBenchmark.testSuiteRoot,
        None
      )
    )
  }
}
