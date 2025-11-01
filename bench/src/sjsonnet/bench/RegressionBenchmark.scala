package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.io.{OutputStream, PrintStream, ByteArrayOutputStream}
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
@Warmup(iterations = 1, time = 2)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class RegressionBenchmark {

  @Param(Array("bench/resources/bug_suite/assertions.jsonnet"))
  var path: String = _

  private val dummyOut = RegressionBenchmark.createDummyOut

  @Setup(Level.Trial)
  def setup(): Unit = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    SjsonnetMainBase.main0(
      Array(path),
      new DefaultParseCache,
      System.in,
      ps,
      System.err,
      RegressionBenchmark.testSuiteRoot,
      None
    )
    ps.close()
    baos.close()
    val golden = os.read(os.Path(path + ".golden", RegressionBenchmark.testSuiteRoot)).stripLineEnd
    if (baos.toString("UTF-8").stripLineEnd != golden) {
      System.err.println(
        "Golden output mismatch:\n%s != %s".format(
          baos.toString("UTF-8").stripLineEnd,
          golden
        )
      )
    }
  }

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
