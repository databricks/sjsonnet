package sjsonnet

import java.io.{OutputStream, PrintStream}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

object MainBenchmark {
  val mainArgs = Array[String](
    "../../universe2/rulemanager/deploy/rulemanager.jsonnet",
    "-J", "../../universe2",
    "-J", "../../universe2/mt-shards/dev/az-westus-c2",
  )
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 30)
@Measurement(iterations = 40)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class MainBenchmark {

  val dummyOut = new PrintStream(new OutputStream {
    def write(b: Int): Unit = ()
    override def write(b: Array[Byte]): Unit = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()
  })

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(SjsonnetMain.main0(
      MainBenchmark.mainArgs,
      collection.mutable.HashMap.empty,
      System.in,
      dummyOut,
      System.err,
      os.pwd,
      None
    ))
  }
}
