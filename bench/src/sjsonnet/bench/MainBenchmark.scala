package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.io.{OutputStream, PrintStream, StringWriter}
import java.util.concurrent.TimeUnit

object MainBenchmark {
  private[bench] val testSuiteRoot: os.Path =
    os.pwd / os.up / os.up / os.up / "sjsonnet" / "test" / "resources" / "test_suite"

  private[bench] val mainArgs = Array[String](
    "stdlib.jsonnet",
    "--ext-code",
    "var1=\"test\"",
    "--ext-code",
    "var2={\"x\": 1, \"y\": 2}"
  )

  def findFiles(): (IndexedSeq[(Path, String)], EvalScope) = {
    val parser = mainargs.ParserForClass[Config]
    val config = parser
      .constructEither(MainBenchmark.mainArgs.toIndexedSeq, autoPrintHelpAndExit = None)
      .toOption
      .get
    val file = config.file
    val wd = testSuiteRoot
    val path = OsPath(os.Path(file, wd))
    val parseCache = new DefaultParseCache
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      Map.empty[String, String],
      OsPath(wd),
      importer = SjsonnetMainBase
        .resolveImport(config.getOrderedJpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
      parseCache = parseCache
    )
    val renderer = new Renderer(new StringWriter, indent = 3)
    val out = interp.interpret0(
      interp.resolver.read(path, binaryData = false).get.readString(),
      path,
      renderer
    )
    if (out.isLeft) {
      throw new RuntimeException(s"Error interpreting file $file: ${out.left}")
    }
    (parseCache.keySet.toIndexedSeq, interp.evaluator)
  }

  def createDummyOut = new PrintStream(new OutputStream {
    def write(b: Int): Unit = ()
    override def write(b: Array[Byte]): Unit = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()
  })
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 30)
@Measurement(iterations = 40)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class MainBenchmark {

  private val dummyOut = MainBenchmark.createDummyOut

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(
      SjsonnetMainBase.main0(
        MainBenchmark.mainArgs,
        new DefaultParseCache,
        System.in,
        dummyOut,
        System.err,
        MainBenchmark.testSuiteRoot,
        None
      )
    )
  }
}

// This is a dummy benchmark to see how much memory is used by the interpreter.
// You're meant to execute it, and it will generate stats about memory usage before exiting.
// dump. You can optionally pass an argument to instruct it to pause post run - then attach a
// profiler.
object MemoryBenchmark {

  private val dummyOut = MainBenchmark.createDummyOut

  val cache = new DefaultParseCache

  def main(args: Array[String]): Unit = {
    assert(args.length <= 1, s"Too many arguments: ${args.mkString(",")}")
    val pause: Boolean = if (args.length == 1) {
      if (args(0) == "--pause") {
        println("Run will pause after completion. Attach a profiler then.")
        true
      } else {
        println("Unknown argument: " + args(0))
        System.exit(1)
        false
      }
    } else {
      false
    }
    SjsonnetMainBase.main0(
      MainBenchmark.mainArgs,
      cache,
      System.in,
      dummyOut,
      System.err,
      MainBenchmark.testSuiteRoot,
      None
    )
    println("Pre-GC Stats")
    println("============")
    println("Total memory: " + Runtime.getRuntime.totalMemory())
    println("Free memory: " + Runtime.getRuntime.freeMemory())
    println("Used memory: " + (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()))
    System.gc()
    // Wait for GC to finish
    Thread.sleep(5000)
    println("Post-GC Stats")
    println("============")
    println("Total memory: " + Runtime.getRuntime.totalMemory())
    println("Free memory: " + Runtime.getRuntime.freeMemory())
    println("Used memory: " + (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()))

    if (pause) {
      println("Pausing. Attach a profiler")
      Thread.sleep(1000000000)
    }
  }
}
