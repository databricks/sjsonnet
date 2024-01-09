package sjsonnet

import java.io.{OutputStream, PrintStream, StringWriter}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

object MainBenchmark {
  val mainArgs = Array[String](
    "../../universe/rulemanager/deploy/rulemanager.jsonnet",
    // "../../universe/kubernetes/admission-controller/gatekeeper/deploy/gatekeeper.jsonnet",
    "-J", "../../universe",
    "-J", "../../universe/mt-shards/dev/az-westus-c2",
    "-J", "../../universe/bazel-bin",
    "--ext-code", "isKubecfg=false"
  )

  def findFiles(): (IndexedSeq[(Path, String)], EvalScope) = {
    val parser = mainargs.ParserForClass[Config]
    val config = parser.constructEither(MainBenchmark.mainArgs, autoPrintHelpAndExit = None).getOrElse(???)
    val file = config.file
    val wd = os.pwd
    val path = OsPath(os.Path(file, wd))
    val parseCache = new DefaultParseCache
    val interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = SjsonnetMain.resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
      parseCache = parseCache
    )
    val renderer = new Renderer(new StringWriter, indent = 3)
    interp.interpret0(interp.resolver.read(path).get.readString(), path, renderer).getOrElse(???)
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

  val dummyOut = MainBenchmark.createDummyOut

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(SjsonnetMain.main0(
      MainBenchmark.mainArgs,
      new DefaultParseCache,
      System.in,
      dummyOut,
      System.err,
      os.pwd,
      None
    ))
  }
}

// This is a dummy benchmark to see how much memory is used by the interpreter.
// You're meant to execute it, and it will generate stats about memory usage before exiting.
// dump. You can optionally pass an argument to instruct it to pause post run - then attach a
// profiler.
object MemoryBenchmark {

  val dummyOut = MainBenchmark.createDummyOut

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
    SjsonnetMain.main0(
      MainBenchmark.mainArgs,
      cache,
      System.in,
      dummyOut,
      System.err,
      os.pwd,
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
