package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream, StringWriter}
import java.util.concurrent.TimeUnit

/**
 * A/B benchmark comparing old (instanceof chain) vs new (tag + tableswitch) evaluator.
 *
 * Runs the full interpret pipeline (parse → optimize → evaluate → materialize) for each benchmark
 * file, isolating the evaluator difference by using the same Settings with only `useNewEvaluator`
 * toggled.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 15)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class EvaluatorBenchmark {

  @Param(
    Array(
      // cpp_suite — C++ jsonnet benchmarks
      "bench/resources/cpp_suite/bench.01.jsonnet",
      "bench/resources/cpp_suite/bench.02.jsonnet",
      "bench/resources/cpp_suite/bench.03.jsonnet",
      "bench/resources/cpp_suite/bench.04.jsonnet",
      "bench/resources/cpp_suite/bench.06.jsonnet",
      "bench/resources/cpp_suite/bench.08.jsonnet",
      "bench/resources/cpp_suite/bench.09.jsonnet",
      "bench/resources/cpp_suite/gen_big_object.jsonnet",
      "bench/resources/cpp_suite/heavy_string_render.jsonnet",
      "bench/resources/cpp_suite/large_string_join.jsonnet",
      "bench/resources/cpp_suite/realistic1.jsonnet",
      "bench/resources/cpp_suite/realistic2.jsonnet",
      "bench/resources/cpp_suite/string_render_perf.jsonnet",
      // go_suite — Go jsonnet builtins
      "bench/resources/go_suite/base64_heavy.jsonnet",
      "bench/resources/go_suite/base64_mega.jsonnet",
      "bench/resources/go_suite/comparison.jsonnet",
      "bench/resources/go_suite/comparison2.jsonnet",
      "bench/resources/go_suite/foldl.jsonnet",
      "bench/resources/go_suite/reverse.jsonnet",
      "bench/resources/go_suite/substr.jsonnet",
      // bug_suite
      "bench/resources/bug_suite/assertions.jsonnet",
      // sjsonnet_suite
      "bench/resources/sjsonnet_suite/setDiff.jsonnet"
    )
  )
  var path: String = _

  private var wd: os.Path = _
  private var filePath: OsPath = _
  private var fileContent: String = _
  private var jpaths: Seq[OsPath] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    wd = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
    filePath = OsPath(wd / os.RelPath(path))
    fileContent = os.read(wd / os.RelPath(path))
    jpaths = Seq(OsPath(wd))
  }

  private def run(useNew: Boolean): String = {
    val settings = new Settings(
      useNewEvaluator = useNew,
      maxStack = 100000
    )
    val interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = new SjsonnetMainBase.SimpleImporter(jpaths, None),
      parseCache = new DefaultParseCache,
      settings = settings
    )
    val writer = new StringWriter
    val renderer = new Renderer(writer, indent = 3)
    interp.interpret0(fileContent, filePath, renderer) match {
      case Right(_) => writer.toString
      case Left(e)  => throw new RuntimeException(e)
    }
  }

  @Benchmark
  def oldEvaluator(bh: Blackhole): Unit = {
    bh.consume(run(useNew = false))
  }

  @Benchmark
  def newEvaluator(bh: Blackhole): Unit = {
    bh.consume(run(useNew = true))
  }
}
