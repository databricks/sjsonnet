package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*
import sjsonnet.starlark.*

import java.io.{OutputStream, PrintStream, StringWriter}
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class StarlarkBenchmark {

  private val wd = {
    var curr = os.pwd
    while (curr.segmentCount > 0 && !os.exists(curr / "bench" / "resources" / "starlark")) {
      curr = curr / os.up
    }
    if (curr.segmentCount == 0 && !os.exists(curr / "bench" / "resources" / "starlark")) {
       throw new RuntimeException("Could not find bench/resources/starlark directory")
    }
    OsPath(curr / "bench" / "resources" / "starlark")
  }

  private val importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] = Some(docBase / importName)
    def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
      val p = path.asInstanceOf[OsPath].p
      if (os.exists(p)) Some(StaticResolvedFile(os.read(p))) else None
    }
  }

  private var starlarkManager: StarlarkContextManager = _
  private var interp: Interpreter = _

  @Setup
  def setup(): Unit = {
    val manager = Platform.makeStarlarkContextManager().get.asInstanceOf[StarlarkContextManager]
    starlarkManager = manager
    StarlarkEngine.currentManager.set(manager)
    
    interp = new Interpreter(
      extVars = Map.empty,
      tlaVars = Map.empty,
      wd = wd,
      importer = importer,
      parseCache = new DefaultParseCache,
      settings = Settings.default,
      variableResolver = {
        case "importstarlark" =>
          Some(Platform.makeStarlarkImportFunc(manager, importer))
        case _ => None
      }
    )
  }

  @TearDown
  def tearDown(): Unit = {
    StarlarkEngine.currentManager.remove()
    Platform.closeStarlarkContextManager(starlarkManager)
  }

  private def runJsonnet(code: String): ujson.Value = {
    interp.interpret(code, wd / "bench.jsonnet") match {
      case Right(v) => v
      case Left(err) => throw new RuntimeException(err)
    }
  }

  @Benchmark
  def pow_jsonnet(bh: Blackhole): Unit = {
    val code = "local n = 1000; [std.pow(3, 2) for i in std.range(1, n)][n-1]"
    bh.consume(runJsonnet(code))
  }

    @Benchmark

    def pow_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.pow_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def makeArray_jsonnet(bh: Blackhole): Unit = {

      val code = "std.makeArray(1000, function(i) i + 1)"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def makeArray_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.makeArray(1000, function(i) i + 1)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def floor_jsonnet(bh: Blackhole): Unit = {

      val code = "local n = 1000; [std.floor(10.99999) for i in std.range(1, n)][n-1]"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def floor_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.floor_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def ceil_jsonnet(bh: Blackhole): Unit = {

      val code = "local n = 1000; [std.ceil(10.99999) for i in std.range(1, n)][n-1]"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def ceil_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.ceil_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def sqrt_jsonnet(bh: Blackhole): Unit = {

      val code = "local n = 1000; [std.sqrt(16) for i in std.range(1, n)][n-1]"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def sqrt_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.sqrt_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def filter_jsonnet(bh: Blackhole): Unit = {

      val code = "std.filter(function(x) x % 2 == 0, std.range(1, 1000))"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def filter_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.filter_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def map_jsonnet(bh: Blackhole): Unit = {

      val code = "std.map(function(x) x * x, std.range(1, 1000))"

      bh.consume(runJsonnet(code))

    }

  

    @Benchmark

    def map_starlark(bh: Blackhole): Unit = {

      val code = """local bench = importstarlark("benchmarks.py"); bench.map_bench(1000)"""

      bh.consume(runJsonnet(code))

    }

  }

  