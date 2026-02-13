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

      starlarkManager = Platform.makeStarlarkContextManager().get.asInstanceOf[StarlarkContextManager]

      interp = new Interpreter(

        extVars = Map.empty, tlaVars = Map.empty, wd = wd, importer = importer,

        parseCache = new DefaultParseCache, settings = Settings.default,

        variableResolver = {

          case "importstarlark" => Some(Platform.makeStarlarkImportFunc(starlarkManager, importer))

          case _ => None

        }

      )

    }

  

    @TearDown

    def tearDown(): Unit = {

      Platform.closeStarlarkContextManager(starlarkManager)

    }

  

    private def runJsonnet(code: String): ujson.Value = {

      StarlarkEngine.currentManager.set(starlarkManager)

      try {

        interp.interpret(code, wd / "bench.jsonnet") match {

          case Right(v) => v

          case Left(err) => throw new RuntimeException(err)

        }

      } finally {

        StarlarkEngine.currentManager.remove()

      }

    }

  

    @Benchmark

    def makeArray_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("std.makeArray(1000, function(i) i + 1)"))

    }

  

    @Benchmark

    def makeArray_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.makeArray(1000, function(i) i + 1)"""))

    }

  

    @Benchmark

    def pow_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("local n = 1000; [std.pow(3, 2) for i in std.range(1, n)][n-1]"))

    }

  

    @Benchmark

    def pow_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.pow_bench(1000)"""))

    }

  

    @Benchmark

    def floor_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("local n = 1000; [std.floor(10.99999) for i in std.range(1, n)][n-1]"))

    }

  

    @Benchmark

    def floor_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.floor_bench(1000)"""))

    }

  

    @Benchmark

    def ceil_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("local n = 1000; [std.ceil(10.99999) for i in std.range(1, n)][n-1]"))

    }

  

    @Benchmark

    def ceil_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.ceil_bench(1000)"""))

    }

  

    @Benchmark

    def sqrt_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("local n = 1000; [std.sqrt(16) for i in std.range(1, n)][n-1]"))

    }

  

    @Benchmark

    def sqrt_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.sqrt_bench(1000)"""))

    }

  

    @Benchmark

    def filter_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("std.filter(function(x) x % 2 == 0, std.range(1, 1000))"))

    }

  

    @Benchmark

    def filter_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.filter_bench(1000)"""))

    }

  

    @Benchmark

    def map_jsonnet(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("std.map(function(x) x * x, std.range(1, 1000))"))

    }

  

    @Benchmark

    def map_starlark(bh: Blackhole): Unit = {

      bh.consume(runJsonnet("""local b = importstarlark("benchmarks.py"); b.map_bench(1000)"""))

    }

  }

  