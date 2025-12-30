package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*
import sjsonnet.starlark.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class StarlarkAllocationBenchmark {

  private val wd = {
    var curr = os.pwd
    while (curr.segmentCount > 0 && !os.exists(curr / "bench" / "resources" / "starlark")) {
      curr = curr / os.up
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

  private var manager: StarlarkContextManager = _
  private var interp: Interpreter = _

  @Setup
  def setup(): Unit = {
    manager = Platform.makeStarlarkContextManager().get.asInstanceOf[StarlarkContextManager]
    interp = new Interpreter(
      extVars = Map.empty, tlaVars = Map.empty, wd = wd, importer = importer,
      parseCache = new DefaultParseCache, settings = Settings.default,
      variableResolver = {
        case "importstarlark" => Some(Platform.makeStarlarkImportFunc(manager, importer))
        case _ => None
      }
    )
  }

  @TearDown
  def tearDown(): Unit = {
    Platform.closeStarlarkContextManager(manager)
  }

  private def runJsonnet(code: String): ujson.Value = {
    StarlarkEngine.currentManager.set(manager)
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
  def immutable_updates_10k(bh: Blackhole): Unit = {
    // 10,000 loop iterations * 10 replacements = 100,000 allocations
    val code = """local b = importstarlark("allocation.py"); b.benchmark_immutable(10000)"""
    bh.consume(runJsonnet(code))
  }

  @Benchmark
  def mutable_updates_10k(bh: Blackhole): Unit = {
    // 10,000 loop iterations * 10 mutations = 1 object
    val code = """local b = importstarlark("allocation.py"); b.benchmark_mutable(10000)"""
    bh.consume(runJsonnet(code))
  }
}
