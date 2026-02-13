package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*
import sjsonnet.starlark.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 30, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class StarlarkPartialEvalBenchmark {

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

  @Benchmark
  def loadExpensiveModule(bh: Blackhole): Unit = {
    val manager = Platform.makeStarlarkContextManager().get.asInstanceOf[StarlarkContextManager]
    StarlarkEngine.currentManager.set(manager)
    try {
      val interp = new Interpreter(
        extVars = Map.empty, tlaVars = Map.empty, wd = wd, importer = importer,
        parseCache = new DefaultParseCache, settings = Settings.default,
        variableResolver = {
          case "importstarlark" => Some(Platform.makeStarlarkImportFunc(manager, importer))
          case _ => None
        }
      )
      // The core of the test: importing the module triggers top-level execution
      val code = """importstarlark("expensive.py").X"""
      bh.consume(interp.interpret(code, wd / "bench.jsonnet"))
    } finally {
      StarlarkEngine.currentManager.remove()
      Platform.closeStarlarkContextManager(manager)
    }
  }
}
