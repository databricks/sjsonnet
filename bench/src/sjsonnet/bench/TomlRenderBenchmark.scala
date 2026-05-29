package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.util.concurrent.TimeUnit

/**
 * Isolates std.manifestTomlEx rendering on a TOML-heavy object. Fresh interpreter each op (no
 * caching) so the TOML render runs every iteration. Used to A/B the TomlRenderer StringWriter ->
 * StringBuilderWriter swap.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class TomlRenderBenchmark {

  private val source: String =
    """std.manifestTomlEx(
      |  { ['section_%d' % i]: {
      |       name: 'value_string_%d_abcdef' % i, count: i, enabled: true,
      |       tags: ['alpha', 'beta', 'gamma'], nested: { a: 1, b: 'two', c: false },
      |     } for i in std.range(0, 3000) },
      |  '  ')""".stripMargin

  private var wd: os.Path = _

  @Setup
  def setup(): Unit = {
    wd = MainBenchmark.testSuiteRoot
    System.err.println("TOML length: " + run().length)
  }

  @Benchmark def manifestToml(bh: Blackhole): Unit = bh.consume(run())

  private def run(): String = {
    val interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = new SjsonnetMainBase.SimpleImporter(IndexedSeq.empty, None),
      parseCache = new DefaultParseCache
    )
    interp
      .evaluate(source, OsPath(wd / "toml_render_bench.jsonnet"))
      .toOption
      .get
      .asInstanceOf[Val.Str]
      .str
  }
}
