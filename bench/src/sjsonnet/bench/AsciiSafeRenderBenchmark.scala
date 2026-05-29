package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*

import java.io.{StringWriter, Writer}
import java.util.concurrent.TimeUnit

/**
 * Isolates the rendering path on a string-heavy value (many [[Val.AsciiSafeStr]] leaves). Used to
 * A/B the AsciiSafeStr renderer fast path: evaluate once in @Setup, then benchmark only render().
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class AsciiSafeRenderBenchmark {

  private val source: String =
    """{
      |  obj: { ['key_field_%d' % i]: 'value_string_%d_abcdefghijklmnop' % i
      |         for i in std.range(0, 3000) },
      |  joined: std.join(',', ['element_token_%d' % i for i in std.range(0, 8000)]),
      |}""".stripMargin

  private var interp: Interpreter = _
  private var value: Val = _

  @Setup
  def setup(): Unit = {
    val wd = MainBenchmark.testSuiteRoot
    this.interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = new SjsonnetMainBase.SimpleImporter(IndexedSeq.empty, None),
      parseCache = new DefaultParseCache
    )
    val path = OsPath(wd / "ascii_safe_render_bench.jsonnet")
    value = interp.evaluate(source, path).toOption.get
    System.err.println("JSON length: " + render().length)
  }

  @Benchmark def renderB(bh: Blackhole): Unit = bh.consume(render())

  private def render(): String = {
    val writer = new StringWriter
    interp.materialize(value, new Renderer(writer, indent = 3))
    writer.toString
  }
}
