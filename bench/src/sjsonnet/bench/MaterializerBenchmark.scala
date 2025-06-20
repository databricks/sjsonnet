package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.*
import sjsonnet.*
import ujson.JsVisitor

import java.io.{StringWriter, Writer}
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class MaterializerBenchmark {

  private var interp: Interpreter = _
  private var value: Val = _

  @Setup
  def setup(): Unit = {
    val parser = mainargs.ParserForClass[Config]
    val config = parser
      .constructEither(MainBenchmark.mainArgs.toIndexedSeq, autoPrintHelpAndExit = None)
      .toOption
      .get
    val file = config.file
    val wd = MainBenchmark.testSuiteRoot
    val path = os.Path(file, wd)
    this.interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = SjsonnetMainBase
        .resolveImport(
          config.getOrderedJpaths.map(os.Path(_, wd)).map(OsPath(_)).toIndexedSeq,
          None
        ),
      parseCache = new DefaultParseCache
    )
    value = interp.evaluate(os.read(path), OsPath(path)).toOption.get
    val r1 = render()
    System.err.println("JSON length: " + r1.length)
  }

  @Benchmark def newRenderB(bh: Blackhole): Unit = bh.consume(render())
  @Benchmark def newRenderPythonB(bh: Blackhole): Unit = bh.consume(renderPython())
  @Benchmark def newRenderYamlB(bh: Blackhole): Unit = bh.consume(renderYaml())

  @Benchmark def renderPrettyYamlB(bh: Blackhole): Unit = bh.consume(renderPrettyYaml())

  private def render() = renderWith(new Renderer(_, indent = 3))
  private def renderPython() = renderWith(new PythonRenderer(_, indent = 3))
  private def renderYaml() = renderWith(new YamlRenderer(_, indent = 3))

  private def renderPrettyYaml() = renderWith(
    new PrettyYamlRenderer(_, indent = 3, getCurrentPosition = () => null)
  )

  private def renderWith[T <: Writer](r: StringWriter => JsVisitor[T, T]): String = {
    val writer = new StringWriter
    val renderer = r(writer)
    interp.materialize(value, renderer)
    writer.toString
  }
}
