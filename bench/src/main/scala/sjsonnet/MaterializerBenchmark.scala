package sjsonnet

import java.io.{StringWriter, Writer}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import ujson.JsVisitor

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
    val config = parser.constructEither(MainBenchmark.mainArgs, autoPrintHelpAndExit = None).getOrElse(???)
    val file = config.file
    val wd = os.pwd
    val path = os.Path(file, wd)
    var currentPos: Position = null
    this.interp = new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      OsPath(wd),
      importer = SjsonnetMain.resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
      parseCache = new DefaultParseCache
    )
    value = interp.evaluate(os.read(path), OsPath(path)).getOrElse(???)
    assert(renderYaml() == oldRenderYaml())
    val r1 = render()
    assert(r1 == oldRender())
    System.err.println("JSON length: "+r1.length)
    assert(renderPython() == oldRenderPython())
  }

  @Benchmark def newRenderB(bh: Blackhole): Unit = bh.consume(render())
  @Benchmark def oldRenderB(bh: Blackhole): Unit = bh.consume(oldRender())
  @Benchmark def newRenderPythonB(bh: Blackhole): Unit = bh.consume(renderPython())
  @Benchmark def oldRenderPythonB(bh: Blackhole): Unit = bh.consume(oldRenderPython())
  @Benchmark def newRenderYamlB(bh: Blackhole): Unit = bh.consume(renderYaml())
  @Benchmark def oldRenderYamlB(bh: Blackhole): Unit = bh.consume(oldRenderYaml())

  @Benchmark def renderPrettyYamlB(bh: Blackhole): Unit = bh.consume(renderPrettyYaml())

  private def render() = renderWith(new Renderer(_, indent=3))
  private def renderPython() = renderWith(new PythonRenderer(_, indent=3))
  private def renderYaml() = renderWith(new YamlRenderer(_, indent=3))
  private def oldRender() = renderWith(new OldRenderer(_, indent=3))
  private def oldRenderPython() = renderWith(new OldPythonRenderer(_, indent=3))
  private def oldRenderYaml() = renderWith(new OldYamlRenderer(_, indent=3))

  private def renderPrettyYaml() = renderWith(new PrettyYamlRenderer(_, indent=3, getCurrentPosition = () => null))

  private def renderWith[T <: Writer](r: StringWriter => JsVisitor[T, T]): String = {
    val writer = new StringWriter
    val renderer = r(writer)
    interp.materialize(value, renderer)
    writer.toString
  }
}
