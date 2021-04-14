package sjsonnet

import java.io.StringWriter
import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ParserBenchmark {

  private var interp: Interpreter = _
  private var inputs: Iterable[(sjsonnet.Path, String)] = _

  @Setup
  def setup(): Unit = {
    val parser = mainargs.ParserForClass[Config]
    val config = parser.constructEither(MainBenchmark.mainArgs, autoPrintHelpAndExit = None).getOrElse(???)
    val file = config.file
    val wd = os.pwd
    val path = os.Path(file, wd)
    var currentPos: Position = null
    this.interp = new Interpreter(
      Map.empty[String, ujson.Value],
      Map.empty[String, ujson.Value],
      OsPath(wd),
      importer = SjsonnetMain.resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
    )
    val writer = new StringWriter
    val renderer = new Renderer(writer, indent = 3)
    interp.interpret0(os.read(path), OsPath(path), renderer).getOrElse(???)
    inputs = interp.parseCache.keySet
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (p, s) =>
      val res = fastparse.parse(s, new Parser(p).document(_))
      bh.consume(res.asInstanceOf[Success[_]])
    })
  }
}
