package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import sjsonnet.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class RawImportBenchmark {
  private var root: os.Path = _
  private var importBinProgram: String = _
  private var importStrProgram: String = _
  private var repeatedSmallImportStrProgram: String = _
  private var singleImportBinProgram: String = _
  private var singleImportStrProgram: String = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    root = os.temp.dir(prefix = "sjsonnet-raw-import-benchmark-")
    os.write.over(root / "payload.bin", Array.tabulate[Byte](256 * 1024)(_.toByte))
    os.write.over(root / "payload.txt", "x" * (1024 * 1024 + 1))
    os.write.over(root / "small.txt", "small payload")
    importBinProgram = """std.sum(std.makeArray(
        |  300,
        |  function(_) std.length(importbin "payload.bin")
        |))""".stripMargin
    importStrProgram = """std.sum(std.makeArray(
        |  200,
        |  function(_) std.length(importstr "payload.txt")
        |))""".stripMargin
    repeatedSmallImportStrProgram = """std.sum(std.makeArray(
        |  300,
        |  function(_) std.length(importstr "small.txt")
        |))""".stripMargin
    singleImportBinProgram = """std.length(importbin "payload.bin")"""
    singleImportStrProgram = """std.length(importstr "payload.txt")"""
  }

  @TearDown(Level.Trial)
  def teardown(): Unit = os.remove.all(root)

  private def evaluate(program: String): ujson.Value = {
    val interpreter = new Interpreter(
      Map.empty,
      Map.empty,
      OsPath(root),
      new SjsonnetMainBase.SimpleImporter(Seq.empty),
      parseCache = new DefaultParseCache
    )
    interpreter.interpret(program, OsPath(root / "main.jsonnet")) match {
      case Right(value) => value
      case Left(error)  => throw new RuntimeException(error)
    }
  }

  @Benchmark
  def repeatedImportBin(blackhole: Blackhole): Unit =
    blackhole.consume(evaluate(importBinProgram))

  @Benchmark
  def repeatedImportStr(blackhole: Blackhole): Unit =
    blackhole.consume(evaluate(importStrProgram))

  @Benchmark
  def repeatedSmallImportStr(blackhole: Blackhole): Unit =
    blackhole.consume(evaluate(repeatedSmallImportStrProgram))

  @Benchmark
  def singleImportBin(blackhole: Blackhole): Unit =
    blackhole.consume(evaluate(singleImportBinProgram))

  @Benchmark
  def singleImportStr(blackhole: Blackhole): Unit =
    blackhole.consume(evaluate(singleImportStrProgram))
}
