package sjsonnet.bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import sjsonnet.*

import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class LongRenderingBenchmark {
  import LongRenderingBenchmark.*

  @Benchmark
  def baseCharRendererLongs(bh: Blackhole): Unit = {
    val out = new StringBuilderWriter(BatchSize * 20)
    val renderer = new ExposedCharRenderer(out)
    var i = 0
    while (i < BatchSize) {
      renderer.write(Values(i & ValuesMask))
      i += 1
    }
    renderer.flushCharBuilder()
    bh.consume(out.getBuilder.length)
  }

  @Benchmark
  def baseByteRendererLongs(bh: Blackhole): Unit = {
    val out = new ByteArrayOutputStream(BatchSize * 20)
    val renderer = new ExposedByteRenderer(out)
    var i = 0
    while (i < BatchSize) {
      renderer.write(Values(i & ValuesMask))
      i += 1
    }
    renderer.flushByteBuilder()
    bh.consume(out.size)
  }

  @Benchmark
  def tomlRendererExactLongDoubles(bh: Blackhole): Unit = {
    val out = new StringBuilderWriter(BatchSize * 20)
    val renderer = new TomlRenderer(out, "", "  ")
    var i = 0
    while (i < BatchSize) {
      renderer.visitFloat64(TomlValues(i & TomlValuesMask), -1)
      i += 1
    }
    bh.consume(out.getBuilder.length)
  }
}

object LongRenderingBenchmark {
  final val BatchSize = 4096

  private val Values: Array[Long] = Array(
    0L,
    7L,
    42L,
    99L,
    100L,
    12345L,
    99999999L,
    100000000L,
    123456789L,
    999999999999L,
    9007199254740991L,
    9007199254740992L,
    Long.MaxValue,
    -1L,
    -123456789L,
    Long.MinValue
  )
  private val ValuesMask = Values.length - 1

  private val TomlValues: Array[Double] = Array(
    0.0, 7.0, 42.0, 99.0, 100.0, 12345.0, 99999999.0, 100000000.0, 123456789.0, 999999999999.0,
    9007199254740991.0, 9007199254740992.0, -1.0, -123456789.0, -9007199254740991.0,
    -9007199254740992.0
  )
  private val TomlValuesMask = TomlValues.length - 1

  private final class ExposedCharRenderer(out: StringBuilderWriter)
      extends BaseCharRenderer[StringBuilderWriter](out) {
    def write(v: Long): Unit = writeLongDirect(v)
  }

  private final class ExposedByteRenderer(out: ByteArrayOutputStream)
      extends BaseByteRenderer[ByteArrayOutputStream](out) {
    def write(v: Long): Unit = writeLongDirect(v)
  }
}
