package sjsonnet

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(4)
@Threads(1)
@Warmup(iterations = 30)
@Measurement(iterations = 40)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class MultiThreadedBenchmark {

  val threads = 8

  @Benchmark
  def main(bh: Blackhole): Unit = {
    val cache: ParseCache = new ParseCache {
      val map = new mutable.HashMap[(Path, String), Either[Error, (Expr, FileScope)]]()
      override def getOrElseUpdate(
          key: (Path, String),
          defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
        var v = map.synchronized(map.getOrElse(key, null))
        if (v == null) {
          v = defaultValue
          map.synchronized(map.put(key, v))
        }
        v
      }
    }

    val pool: ExecutorService = Executors.newFixedThreadPool(threads)
    val futs = (1 to threads).map { _ =>
      pool.submit {
        (() =>
          if (
            SjsonnetMainBase.main0(
              MainBenchmark.mainArgs,
              cache, // new DefaultParseCache
              System.in,
              MainBenchmark.createDummyOut,
              System.err,
              os.pwd,
              None
            ) != 0
          ) throw new Exception): Runnable
      }
    }
    var err: Throwable = null
    bh.consume(futs.map { f =>
      try f.get()
      catch { case e: Throwable => err = e }
    })
    pool.shutdown()
    if (err != null) throw err
  }
}
