package sjsonnet

import java.io.StringWriter

object RunProfiler extends App {
  val parser = mainargs.ParserForClass[Config]
  val config = parser
    .constructEither(MainBenchmark.mainArgs.toIndexedSeq, autoPrintHelpAndExit = None)
    .getOrElse(???)
  val file = config.file
  val wd = os.pwd
  val path = OsPath(os.Path(file, wd))
  val parseCache = new DefaultParseCache
  val interp = new Interpreter(
    Map.empty[String, String],
    Map.empty[String, String],
    OsPath(wd),
    importer = SjsonnetMain
      .resolveImport(config.getJpaths.map(os.Path(_, wd)).map(OsPath(_)).toIndexedSeq, None),
    parseCache = parseCache
  ) {
    override def createEvaluator(
        resolver: CachedResolver,
        extVars: String => Option[Expr],
        wd: Path,
        settings: Settings): Evaluator =
      new ProfilingEvaluator(resolver, extVars, wd, settings, null)
  }
  val profiler = interp.evaluator.asInstanceOf[ProfilingEvaluator]

  def run(): Long = {
    val renderer = new Renderer(new StringWriter, indent = 3)
    val start = interp.resolver.read(path, binaryData = false).get.readString()
    val t0 = System.nanoTime()
    interp.interpret0(start, path, renderer).getOrElse(???)
    System.nanoTime() - t0
  }

  println("\nWarming up...")
  profiler.clear()
  for (i <- 1 to 10) run()

  println("\nProfiling...")
  profiler.clear()
  val total = (for (i <- 1 to 5) yield run()).sum

  val roots = parseCache.valuesIterator.map(_.getOrElse(???)).map(_._1).toSeq
  roots.foreach(profiler.accumulate)

  println(s"\nTop 20 by time:")
  profiler.all.sortBy(-_.time).take(20).foreach { b => show(b.time, b, "- ", rec = false) }

  val cutoff = 0.02
  println(s"\nTrees with >= $cutoff time:")
  showAll(roots, "")

  def showAll(es: Seq[Expr], indent: String): Unit = {
    val timed = es.iterator
      .map(profiler.get)
      .filter(_.totalTime.toDouble / total.toDouble >= cutoff)
      .toSeq
      .sortBy(-_.time)
    timed.foreach { b => show(b.totalTime, b, indent, rec = true) }
  }

  def show(time: Long, box: profiler.ExprBox, indent: String, rec: Boolean): Unit = {
    println(s"$indent${time / 1000000L}ms ${box.name} ${box.prettyPos}")
    if (rec) showAll(box.children, indent + "  ")
  }

  def show(n: String, bs: Seq[profiler.Box]): Unit = {
    println(n)
    bs.filter(_.count > 0).sortBy(-_.time).foreach { ob =>
      val avg = if (ob.count == 0) 0 else ob.time / ob.count
      println(s"- ${ob.time / 1000000L}ms\t${ob.count}\t${avg}ns\t${ob.name}")
    }
  }

  show(s"\nBinary operators:", profiler.binaryOperators())
  show(s"\nUnary operators:", profiler.unaryOperators())
  show(s"\nBuilt-in functions:", profiler.builtins())
  show(s"\nExpr types:", profiler.exprTypes())
}
