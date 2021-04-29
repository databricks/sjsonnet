package sjsonnet

import java.io.StringWriter

object RunProfiler extends App {
  val parser = mainargs.ParserForClass[Config]
  val config = parser.constructEither(MainBenchmark.mainArgs, autoPrintHelpAndExit = None).getOrElse(???)
  val file = config.file
  val wd = os.pwd
  val path = OsPath(os.Path(file, wd))
  val interp = new Interpreter(
    Map.empty[String, ujson.Value],
    Map.empty[String, ujson.Value],
    OsPath(wd),
    importer = SjsonnetMain.resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
  ) {
    override def createEvaluator(resolver: CachedResolver, extVars: Map[String, ujson.Value], wd: Path,
                                 preserveOrder: Boolean, strict: Boolean): Evaluator =
      new ProfilingEvaluator(resolver, extVars, wd, preserveOrder, strict)
  }
  val profiler = interp.evaluator.asInstanceOf[ProfilingEvaluator]

  def run(): Long = {
    val renderer = new Renderer(new StringWriter, indent = 3)
    val start = interp.resolver.read(path).get
    val t0 = System.nanoTime()
    interp.interpret0(start, path, renderer).getOrElse(???)
    System.nanoTime() - t0
  }

  println("Warming up...")
  profiler.data.clear()
  for(i <- 1 to 10) run()

  println("Profiling...")
  profiler.data.clear()
  val total = (for(i <- 1 to 5) yield run()).sum

  val roots = interp.parseCache.valuesIterator.map(_.getOrElse(???)).map(_._1).toSeq

  val cutoff = 0.05

  println(s"Top:")
  allExprs(roots).sortBy(-_._2).take(20).foreach { case (e, t) => show(e, t, "- ", false) }

  println(s"Results (total=${total/1000000.0}ms)")
  roots.foreach(accumulate)
  showAll(roots, "")

  def showAll(es: Seq[Expr], indent: String) = {
    val timed = es.iterator.filter(_ != null).map { e =>
      (e, profiler.data.getOrDefault(e, new profiler.Box).time)
    }.filter { case (e, time) =>
      time.toDouble / total.toDouble >= cutoff
    }.toSeq.sortBy(-_._2)
    timed.foreach { case (e, time) => show(e, time, indent, true) }
  }

  def show(e: Expr, time: Long, indent: String, rec: Boolean): Unit = {
    val name = e.getClass.getName.split('.').last.split('$').last
    //val file = e.pos.currentFile.relativeToString(profiler.wd)
    val file = e.pos.currentFile.asInstanceOf[OsPath].p.toString
    val pos = profiler.prettyIndex(e.pos).map { case (l,c) => s"$l:$c" }.getOrElse("?:?")
    println(s"$indent${time/1000000L}ms ${name} ${file}:${pos}")
    if(rec) showAll(getChildren(e), indent + "  ")
  }

  def accumulate(e: Expr): Long = {
    val ch = getChildren(e).map(accumulate).sum
    var box = profiler.data.get(e)
    if(box == null) {
      box = new profiler.Box
      profiler.data.put(e, box)
    }
    box.time += ch
    box.time
  }

  def allExprs(es: Seq[Expr]): Seq[(Expr, Long)] = {
    val b = Vector.newBuilder[(Expr, Long)]
    def f(e: Expr): Unit = {
      b.+=((e, profiler.data.getOrDefault(e, new profiler.Box).time))
      getChildren(e).foreach(f)
    }
    es.foreach(f)
    b.result()
  }

  def getChildren(e: Expr): Seq[Expr] = e match {
    case p: Product =>
      (0 until p.productArity).iterator.map(p.productElement).flatMap {
        case e: Expr => Seq(e)
        case a: Array[Expr] => a.toSeq
        case a: Array[Expr.CompSpec] => a.toSeq
        case p: Expr.Params => getChildren(p)
        case a: Expr.Member.AssertStmt => Seq(a.value, a.msg)
        case a: Array[Expr.Bind] => a.iterator.flatMap(getChildren).toSeq
        case a: Array[Expr.Member.Field] => a.iterator.flatMap(getChildren).toSeq
        case a: Array[Expr.Member.AssertStmt] => a.iterator.flatMap(getChildren).toSeq
        case s: Seq[_] => s.collect { case e: Expr => e }
        case s: Some[_] => s.collect { case e: Expr => e }
        case _ => Nil
      }.filter(_ != null).toSeq
    case _ => Nil
  }

  def getChildren(p: Expr.Params): Seq[Expr] =
    if(p == null || p.defaultExprs == null) Nil else p.defaultExprs.toSeq

  def getChildren(b: Expr.Bind): Seq[Expr] =
    getChildren(b.args) :+ b.rhs

  def getChildren(m: Expr.Member): Seq[Expr] = m match {
    case b: Expr.Bind => getChildren(b.args) :+ b.rhs
    case a: Expr.Member.AssertStmt => Seq(a.value, a.msg)
    case f: Expr.Member.Field => getChildren(f.fieldName) ++ getChildren(f.args) :+ f.rhs
    case _ => Nil
  }

  def getChildren(f: Expr.FieldName): Seq[Expr] = f match {
    case f: Expr.FieldName.Dyn => Seq(f.expr)
    case _ => Nil
  }
}
