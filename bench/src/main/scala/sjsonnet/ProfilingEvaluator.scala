package sjsonnet

import java.util
import scala.collection.compat.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.existentials

class ProfilingEvaluator(
    r: CachedResolver,
    e: String => Option[Expr],
    w: Path,
    s: Settings,
    wa: (Boolean, String) => Unit)
    extends Evaluator(r, e, w, s, wa) {

  trait Box {
    def name: String
    var time: Long = 0
    var count: Long = 0
  }

  class ExprBox(val expr: Expr) extends Box {
    var children: Seq[Expr] = Nil
    var totalTime: Long = 0
    lazy val prettyOffset: String =
      prettyIndex(expr.pos).map { case (l, c) => s"$l:$c" }.getOrElse("?:?")
    lazy val prettyPos = s"${expr.pos.currentFile.asInstanceOf[OsPath].p}:$prettyOffset"
    lazy val name: String = {
      val exprName = expr.getClass.getName.split('.').last.split('$').last
      val funcOrOptName: Option[String] = expr match {
        case a: Expr.ApplyBuiltin  => Some(a.func.functionName)
        case a: Expr.ApplyBuiltin1 => Some(a.func.functionName)
        case a: Expr.ApplyBuiltin2 => Some(a.func.functionName)
        case a: Expr.ApplyBuiltin3 => Some(a.func.functionName)
        case a: Expr.ApplyBuiltin4 => Some(a.func.functionName)
        case u: Expr.UnaryOp       => Some(Expr.UnaryOp.name(u.op))
        case b: Expr.BinaryOp      => Some(Expr.BinaryOp.name(b.op))
        case _                     => None
      }
      exprName + funcOrOptName.map(n => s" ($n)").getOrElse("")
    }
  }

  class OpBox(val op: Int, val name: String) extends Box

  class ExprTypeBox(val name: String) extends Box

  class BuiltinBox(val name: String) extends Box

  private val data = new util.IdentityHashMap[Expr, ExprBox]
  private var parent: ExprBox = _

  private def getOrCreate(e: Expr): ExprBox = {
    var box = data.get(e)
    if (box == null) {
      box = new ExprBox(e)
      data.put(e, box)
    }
    box
  }

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = {
    val pt0 = System.nanoTime()
    val box = getOrCreate(e)
    val prevParent = parent
    parent = box
    val t0 = System.nanoTime()
    try super.visitExpr(e)
    finally {
      box.time += (System.nanoTime() - t0)
      box.count += 1
      parent = prevParent
      if (parent != null) {
        parent.time -= (System.nanoTime() - pt0)
      }
    }
  }

  def clear(): Unit = data.clear()

  def get(e: Expr): ExprBox = data.get(e)

  def accumulate(e: Expr): Long = {
    val box = getOrCreate(e)
    box.children = getChildren(e)
    box.totalTime = box.time + box.children.map(accumulate).sum
    box.totalTime
  }

  private def getChildren(e: Expr): Seq[Expr] = e match {
    case p: Product =>
      (0 until p.productArity).iterator
        .map(p.productElement)
        .flatMap {
          case _: Expr                          => Seq(e)
          case a: Array[Expr]                   => a.toSeq
          case a: Array[Expr.CompSpec]          => a.toSeq
          case p: Expr.Params                   => getChildren(p)
          case a: Expr.Member.AssertStmt        => Seq(a.value, a.msg)
          case a: Array[Expr.Bind]              => a.iterator.flatMap(getChildren).toSeq
          case a: Array[Expr.Member.Field]      => a.iterator.flatMap(getChildren).toSeq
          case a: Array[Expr.Member.AssertStmt] => a.iterator.flatMap(getChildren).toSeq
          case s: Seq[?]                        => s.collect { case ee: Expr => ee }
          case s: Some[?]                       => s.collect { case ee: Expr => ee }
          case _                                => Nil
        }
        .filter(_ != null)
        .toSeq
    case _ => Nil
  }

  private def getChildren(p: Expr.Params): Seq[Expr] =
    if (p == null || p.defaultExprs == null) Nil else p.defaultExprs.toSeq

  private def getChildren(b: Expr.Bind): Seq[Expr] =
    getChildren(b.args) :+ b.rhs

  private def getChildren(m: Expr.Member): Seq[Expr] = m match {
    case b: Expr.Bind              => getChildren(b.args) :+ b.rhs
    case a: Expr.Member.AssertStmt => Seq(a.value, a.msg)
    case f: Expr.Member.Field      => getChildren(f.fieldName) ++ getChildren(f.args) :+ f.rhs
    case null                      => Nil
  }

  private def getChildren(f: Expr.FieldName): Seq[Expr] = f match {
    case f: Expr.FieldName.Dyn => Seq(f.expr)
    case _                     => Nil
  }

  def all: Seq[ExprBox] = data.values().asScala.toSeq

  def binaryOperators(): Seq[OpBox] = {
    val m = new mutable.HashMap[Int, OpBox]
    all.foreach { b =>
      b.expr match {
        case Expr.BinaryOp(_, _, op, _) =>
          val ob = m.getOrElseUpdate(op, new OpBox(op, Expr.BinaryOp.name(op)))
          ob.time += b.time
          ob.count += b.count
        case _ =>
      }
    }
    m.valuesIterator.toSeq
  }

  def unaryOperators(): Seq[OpBox] = {
    val m = new mutable.HashMap[Int, OpBox]
    all.foreach { b =>
      b.expr match {
        case Expr.UnaryOp(_, op, _) =>
          val ob = m.getOrElseUpdate(op, new OpBox(op, Expr.UnaryOp.name(op)))
          ob.time += b.time
          ob.count += b.count
        case _ =>
      }
    }
    m.valuesIterator.toSeq
  }

  def exprTypes(): Seq[ExprTypeBox] = {
    val m = new mutable.HashMap[String, ExprTypeBox]
    all.foreach { b =>
      val cl = b.expr match {
        case _: Val => classOf[Val]
        case ee     => ee.getClass
      }
      val n = cl.getName.replaceAll("^sjsonnet\\.", "").replace('$', '.')
      val eb = m.getOrElseUpdate(n, new ExprTypeBox(n))
      eb.time += b.time
      eb.count += b.count
    }
    m.valuesIterator.toSeq
  }

  def builtins(): Seq[BuiltinBox] = {
    val names = new util.IdentityHashMap[Val.Func, String]()
    new Std().functions.foreachEntry((n, f) => names.put(f, n))
    val m = new mutable.HashMap[String, BuiltinBox]
    def add(b: ExprBox, func: Val.Builtin): Unit = {
      val n = names.getOrDefault(func, func.functionName)
      val bb = m.getOrElseUpdate(n, new BuiltinBox(n))
      bb.time += b.time
      bb.count += b.count
    }
    all.foreach { b =>
      b.expr match {
        case a: Expr.ApplyBuiltin1 => add(b, a.func)
        case a: Expr.ApplyBuiltin2 => add(b, a.func)
        case a: Expr.ApplyBuiltin  => add(b, a.func)
        case _                     =>
      }
    }
    m.valuesIterator.toSeq
  }
}
