package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{Error => _, _}
import ujson.Value

import scala.annotation.{switch, tailrec}

/**
 * Recursively walks the [[Expr]] trees to convert them into into [[Val]] objects that can be
 * materialized to JSON.
 *
 * Performs import resolution and parsing on-demand when the relevant nodes in the syntax tree are
 * reached, and caches the evaluated result of each imported module to be re-used. Parsing is cached
 * separatedly by an external `parseCache`.
 */
class Evaluator(
    resolver: CachedResolver,
    val extVars: String => Option[Expr],
    val wd: Path,
    val settings: Settings,
    warnLogger: Error => Unit = null)
    extends EvalScope {
  implicit def evalScope: EvalScope = this
  def importer: CachedImporter = resolver

  def warn(e: Error): Unit = if (warnLogger != null) warnLogger(e)

  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports: collection.mutable.HashMap[Path, Val] =
    collection.mutable.HashMap.empty[Path, Val]
  var tailstrict: Boolean = false

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = try {
    e match {
      case e: ValidId            => visitValidId(e)
      case e: BinaryOp           => visitBinaryOp(e)
      case e: Select             => visitSelect(e)
      case e: Val                => e
      case e: ApplyBuiltin0      => visitApplyBuiltin0(e)
      case e: ApplyBuiltin1      => visitApplyBuiltin1(e)
      case e: ApplyBuiltin2      => visitApplyBuiltin2(e)
      case e: ApplyBuiltin3      => visitApplyBuiltin3(e)
      case e: ApplyBuiltin4      => visitApplyBuiltin4(e)
      case e: And                => visitAnd(e)
      case e: Or                 => visitOr(e)
      case e: UnaryOp            => visitUnaryOp(e)
      case e: Apply1             => visitApply1(e)
      case e: Lookup             => visitLookup(e)
      case e: Function           => visitMethod(e.body, e.params, e.pos)
      case e: LocalExpr          => visitLocalExpr(e)
      case e: Apply              => visitApply(e)
      case e: IfElse             => visitIfElse(e)
      case e: Apply3             => visitApply3(e)
      case e: ObjBody.MemberList => visitMemberList(e.pos, e, null)
      case e: Apply2             => visitApply2(e)
      case e: AssertExpr         => visitAssert(e)
      case e: ApplyBuiltin       => visitApplyBuiltin(e)
      case e: Comp               => visitComp(e)
      case e: Arr                => visitArr(e)
      case e: SelectSuper        => visitSelectSuper(e)
      case e: LookupSuper        => visitLookupSuper(e)
      case e: InSuper            => visitInSuper(e)
      case e: ObjExtend          => visitObjExtend(e)
      case e: ObjBody.ObjComp    => visitObjComp(e, null)
      case e: Slice              => visitSlice(e)
      case e: Import             => visitImport(e)
      case e: Apply0             => visitApply0(e)
      case e: ImportStr          => visitImportStr(e)
      case e: ImportBin          => visitImportBin(e)
      case e: Expr.Error         => visitError(e)
      case e                     => visitInvalid(e)
    }
  } catch {
    Error.withStackFrame(e)
  }
  // This is only needed for --no-static-errors, otherwise these expression types do not make it past the optimizer
  def visitInvalid(e: Expr): Nothing = e match {
    case Id(pos, name) =>
      Error.fail("Unknown variable: " + name, pos)
    case Self(pos) =>
      Error.fail("Can't use self outside of an object", pos)
    case $(pos) =>
      Error.fail("Can't use $ outside of an object", pos)
    case Super(pos) =>
      Error.fail("Can't use super outside of an object", pos)
    case _ =>
      Error.fail("Should not have happened.", e.pos)
  }

  def visitAsLazy(e: Expr)(implicit scope: ValScope): Lazy = e match {
    case v: Val => v
    case e      => new LazyWithComputeFunc(() => visitExpr(e))
  }

  def visitValidId(e: ValidId)(implicit scope: ValScope): Val = {
    val ref = scope.bindings(e.nameIdx)
    ref.force
  }

  def visitSelect(e: Select)(implicit scope: ValScope): Val = visitExpr(e.value) match {
    case obj: Val.Obj => obj.value(e.name, e.pos)
    case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${e.name}", e.pos)
  }

  def visitLocalExpr(e: LocalExpr)(implicit scope: ValScope): Val = {
    val bindings = e.bindings
    val s =
      if (bindings == null) scope
      else {
        val base = scope.length
        val newScope = scope.extendBy(bindings.length)
        var i = 0
        while (i < bindings.length) {
          val b = bindings(i)
          newScope.bindings(base + i) = b.args match {
            case null => visitAsLazy(b.rhs)(newScope)
            case argSpec =>
              new LazyWithComputeFunc(() => visitMethod(b.rhs, argSpec, b.pos)(newScope))
          }
          i += 1
        }
        newScope
      }
    visitExpr(e.returned)(s)
  }

  def visitComp(e: Comp)(implicit scope: ValScope): Val =
    new Val.Arr(
      e.pos,
      visitComp(e.first :: e.rest.toList, Array(scope)).map(s => visitAsLazy(e.value)(s))
    )

  def visitArr(e: Arr)(implicit scope: ValScope): Val =
    new Val.Arr(e.pos, e.value.map(visitAsLazy))

  def visitSelectSuper(e: SelectSuper)(implicit scope: ValScope): Val = {
    val sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    if (sup == null) Error.fail("Attempt to use `super` when there is no super class", e.pos)
    else sup.value(e.name, e.pos, scope.bindings(e.selfIdx).asInstanceOf[Val.Obj])
  }

  def visitObjExtend(e: ObjExtend)(implicit scope: ValScope): Val = {
    val original = visitExpr(e.base).cast[Val.Obj]
    e.ext match {
      case ext: ObjBody.MemberList => visitMemberList(e.pos, ext, original)
      case ext: ObjBody.ObjComp    => visitObjComp(ext, original)
      case o: Val.Obj              => o.addSuper(e.pos, original)
      case _                       => Error.fail("Should not have happened", e.pos)
    }
  }

  def visitIfElse(e: IfElse)(implicit scope: ValScope): Val = {
    visitExpr(e.cond) match {
      case Val.True(_) => visitExpr(e.`then`)
      case Val.False(_) =>
        e.`else` match {
          case null => Val.Null(e.pos)
          case v    => visitExpr(v)
        }
      case v => Error.fail("Need boolean, found " + v.prettyName, e.pos)
    }
  }

  def visitError(e: Expr.Error)(implicit scope: ValScope): Nothing = {
    Error.fail(materializeError(visitExpr(e.value)), e.pos)
  }

  protected def materializeError(value: Val) = value match {
    case Val.Str(_, s) => s
    case r             => Materializer.stringify(r)
  }

  def visitUnaryOp(e: UnaryOp)(implicit scope: ValScope): Val = {
    val v = visitExpr(e.value)
    val pos = e.pos
    def fail() =
      Error.fail(s"Unknown unary operation: ${Expr.UnaryOp.name(e.op)} ${v.prettyName}", pos)
    e.op match {
      case Expr.UnaryOp.OP_! =>
        v match {
          case Val.True(_)  => Val.False(pos)
          case Val.False(_) => Val.True(pos)
          case _            => fail()
        }
      case Expr.UnaryOp.OP_- =>
        v match {
          case Val.Num(_, v) => Val.Num(pos, -v)
          case _             => fail()
        }
      case Expr.UnaryOp.OP_~ =>
        v match {
          case Val.Num(_, v) => Val.Num(pos, (~v.toLong).toDouble)
          case _             => fail()
        }
      case Expr.UnaryOp.OP_+ =>
        v match {
          case Val.Num(_, v) => Val.Num(pos, v)
          case _             => fail()
        }
      case _ => fail()
    }
  }

  protected def visitApply(e: Apply)(implicit scope: ValScope) = {
    val lhs = visitExpr(e.value)

    if (tailstrict) {
      lhs.cast[Val.Func].apply(e.args.map(visitExpr(_)), e.namedNames, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = lhs.cast[Val.Func].apply(e.args.map(visitExpr(_)), e.namedNames, e.pos)
      tailstrict = false
      res
    } else {
      val args = e.args
      val argsL = new Array[Lazy](args.length)
      var idx = 0
      while (idx < args.length) {
        argsL(idx) = visitAsLazy(args(idx))
        idx += 1
      }
      lhs.cast[Val.Func].apply(argsL, e.namedNames, e.pos)
    }
  }

  protected def visitApply0(e: Apply0)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    if (e.tailstrict) {
      tailstrict = true
      val res = lhs.cast[Val.Func].apply0(e.pos)
      tailstrict = false
      res
    } else {
      lhs.cast[Val.Func].apply0(e.pos)
    }
  }

  protected def visitApply1(e: Apply1)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    if (tailstrict) {
      lhs.cast[Val.Func].apply1(visitExpr(e.a1), e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = lhs.cast[Val.Func].apply1(visitExpr(e.a1), e.pos)
      tailstrict = false
      res
    } else {
      val l1 = visitAsLazy(e.a1)
      lhs.cast[Val.Func].apply1(l1, e.pos)
    }
  }

  protected def visitApply2(e: Apply2)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    if (tailstrict) {
      lhs.cast[Val.Func].apply2(visitExpr(e.a1), visitExpr(e.a2), e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = lhs.cast[Val.Func].apply2(visitExpr(e.a1), visitExpr(e.a2), e.pos)
      tailstrict = false
      res
    } else {
      val l1 = visitAsLazy(e.a1)
      val l2 = visitAsLazy(e.a2)
      lhs.cast[Val.Func].apply2(l1, l2, e.pos)
    }
  }

  protected def visitApply3(e: Apply3)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    if (tailstrict) {
      lhs.cast[Val.Func].apply3(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = lhs.cast[Val.Func].apply3(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), e.pos)
      tailstrict = false
      res
    } else {
      val l1 = visitAsLazy(e.a1)
      val l2 = visitAsLazy(e.a2)
      val l3 = visitAsLazy(e.a3)
      lhs.cast[Val.Func].apply3(l1, l2, l3, e.pos)
    }
  }

  protected def visitApplyBuiltin0(e: ApplyBuiltin0) = {
    if (tailstrict) {
      e.func.evalRhs(this, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = e.func.evalRhs(this, e.pos)
      tailstrict = false
      res
    } else {
      e.func.evalRhs(this, e.pos)
    }
  }

  protected def visitApplyBuiltin1(e: ApplyBuiltin1)(implicit scope: ValScope) = {
    if (tailstrict) {
      e.func.evalRhs(visitExpr(e.a1), this, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = e.func.evalRhs(visitExpr(e.a1), this, e.pos)
      tailstrict = false
      res
    } else {
      e.func.evalRhs(visitAsLazy(e.a1), this, e.pos)
    }
  }

  protected def visitApplyBuiltin2(e: ApplyBuiltin2)(implicit scope: ValScope) = {
    if (tailstrict) {
      e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), this, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), this, e.pos)
      tailstrict = false
      res
    } else {
      e.func.evalRhs(visitAsLazy(e.a1), visitAsLazy(e.a2), this, e.pos)
    }
  }

  protected def visitApplyBuiltin3(e: ApplyBuiltin3)(implicit scope: ValScope) = {
    if (tailstrict) {
      e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), this, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      val res = e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), visitExpr(e.a3), this, e.pos)
      tailstrict = false
      res
    } else {
      e.func.evalRhs(visitAsLazy(e.a1), visitAsLazy(e.a2), visitAsLazy(e.a3), this, e.pos)
    }
  }

  protected def visitApplyBuiltin4(e: ApplyBuiltin4)(implicit scope: ValScope) = {
    if (tailstrict) {
      e.func.evalRhs(
        visitExpr(e.a1),
        visitExpr(e.a2),
        visitExpr(e.a3),
        visitExpr(e.a4),
        this,
        e.pos
      )
    } else if (e.tailstrict) {
      tailstrict = true
      val res = e.func.evalRhs(
        visitExpr(e.a1),
        visitExpr(e.a2),
        visitExpr(e.a3),
        visitExpr(e.a4),
        this,
        e.pos
      )
      tailstrict = false
      res
    } else {
      e.func.evalRhs(
        visitAsLazy(e.a1),
        visitAsLazy(e.a2),
        visitAsLazy(e.a3),
        visitAsLazy(e.a4),
        this,
        e.pos
      )
    }
  }

  protected def visitApplyBuiltin(e: ApplyBuiltin)(implicit scope: ValScope) = {
    val arr = new Array[Lazy](e.argExprs.length)
    var idx = 0

    if (tailstrict) {
      while (idx < e.argExprs.length) {
        arr(idx) = visitExpr(e.argExprs(idx))
        idx += 1
      }
      e.func.evalRhs(arr, this, e.pos)
    } else if (e.tailstrict) {
      tailstrict = true
      while (idx < e.argExprs.length) {
        arr(idx) = visitExpr(e.argExprs(idx))
        idx += 1
      }
      val res = e.func.evalRhs(arr, this, e.pos)
      tailstrict = false
      res
    } else {

      while (idx < e.argExprs.length) {
        val boundIdx = idx
        arr(idx) = visitAsLazy(e.argExprs(boundIdx))
        idx += 1
      }
      e.func.evalRhs(arr, this, e.pos)
    }
  }

  def visitAssert(e: AssertExpr)(implicit scope: ValScope): Val = {
    if (!visitExpr(e.asserted.value).isInstanceOf[Val.True]) {
      e.asserted.msg match {
        case null => Error.fail("Assertion failed", e)
        case msg =>
          Error.fail("Assertion failed: " + materializeError(visitExpr(msg)), e)
      }
    }
    visitExpr(e.returned)
  }

  protected def visitSlice(e: Slice)(implicit scope: ValScope): Val = {
    def extractParam(e: Option[Expr]): Option[Int] = e.flatMap(visitExpr(_) match {
      case _: Val.Null => None
      case v: Val.Num  => Some(v.value.toInt)
      case v: Val      => Some(v.cast[Val.Num].value.toInt)
    })

    val indexable = visitExpr(e.value) match {
      case arr: Val.Arr => arr
      case str: Val.Str => str
      case x            => Error.fail("Can only slice array or string, not " + x.prettyName, e.pos)
    }
    Util.slice(
      e.pos,
      this,
      indexable,
      extractParam(e.start),
      extractParam(e.end),
      extractParam(e.stride)
    )
  }

  def visitLookup(e: Lookup)(implicit scope: ValScope): Val = {
    val pos = e.pos
    (visitExpr(e.value), visitExpr(e.index)) match {
      case (v: Val.Arr, i: Val.Num) =>
        val int = i.value.toInt
        if (int != i.value) Error.fail("array index was not integer: " + i.value, pos)
        if (v.length == 0) Error.fail(s"array bounds error: array is empty", pos)
        if (int >= v.length)
          Error.fail(s"array bounds error: ${int} not within [0, ${v.length})", pos)
        v.force(int)
      case (v: Val.Str, i: Val.Num) => Val.Str(pos, new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        v.value(i.value, pos)
      case (lhs, rhs) =>
        Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
    }
  }

  def visitLookupSuper(e: LookupSuper)(implicit scope: ValScope): Val = {
    var sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    val key = visitExpr(e.index).cast[Val.Str]
    if (sup == null) sup = scope.bindings(e.selfIdx).asInstanceOf[Val.Obj]
    sup.value(key.value, e.pos)
  }

  def visitImportStr(e: ImportStr): Val.Str =
    Val.Str(
      e.pos,
      importer.resolveAndReadOrFail(e.value, e.pos, binaryData = false)._2.readString()
    )

  def visitImportBin(e: ImportBin): Val.Arr =
    new Val.Arr(
      e.pos,
      importer
        .resolveAndReadOrFail(e.value, e.pos, binaryData = true)
        ._2
        .readRawBytes()
        .map(x => Val.Num(e.pos, (x & 0xff).doubleValue))
    )

  def visitImport(e: Import): Val = {
    val (p, str) = importer.resolveAndReadOrFail(e.value, e.pos, binaryData = false)
    cachedImports.getOrElseUpdate(
      p, {
        val doc = resolver.parse(p, str) match {
          case Right((expr, _)) => expr
          case Left(err)        => throw err.asSeenFrom(this)
        }
        visitExpr(doc)(ValScope.empty)
      }
    )
  }

  def visitAnd(e: And)(implicit scope: ValScope): Val.Bool = {
    visitExpr(e.lhs) match {
      case _: Val.True =>
        visitExpr(e.rhs) match {
          case b: Val.Bool =>
            b
          case unknown =>
            Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case _: Val.False =>
        Val.False(e.pos)
      case unknown =>
        Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitOr(e: Or)(implicit scope: ValScope): Val.Bool = {
    visitExpr(e.lhs) match {
      case _: Val.True => Val.True(e.pos)
      case _: Val.False =>
        visitExpr(e.rhs) match {
          case b: Val.Bool => b
          case unknown =>
            Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case unknown =>
        Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitInSuper(e: InSuper)(implicit scope: ValScope): Val.Bool = {
    val sup = scope.bindings(e.selfIdx + 1).asInstanceOf[Val.Obj]
    if (sup == null) Val.False(e.pos)
    else {
      val key = visitExpr(e.value).cast[Val.Str]
      Val.bool(e.pos, sup.containsKey(key.value))
    }
  }

  def visitBinaryOp(e: BinaryOp)(implicit scope: ValScope): Val.Literal = {
    val l = visitExpr(e.lhs)
    val r = visitExpr(e.rhs)
    val pos = e.pos
    def fail() = Error.fail(
      s"Unknown binary operation: ${l.prettyName} ${Expr.BinaryOp.name(e.op)} ${r.prettyName}",
      pos
    )
    e.op match {

      case Expr.BinaryOp.OP_== =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        Val.bool(pos, equal(l, r))

      case Expr.BinaryOp.OP_!= =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        Val.bool(pos, !equal(l, r))

      case Expr.BinaryOp.OP_+ =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l + r)
          case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
          case (Val.Str(_, l), r)             => Val.Str(pos, l + Materializer.stringify(r))
          case (l, Val.Str(_, r))             => Val.Str(pos, Materializer.stringify(l) + r)
          case (l: Val.Obj, r: Val.Obj)       => r.addSuper(pos, l)
          case (l: Val.Arr, r: Val.Arr)       => l.concat(pos, r)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_- =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l - r)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_* =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l * r)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_/ =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) =>
            if (r == 0) Error.fail("division by zero", pos)
            Val.Num(pos, l / r)
          case _ => fail()
        }

      case Expr.BinaryOp.OP_% =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l % r)
          case (Val.Str(_, l), r)             => Val.Str(pos, Format.format(l, r, pos))
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_< =>
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l < r)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l < r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(pos, compare(x, y) < 0)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_> =>
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l > r)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l > r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(pos, compare(x, y) > 0)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_<= =>
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l <= r)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l <= r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(pos, compare(x, y) <= 0)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_>= =>
        (l, r) match {
          case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l >= r)
          case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l >= r)
          case (x: Val.Arr, y: Val.Arr)       => Val.bool(pos, compare(x, y) >= 0)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_<< =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, (l.toLong << r.toLong).toDouble)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_>> =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, (l.toLong >> r.toLong).toDouble)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_in =>
        (l, r) match {
          case (Val.Str(_, l), o: Val.Obj) => Val.bool(pos, o.containsKey(l))
          case _                           => fail()
        }

      case Expr.BinaryOp.OP_& =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, (l.toLong & r.toLong).toDouble)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_^ =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, (l.toLong ^ r.toLong).toDouble)
          case _                              => fail()
        }

      case Expr.BinaryOp.OP_| =>
        (l, r) match {
          case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, (l.toLong | r.toLong).toDouble)
          case _                              => fail()
        }

      case _ => fail()
    }
  }

  def visitFieldName(fieldName: FieldName, pos: Position)(implicit scope: ValScope): String = {
    fieldName match {
      case FieldName.Fixed(s) => s
      case FieldName.Dyn(k) =>
        visitExpr(k) match {
          case Val.Str(_, k1) => k1
          case Val.Null(_)    => null
          case x              => fieldNameTypeError(x, pos)
        }
    }
  }

  private def fieldNameTypeError(fieldName: Val, pos: Position): Nothing = {
    Error.fail(s"Field name must be string or null, not ${fieldName.prettyName}", pos)
  }

  def visitMethod(rhs: Expr, params: Params, outerPos: Position)(implicit
      scope: ValScope): Val.Func =
    new Val.Func(outerPos, scope, params) {
      def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position): Val =
        visitExpr(rhs)(vs)
      override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope) = visitExpr(expr)(vs)
    }

  def visitBindings(bindings: Array[Bind], scope: => ValScope): Array[Lazy] = {
    val arrF = new Array[Lazy](bindings.length)
    var i = 0
    while (i < bindings.length) {
      val b = bindings(i)
      arrF(i) = b.args match {
        case null =>
          new LazyWithComputeFunc(() => visitExpr(b.rhs)(scope))
        case argSpec =>
          new LazyWithComputeFunc(() => visitMethod(b.rhs, argSpec, b.pos)(scope))
      }
      i += 1
    }
    arrF
  }

  def visitMemberList(objPos: Position, e: ObjBody.MemberList, sup: Val.Obj)(implicit
      scope: ValScope): Val.Obj = {
    val asserts = e.asserts
    val fields = e.fields
    var cachedSimpleScope: Option[ValScope] = None
    var cachedObj: Val.Obj = null
    var asserting: Boolean = false

    def makeNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      if ((sup eq null) && (self eq cachedObj)) {
        cachedSimpleScope.getOrElse {
          cachedSimpleScope = Some(createNewScope(self, sup))
          cachedSimpleScope.get
        }
      } else createNewScope(self, sup)
    }

    def assertions(self: Val.Obj): Unit = if (
      // We need to avoid asserting the same object more than once to prevent
      // infinite recursion, but the previous implementation had the `asserting`
      // flag kept under the object's *instantiation* rather than under the
      // object itself. That means that objects that are instantiated once then
      // extended multiple times would only trigger the assertions once, rather
      // than once per extension.
      //
      // Due to backwards compatibility concerns, this fix has to go in under
      // the flag `strictInheritedAssertions`, to be removed after an appropriate
      // time period has passed.
      (settings.strictInheritedAssertions && !self.asserting) ||
      (!settings.strictInheritedAssertions && !asserting)
    ) {
      if (settings.strictInheritedAssertions) self.asserting = true
      else asserting = true
      val newScope: ValScope = makeNewScope(self, self.getSuper)
      var i = 0
      while (i < asserts.length) {
        val a = asserts(i)
        if (!visitExpr(a.value)(newScope).isInstanceOf[Val.True]) {
          a.msg match {
            case null => Error.fail("Assertion failed", a.value.pos, "Assert")
            case msg =>
              Error.fail(
                "Assertion failed: " + visitExpr(msg)(newScope).cast[Val.Str].value,
                a.value.pos,
                "Assert"
              )
          }
        }
        i += 1
      }
    }

    def createNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      val scopeLen = scope.length
      val binds = e.binds
      val by = if (binds == null) 2 else 2 + binds.length
      val newScope = scope.extendBy(by)
      newScope.bindings(scopeLen) = self
      newScope.bindings(scopeLen + 1) = sup
      if (binds != null) {
        val arrF = newScope.bindings
        var i = 0
        var j = scopeLen + 2
        while (i < binds.length) {
          val b = binds(i)
          arrF(j) = b.args match {
            case null =>
              visitAsLazy(b.rhs)(newScope)
            case argSpec =>
              new LazyWithComputeFunc(() => visitMethod(b.rhs, argSpec, b.pos)(newScope))
          }
          i += 1
          j += 1
        }
      }
      newScope
    }

    val builder = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](fields.length)
    fields.foreach {
      case Member.Field(offset, fieldName, plus, null, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if (k != null) {
          val v = new Val.Obj.Member(plus, sep) {
            def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
              if (asserts != null) assertions(self)
              visitExpr(rhs)(makeNewScope(self, sup))
            }
          }
          builder.put(k, v)
        }
      case Member.Field(offset, fieldName, false, argSpec, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if (k != null) {
          val v = new Val.Obj.Member(false, sep) {
            def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
              if (asserts != null) assertions(self)
              visitMethod(rhs, argSpec, offset)(makeNewScope(self, sup))
            }
          }
          builder.put(k, v)
        }
      case _ =>
        Error.fail("This case should never be hit", objPos)
    }
    val valueCache = if (sup == null) {
      Val.Obj.getEmptyValueCacheForObjWithoutSuper(fields.length)
    } else {
      new java.util.HashMap[Any, Val]()
    }
    cachedObj = new Val.Obj(
      objPos,
      builder,
      false,
      if (asserts != null) assertions else null,
      sup,
      valueCache
    )
    cachedObj
  }

  def visitObjComp(e: ObjBody.ObjComp, sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val binds = e.preLocals ++ e.postLocals
    val compScope: ValScope = scope // .clearSuper
    val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
    for (s <- visitComp(e.first :: e.rest, Array(compScope))) {
      visitExpr(e.key)(s) match {
        case Val.Str(_, k) =>
          val prev_length = builder.size()
          builder.put(
            k,
            new Val.Obj.Member(e.plus, Visibility.Normal) {
              def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
                // There is a circular dependency between `newScope` and `newBindings` because
                // bindings may refer to other bindings (e.g. chains of locals that build on
                // each other):
                lazy val newScope: ValScope = s.extend(newBindings, self, sup)
                lazy val newBindings = visitBindings(binds, newScope)
                visitExpr(e.value)(newScope)
              }
            }
          )
          if (prev_length == builder.size() && settings.noDuplicateKeysInComprehension) {
            Error.fail(s"Duplicate key ${k} in evaluated object comprehension.", e.pos);
          }
        case Val.Null(_) => // do nothing
        case x           => fieldNameTypeError(x, e.pos)
      }
    }
    val valueCache = if (sup == null) {
      Val.Obj.getEmptyValueCacheForObjWithoutSuper(builder.size())
    } else {
      new java.util.HashMap[Any, Val]()
    }
    new Val.Obj(e.pos, builder, false, null, sup, valueCache)
  }

  @tailrec
  final def visitComp(f: List[CompSpec], scopes: Array[ValScope]): Array[ValScope] = f match {
    case (spec @ ForSpec(_, name, expr)) :: rest =>
      visitComp(
        rest,
        for {
          s <- scopes
          e <- visitExpr(expr)(s) match {
            case a: Val.Arr => a.asLazyArray
            case r =>
              Error.fail(
                "In comprehension, can only iterate over array, not " + r.prettyName,
                spec
              )
          }
        } yield s.extendSimple(e)
      )
    case (spec @ IfSpec(offset, expr)) :: rest =>
      visitComp(
        rest,
        scopes.filter(visitExpr(expr)(_) match {
          case Val.True(_)  => true
          case Val.False(_) => false
          case other =>
            Error.fail(
              "Condition must be boolean, got " + other.prettyName,
              spec
            )
        })
      )
    case Nil => scopes
  }

  def compare(x: Val, y: Val): Int = (x, y) match {
    case (_: Val.Null, _: Val.Null) => 0
    case (x: Val.Num, y: Val.Num)   => x.value.compareTo(y.value)
    case (x: Val.Str, y: Val.Str)   => x.value.compareTo(y.value)
    case (x: Val.Bool, y: Val.Bool) => x.asBoolean.compareTo(y.asBoolean)
    case (x: Val.Arr, y: Val.Arr) =>
      val len = math.min(x.length, y.length)
      var i = 0
      while (i < len) {
        val cmp = compare(x.force(i), y.force(i))
        if (cmp != 0) return cmp
        i += 1
      }
      Ordering[Int].compare(x.length, y.length)
    case _ => Error.fail("Cannot compare " + x.prettyName + " with " + y.prettyName, x.pos)
  }

  def equal(x: Val, y: Val): Boolean = (x eq y) || (x match {
    case _: Val.True  => y.isInstanceOf[Val.True]
    case _: Val.False => y.isInstanceOf[Val.False]
    case _: Val.Null  => y.isInstanceOf[Val.Null]
    case x: Val.Str =>
      y match {
        case y: Val.Str => x.value == y.value
        case _          => false
      }
    case x: Val.Num =>
      y match {
        case y: Val.Num => x.value == y.value
        case _          => false
      }
    case x: Val.Arr =>
      y match {
        case y: Val.Arr =>
          val xlen = x.length
          if (xlen != y.length) return false
          var i = 0
          while (i < xlen) {
            if (!equal(x.force(i), y.force(i))) return false
            i += 1
          }
          true
        case _ => false
      }
    case x: Val.Obj =>
      y match {
        case y: Val.Obj =>
          val k1 = x.visibleKeyNames
          val k2 = y.visibleKeyNames
          val k1len = k1.length
          if (k1len != k2.length) return false
          x.triggerAllAsserts(x)
          y.triggerAllAsserts(y)
          var i = 0
          while (i < k1len) {
            val k = k1(i)
            if (!y.containsKey(k)) return false
            val v1 = x.value(k, emptyMaterializeFileScopePos)
            val v2 = y.value(k, emptyMaterializeFileScopePos)
            if (!equal(v1, v2)) return false
            i += 1
          }
          true
        case _ => false
      }
    case _ => false
  })
}

class NewEvaluator(
    private val r: CachedResolver,
    private val e: String => Option[Expr],
    private val w: Path,
    private val s: Settings,
    private val wa: Error => Unit = null)
    extends Evaluator(r, e, w, s, wa) {

  override def visitExpr(e: Expr)(implicit scope: ValScope): Val = try {
    (e._tag: @switch) match {
      case ExprTags.ValidId       => visitValidId(e.asInstanceOf[ValidId])
      case ExprTags.BinaryOp      => visitBinaryOp(e.asInstanceOf[BinaryOp])
      case ExprTags.Select        => visitSelect(e.asInstanceOf[Select])
      case ExprTags.`Val.Func`    => e.asInstanceOf[Val.Func]
      case ExprTags.`Val.Literal` => e.asInstanceOf[Val.Literal]
      case ExprTags.ApplyBuiltin0 => visitApplyBuiltin0(e.asInstanceOf[ApplyBuiltin0])
      case ExprTags.ApplyBuiltin1 => visitApplyBuiltin1(e.asInstanceOf[ApplyBuiltin1])
      case ExprTags.ApplyBuiltin2 => visitApplyBuiltin2(e.asInstanceOf[ApplyBuiltin2])
      case ExprTags.ApplyBuiltin3 => visitApplyBuiltin3(e.asInstanceOf[ApplyBuiltin3])
      case ExprTags.ApplyBuiltin4 => visitApplyBuiltin4(e.asInstanceOf[ApplyBuiltin4])
      case ExprTags.And           => visitAnd(e.asInstanceOf[And])
      case ExprTags.Or            => visitOr(e.asInstanceOf[Or])
      case ExprTags.UnaryOp       => visitUnaryOp(e.asInstanceOf[UnaryOp])
      case ExprTags.Apply1        => visitApply1(e.asInstanceOf[Apply1])
      case ExprTags.Lookup        => visitLookup(e.asInstanceOf[Lookup])
      case ExprTags.Function =>
        val f = e.asInstanceOf[Function]
        visitMethod(f.body, f.params, f.pos)
      case ExprTags.LocalExpr => visitLocalExpr(e.asInstanceOf[LocalExpr])
      case ExprTags.Apply     => visitApply(e.asInstanceOf[Apply])
      case ExprTags.IfElse    => visitIfElse(e.asInstanceOf[IfElse])
      case ExprTags.Apply3    => visitApply3(e.asInstanceOf[Apply3])
      case ExprTags.`ObjBody.MemberList` =>
        val oml = e.asInstanceOf[ObjBody.MemberList]
        visitMemberList(oml.pos, oml, null)
      case ExprTags.Apply2            => visitApply2(e.asInstanceOf[Apply2])
      case ExprTags.AssertExpr        => visitAssert(e.asInstanceOf[AssertExpr])
      case ExprTags.ApplyBuiltin      => visitApplyBuiltin(e.asInstanceOf[ApplyBuiltin])
      case ExprTags.Comp              => visitComp(e.asInstanceOf[Comp])
      case ExprTags.Arr               => visitArr(e.asInstanceOf[Arr])
      case ExprTags.SelectSuper       => visitSelectSuper(e.asInstanceOf[SelectSuper])
      case ExprTags.LookupSuper       => visitLookupSuper(e.asInstanceOf[LookupSuper])
      case ExprTags.InSuper           => visitInSuper(e.asInstanceOf[InSuper])
      case ExprTags.ObjExtend         => visitObjExtend(e.asInstanceOf[ObjExtend])
      case ExprTags.`ObjBody.ObjComp` => visitObjComp(e.asInstanceOf[ObjBody.ObjComp], null)
      case ExprTags.Slice             => visitSlice(e.asInstanceOf[Slice])
      case ExprTags.Import            => visitImport(e.asInstanceOf[Import])
      case ExprTags.Apply0            => visitApply0(e.asInstanceOf[Apply0])
      case ExprTags.ImportStr         => visitImportStr(e.asInstanceOf[ImportStr])
      case ExprTags.ImportBin         => visitImportBin(e.asInstanceOf[ImportBin])
      case ExprTags.Error             => visitError(e.asInstanceOf[Expr.Error])
      case _                          => visitInvalid(e)
    }
  } catch {
    Error.withStackFrame(e)
  }
  // This is only needed for --no-static-errors, otherwise these expression types do not make it past the optimizer
  override def visitInvalid(e: Expr): Nothing = (e._tag: @switch) match {
    case ExprTags.Id =>
      val id = e.asInstanceOf[Id]
      Error.fail("Unknown variable: " + id.name, id.pos)
    case ExprTags.Self =>
      Error.fail("Can't use self outside of an object", e.pos)
    case ExprTags.`$` =>
      Error.fail("Can't use $ outside of an object", e.pos)
    case ExprTags.Super =>
      Error.fail("Can't use super outside of an object", e.pos)
  }
}

object Evaluator {
  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Lazy](0)
}
