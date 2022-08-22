package sjsonnet

/*-
 * Changed:
 * - 7187ded408ff8ea68822297079549c1c323bcdee: implement support for ?? binary op
 * - 80f58d4e2d5e4d4ea94ef828962ef5d8cba1a625: implements support for safe select operator ?.
 */

import Expr.{Error => _, _}
import sjsonnet.Expr.Member.Visibility
import ujson.Value

import scala.collection.mutable

/**
  * Recursively walks the [[Expr]] trees to convert them into into [[Val]]
  * objects that can be materialized to JSON.
  *
  * Performs import resolution and parsing on-demand when the relevant nodes
  * in the syntax tree are reached, and caches the evaluated result of each
  * imported module to be re-used. Parsing is cached separatedly by an external
  * `parseCache`.
  */
class Evaluator(resolver: CachedResolver,
                val extVars: String => Option[Expr],
                val wd: Path,
                val settings: Settings,
                warnLogger: Error => Unit = null) extends EvalScope {
  implicit def evalScope: EvalScope = this
  def importer: CachedImporter = resolver

  def warn(e: Error): Unit = if(warnLogger != null) warnLogger(e)

  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports = collection.mutable.HashMap.empty[Path, Val]

  def visitExpr(e: Expr)(implicit scope: ValScope): Val = try {
    e match {
      case e: ValidId => visitValidId(e)
      case e: BinaryOp => visitBinaryOp(e)
      case e: Select => visitSelect(e)
      case e: Val => e
      case e: ApplyBuiltin1 => visitApplyBuiltin1(e)
      case e: ApplyBuiltin2 => visitApplyBuiltin2(e)
      case e: And => visitAnd(e)
      case e: Or => visitOr(e)
      case e: NullCoal => visitNullCoalesce(e)
      case e: UnaryOp => visitUnaryOp(e)
      case e: Apply1 => visitApply1(e)
      case e: Lookup => visitLookup(e)
      case e: Function => visitMethod(e.body, e.params, e.pos)
      case e: LocalExpr => visitLocalExpr(e)
      case e: Apply => visitApply(e)
      case e: IfElse => visitIfElse(e)
      case e: Apply3 => visitApply3(e)
      case e: ObjBody.MemberList => visitMemberList(e.pos, e, null)
      case e: Apply2 => visitApply2(e)
      case e: AssertExpr => visitAssert(e)
      case e: ApplyBuiltin => visitApplyBuiltin(e)
      case e: Comp => visitComp(e)
      case e: Arr => visitArr(e)
      case e: SelectSuper => visitSelectSuper(e)
      case e: LookupSuper => visitLookupSuper(e)
      case e: InSuper => visitInSuper(e)
      case e: ObjExtend => visitObjExtend(e)
      case e: ObjBody.ObjComp => visitObjComp(e, null)
      case e: Slice => visitSlice(e)
      case e: Import => visitImport(e)
      case e: Apply0 => visitApply0(e)
      case e: ImportStr => visitImportStr(e)
      case e: Expr.Error => visitError(e)
      case e => visitInvalid(e)
    }
  } catch Error.withStackFrame(e)

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
  }

  def visitAsLazy(e: Expr)(implicit scope: ValScope): Lazy = e match {
    case v: Val => v
    case e => () => visitExpr(e)
  }

  def visitValidId(e: ValidId)(implicit scope: ValScope): Val = {
    val ref = scope.bindings(e.nameIdx)
    ref.force
  }

  def visitSelect(e: Select)(implicit scope: ValScope): Val = visitExpr(e.value) match {
    case obj: Val.Obj => obj.value(e.name, e.pos, safe = e.safe)
    case _: Val.Null if e.safe => Val.Null(e.pos)
    case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${e.name}", e.pos)
  }

  def visitLocalExpr(e: LocalExpr)(implicit scope: ValScope): Val = {
    val bindings = e.bindings
    val s =
      if(bindings == null) scope else {
        val base = scope.length
        val newScope = scope.extendBy(bindings.length)
        var i = 0
        while(i < bindings.length) {
          val b = bindings(i)
          newScope.bindings(base+i) = b.args match {
            case null => visitAsLazy(b.rhs)(newScope)
            case argSpec => () => visitMethod(b.rhs, argSpec, b.pos)(newScope)
          }
          i += 1
        }
        newScope
      }
    visitExpr(e.returned)(s)
  }

  def visitComp(e: Comp)(implicit scope: ValScope): Val =
    new Val.Arr(e.pos, visitComp(e.first :: e.rest.toList, Array(scope)).map(s => visitAsLazy(e.value)(s)))

  def visitArr(e: Arr)(implicit scope: ValScope): Val =
    new Val.Arr(e.pos, e.value.map(visitAsLazy))

  def visitSelectSuper(e: SelectSuper)(implicit scope: ValScope): Val = {
    val sup = scope.bindings(e.selfIdx+1).asInstanceOf[Val.Obj]
    if(sup == null) Error.fail("Attempt to use `super` when there is no super class", e.pos)
    else sup.value(e.name, e.pos, scope.bindings(e.selfIdx).asInstanceOf[Val.Obj], safe = e.safe)
  }

  def visitObjExtend(e: ObjExtend)(implicit scope: ValScope): Val = {
    val original = visitExpr(e.base).cast[Val.Obj]
    e.ext match {
      case ext: ObjBody.MemberList => visitMemberList(e.pos, ext, original)
      case ext: ObjBody.ObjComp => visitObjComp(ext, original)
      case o: Val.Obj => o.addSuper(e.pos, original)
    }
  }

  def visitIfElse(e: IfElse)(implicit scope: ValScope): Val = {
    visitExpr(e.cond) match {
      case Val.True(_) => visitExpr(e.`then`)
      case Val.False(_) =>
        e.`else` match {
          case null => Val.Null(e.pos)
          case v => visitExpr(v)
        }
      case v => Error.fail("Need boolean, found " + v.prettyName, e.pos)
    }
  }

  def visitError(e: Expr.Error)(implicit scope: ValScope): Nothing = {
    Error.fail(materializeError(visitExpr(e.value)), e.pos)
  }

  private def materializeError(value: Val) = value match {
    case Val.Str(_, s) => s
    case r => Materializer.stringify(r)
  }

  def visitUnaryOp(e: UnaryOp)(implicit scope: ValScope): Val = {
    val v = visitExpr(e.value)
    val pos = e.pos
    def fail() = Error.fail(s"Unknown unary operation: ${Expr.UnaryOp.name(e.op)} ${v.prettyName}", pos)
    e.op match {
      case Expr.UnaryOp.OP_! => v match {
        case Val.True(_) => Val.False(pos)
        case Val.False(_) => Val.True(pos)
        case _ => fail()
      }
      case Expr.UnaryOp.OP_- => v match {
        case Val.Num(_, v) => Val.Num(pos, -v)
        case _ => fail()
      }
      case Expr.UnaryOp.OP_~ => v match {
        case Val.Num(_, v) => Val.Num(pos, ~v.toLong)
        case _ => fail()
      }
      case Expr.UnaryOp.OP_+ => v match {
        case Val.Num(_, v) => Val.Num(pos, v)
        case _ => fail()
      }
      case _ => fail()
    }
  }

  private def visitApply(e: Apply)(implicit scope: ValScope) = {
    val lhs = visitExpr(e.value)
    val args = e.args
    val argsL = new Array[Lazy](args.length)
    var idx = 0
    while (idx < args.length) {
      argsL(idx) = visitAsLazy(args(idx))
      idx += 1
    }
    lhs.cast[Val.Func].apply(argsL, e.namedNames, e.pos)
  }

  private def visitApply0(e: Apply0)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    lhs.cast[Val.Func].apply0(e.pos)
  }

  private def visitApply1(e: Apply1)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    val l1 = visitAsLazy(e.a1)
    lhs.cast[Val.Func].apply1(l1, e.pos)
  }

  private def visitApply2(e: Apply2)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    val l1 = visitAsLazy(e.a1)
    val l2 = visitAsLazy(e.a2)
    lhs.cast[Val.Func].apply2(l1, l2, e.pos)
  }

  private def visitApply3(e: Apply3)(implicit scope: ValScope): Val = {
    val lhs = visitExpr(e.value)
    val l1 = visitAsLazy(e.a1)
    val l2 = visitAsLazy(e.a2)
    val l3 = visitAsLazy(e.a3)
    lhs.cast[Val.Func].apply3(l1, l2, l3, e.pos)
  }

  private def visitApplyBuiltin1(e: ApplyBuiltin1)(implicit scope: ValScope) =
    e.func.evalRhs(visitExpr(e.a1), this, e.pos)

  private def visitApplyBuiltin2(e: ApplyBuiltin2)(implicit scope: ValScope) =
    e.func.evalRhs(visitExpr(e.a1), visitExpr(e.a2), this, e.pos)

  private def visitApplyBuiltin(e: ApplyBuiltin)(implicit scope: ValScope) = {
    val arr = new Array[Val](e.argExprs.length)
    var idx = 0
    while (idx < e.argExprs.length) {
      val boundIdx = idx
      arr(idx) = visitExpr(e.argExprs(boundIdx))
      idx += 1
    }
    e.func.evalRhs(arr, this, e.pos)
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

  private def visitSlice(e: Slice)(implicit scope: ValScope): Val = {
    visitExpr(e.value) match {
      case a: Val.Arr =>
        new Val.Arr(
          e.pos,
          Util.sliceArr(
            a.asLazyArray,
            e.start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt),
            e.end.fold(a.length)(visitExpr(_).cast[Val.Num].value.toInt),
            e.stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
          )
        )


      case Val.Str(_, s) =>
        Val.Str(
          e.pos,
          Util.sliceStr(
            s,
            e.start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt),
            e.end.fold(s.length)(visitExpr(_).cast[Val.Num].value.toInt),
            e.stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
          )
        )

      case x => Error.fail("Can only slice array or string, not " + x.prettyName, e.pos)
    }
  }

  def visitLookup(e: Lookup)(implicit scope: ValScope): Val = {
    val pos = e.pos
    (visitExpr(e.value), visitExpr(e.index)) match {
      case (v: Val.Arr, i: Val.Num) =>
        val int = i.value.toInt
        if (int != i.value) Error.fail("array index was not integer: " + i.value, pos)
        if (int >= v.length) Error.fail(s"array bounds error: ${int} not within [0, ${v.length})", pos)
        v.force(int)
      case (v: Val.Str, i: Val.Num) => Val.Str(pos, new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        v.value(i.value, pos)
      case (lhs, rhs) =>
        Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
    }
  }

  def visitLookupSuper(e: LookupSuper)(implicit scope: ValScope): Val = {
    var sup = scope.bindings(e.selfIdx+1).asInstanceOf[Val.Obj]
    val key = visitExpr(e.index).cast[Val.Str]
    if(sup == null) sup = scope.bindings(e.selfIdx).asInstanceOf[Val.Obj]
    sup.value(key.value, e.pos)
  }

  def visitImportStr(e: ImportStr)(implicit scope: ValScope): Val.Str =
    Val.Str(e.pos, importer.resolveAndReadOrFail(e.value, e.pos)._2)

  def visitImport(e: Import)(implicit scope: ValScope): Val = {
    val (p, str) = importer.resolveAndReadOrFail(e.value, e.pos)
    cachedImports.getOrElseUpdate(
      p,
      {
        val doc = resolver.parse(p, str) match {
          case Right((expr, _)) => expr
          case Left(err) => throw err.asSeenFrom(this)
        }
        visitExpr(doc)(ValScope.empty)
      }
    )
  }

  def visitAnd(e: And)(implicit scope: ValScope) = {
    visitExpr(e.lhs) match {
      case _: Val.True =>
        visitExpr(e.rhs) match{
          case b: Val.Bool => b
          case unknown =>
            Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case _: Val.False => Val.False(e.pos)
      case unknown =>
        Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitOr(e: Or)(implicit scope: ValScope) = {
    visitExpr(e.lhs) match {
      case _: Val.True => Val.True(e.pos)
      case _: Val.False =>
        visitExpr(e.rhs) match{
          case b: Val.Bool => b
          case unknown =>
            Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case unknown =>
        Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitNullCoalesce(e: NullCoal)(implicit scope: ValScope) = {
    visitExpr(e.lhs) match {
      case _: Val.Null => visitExpr(e.rhs)
      case any => any
    }
  }

  def visitInSuper(e: InSuper)(implicit scope: ValScope) = {
    val sup = scope.bindings(e.selfIdx+1).asInstanceOf[Val.Obj]
    if(sup == null) Val.False(e.pos)
    else {
      val key = visitExpr(e.value).cast[Val.Str]
      Val.bool(e.pos, sup.containsKey(key.value))
    }
  }

  def visitBinaryOp(e: BinaryOp)(implicit scope: ValScope) = {
    val l = visitExpr(e.lhs)
    val r = visitExpr(e.rhs)
    val pos = e.pos
    def fail() = Error.fail(s"Unknown binary operation: ${l.prettyName} ${Expr.BinaryOp.name(e.op)} ${r.prettyName}", pos)
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

      case Expr.BinaryOp.OP_+ => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l + r)
        case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
        case (Val.Str(_, l), r) => Val.Str(pos, l + Materializer.stringify(r))
        case (l, Val.Str(_, r)) => Val.Str(pos, Materializer.stringify(l) + r)
        case (l: Val.Obj, r: Val.Obj) => r.addSuper(pos, l)
        case (l: Val.Arr, r: Val.Arr) => l.concat(pos, r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_- => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l - r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_* => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l * r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_/ => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) =>
          if (r == 0) Error.fail("division by zero", pos)
          Val.Num(pos, l / r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_% => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l % r)
        case (Val.Str(_, l), r) => Val.Str(pos, Format.format(l, r, pos))
        case _ => fail()
      }

      case Expr.BinaryOp.OP_< => (l, r) match {
        case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l < r)
        case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l < r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_> => (l, r) match {
        case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l > r)
        case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l > r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_<= => (l, r) match {
        case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l <= r)
        case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l <= r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_>= => (l, r) match {
        case (Val.Str(_, l), Val.Str(_, r)) => Val.bool(pos, l >= r)
        case (Val.Num(_, l), Val.Num(_, r)) => Val.bool(pos, l >= r)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_<< => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l.toLong << r.toLong)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_>> => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l.toLong >> r.toLong)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_in => (l, r) match {
        case (Val.Str(_, l), o: Val.Obj) => Val.bool(pos, o.containsKey(l))
        case _ => fail()
      }

      case Expr.BinaryOp.OP_& => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l.toLong & r.toLong)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_^ => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l.toLong ^ r.toLong)
        case _ => fail()
      }

      case Expr.BinaryOp.OP_| => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l.toLong | r.toLong)
        case _ => fail()
      }

      case _ => fail()
    }
  }

  def visitFieldName(fieldName: FieldName, pos: Position)(implicit scope: ValScope): String = {
    fieldName match {
      case FieldName.Fixed(s) => s
      case FieldName.Dyn(k) => visitExpr(k) match{
        case Val.Str(_, k1) => k1
        case Val.Null(_) => null
        case x => Error.fail(
          s"Field name must be string or null, not ${x.prettyName}",
          pos
        )
      }
    }
  }

  def visitMethod(rhs: Expr, params: Params, outerPos: Position)(implicit scope: ValScope) =
    new Val.Func(outerPos, scope, params) {
      def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position): Val = visitExpr(rhs)(vs)
      override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope) = visitExpr(expr)(vs)
    }

  def visitBindings(bindings: Array[Bind], scope: (Val.Obj, Val.Obj) => ValScope): Array[(Val.Obj, Val.Obj) => Lazy] = {
    val arrF = new Array[(Val.Obj, Val.Obj) => Lazy](bindings.length)
    var i = 0
    while(i < bindings.length) {
      val b = bindings(i)
      arrF(i) = b.args match {
        case null =>
          (self: Val.Obj, sup: Val.Obj) => () => visitExpr(b.rhs)(scope(self, sup))
        case argSpec =>
          (self: Val.Obj, sup: Val.Obj) => () => visitMethod(b.rhs, argSpec, b.pos)(scope(self, sup))
      }
      i += 1
    }
    arrF
  }

  def visitMemberList(objPos: Position, e: ObjBody.MemberList, sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val asserts = e.asserts
    val fields = e.fields
    var cachedSimpleScope: ValScope = null.asInstanceOf[ValScope]
    var cachedObj: Val.Obj = null
    var asserting: Boolean = false

    def makeNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      if((sup eq null) && (self eq cachedObj)) {
        if(cachedSimpleScope == null.asInstanceOf[ValScope]) cachedSimpleScope = createNewScope(self, sup)
        cachedSimpleScope
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
      while(i < asserts.length) {
        val a = asserts(i)
        if (!visitExpr(a.value)(newScope).isInstanceOf[Val.True]) {
          a.msg match {
            case null => Error.fail("Assertion failed", a.value.pos, "Assert")
            case msg =>
              Error.fail(
                "Assertion failed: " + visitExpr(msg)(newScope).cast[Val.Str].value,
                a.value.pos, "Assert"
              )
          }
        }
        i += 1
      }
    }

    def createNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      val scopeLen = scope.length
      val binds = e.binds
      val by = if(binds == null) 2 else 2 + binds.length
      val newScope = scope.extendBy(by)
      newScope.bindings(scopeLen) = self
      newScope.bindings(scopeLen+1) = sup
      if(binds != null) {
        val arrF = newScope.bindings
        var i = 0
        var j = scopeLen+2
        while(i < binds.length) {
          val b = binds(i)
          arrF(j) = b.args match {
            case null =>
              visitAsLazy(b.rhs)(newScope)
            case argSpec =>
              () => visitMethod(b.rhs, argSpec, b.pos)(newScope)
          }
          i += 1
          j += 1
        }
      }
      newScope
    }

    val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
    fields.foreach {
      case Member.Field(offset, fieldName, plus, null, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if(k != null) {
          val v = new Val.Obj.Member(plus, sep) {
            def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
              if(asserts != null) assertions(self)
              visitExpr(rhs)(makeNewScope(self, sup))
            }
          }
          builder.put(k, v)
        }
      case Member.Field(offset, fieldName, false, argSpec, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if(k != null) {
          val v = new Val.Obj.Member(false, sep) {
            def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
              if(asserts != null) assertions(self)
              visitMethod(rhs, argSpec, offset)(makeNewScope(self, sup))
            }
          }
          builder.put(k, v)
        }
    }
    cachedObj = new Val.Obj(objPos, builder, false, if(asserts != null) assertions else null, sup)
    cachedObj
  }

  def visitObjComp(e: ObjBody.ObjComp, sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val binds = e.preLocals ++ e.postLocals
    val compScope: ValScope = scope //.clearSuper

    lazy val newSelf: Val.Obj = {
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      for(s <- visitComp(e.first :: e.rest, Array(compScope))){
        lazy val newScope: ValScope = s.extend(newBindings, newSelf, null)

        lazy val newBindings = visitBindings(binds, (self, sup) => newScope)

        visitExpr(e.key)(s) match {
          case Val.Str(_, k) =>
            val prev_length = builder.size()
            builder.put(k, new Val.Obj.Member(e.plus, Visibility.Normal) {
              def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
                visitExpr(e.value)(
                  s.extend(newBindings, self, null)
                )
            })
            if (prev_length == builder.size() && settings.noDuplicateKeysInComprehension) {
              Error.fail(s"Duplicate key ${k} in evaluated object comprehension.", e.pos);
            }
          case Val.Null(_) => // do nothing
        }
      }
      new Val.Obj(e.pos, builder, false, null, sup)
    }

    newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Array[ValScope]): Array[ValScope] = f match{
    case (spec @ ForSpec(_, name, expr)) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr)(s) match{
            case a: Val.Arr => a.asLazyArray
            case r => Error.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              spec
            )
          }
        } yield s.extendSimple(e)
      )
    case (spec @ IfSpec(offset, expr)) :: rest =>
      visitComp(rest, scopes.filter(visitExpr(expr)(_) match {
        case Val.True(_) => true
        case Val.False(_) => false
        case other => Error.fail(
          "Condition must be boolean, got " + other.prettyName,
          spec
        )
      }))
    case Nil => scopes
  }

  def equal(x: Val, y: Val): Boolean = (x eq y) || (x match {
    case _: Val.True => y.isInstanceOf[Val.True]
    case _: Val.False => y.isInstanceOf[Val.False]
    case _: Val.Null => y.isInstanceOf[Val.Null]
    case x: Val.Str => y match {
      case y: Val.Str => x.value == y.value
      case _ => false
    }
    case x: Val.Num => y match {
      case y: Val.Num => x.value == y.value
      case _ => false
    }
    case x: Val.Arr => y match {
      case y: Val.Arr =>
        val xlen = x.length
        if(xlen != y.length) return false
        var i = 0
        while(i < xlen) {
          if(!equal(x.force(i), y.force(i))) return false
          i += 1
        }
        true
      case _ => false
    }
    case x: Val.Obj => y match {
      case y: Val.Obj =>
        val k1 = x.visibleKeyNames
        val k2 = y.visibleKeyNames
        val k1len = k1.length
        if(k1len != k2.length) return false
        x.triggerAllAsserts(x)
        y.triggerAllAsserts(y)
        var i = 0
        while(i < k1len) {
          val k = k1(i)
          if(!y.containsKey(k)) return false
          val v1 = x.value(k, emptyMaterializeFileScopePos)
          val v2 = y.value(k, emptyMaterializeFileScopePos)
          if(!equal(v1, v2)) return false
          i += 1
        }
        true
      case _ => false
    }
    case _ => false
  })
}

object Evaluator {
  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Lazy](0)
}
