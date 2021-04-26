package sjsonnet

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
                val extVars: Map[String, ujson.Value],
                val wd: Path,
                override val preserveOrder: Boolean = false,
                strict: Boolean) extends EvalScope{
  implicit def evalScope: EvalScope = this
  def importer: CachedImporter = resolver

  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports = collection.mutable.HashMap.empty[Path, Val]

  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val = try {
    expr match {
      case e: ValidId =>
        val ref = scope.bindings(e.nameIdx)
        try ref.force catch Error.tryCatchWrap(e.pos)

      case e: BinaryOp => visitBinaryOp(e.pos, e.lhs, e.op, e.rhs)

      case s: Select =>
        visitExpr(s.value) match {
          case obj: Val.Obj => obj.value(s.name, s.pos)
          case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${s.name}", s.pos)
        }

      case lit: Val => lit

      case ApplyBuiltin1(pos, func, a1) => visitApplyBuiltin1(pos, func, a1)
      case ApplyBuiltin2(pos, func, a1, a2) => visitApplyBuiltin2(pos, func, a1, a2)

      case e: And => visitAnd(e)
      case e: Or => visitOr(e)

      case UnaryOp(pos, op, value) => visitUnaryOp(pos, op, value)

      case Apply1(pos, value, a1) => visitApply1(pos, value, a1)

      case Lookup(pos, value, index) => visitLookup(pos, value, index)

      case Function(pos, params, body) => visitMethod(body, params, pos)

      case LocalExpr(pos, bindings, returned) =>
        val s =
          if(bindings == null) scope else {
            val base = scope.length
            val newScope = scope.extendBy(bindings.length)
            var i = 0
            while(i < bindings.length) {
              val b = bindings(i)
              newScope.bindings(base+i) = b.args match {
                case null => () => visitExpr(b.rhs)(newScope)
                case argSpec => () => visitMethod(b.rhs, argSpec, b.pos)(newScope)
              }
              i += 1
            }
            newScope
          }
        visitExpr(returned)(s)

      case Apply(pos, value, args, namedNames) => visitApply(pos, value, args, namedNames)

      case IfElse(pos, cond, then0, else0) => visitIfElse(pos, cond, then0, else0)

      case Apply3(pos, value, a1, a2, a3) => visitApply3(pos, value, a1, a2, a3)

      case ObjBody.MemberList(pos, binds, fields, asserts) => visitMemberList(pos, pos, binds, fields, asserts, null)

      case Apply2(pos, value, a1, a2) => visitApply2(pos, value, a1, a2)

      case AssertExpr(pos, Member.AssertStmt(value, msg), returned) =>
        visitAssert(pos, value, msg, returned)

      case ApplyBuiltin(pos, func, argExprs) => visitApplyBuiltin(pos, func, argExprs)

      case Comp(pos, value, first, rest) =>
        new Val.Arr(pos, visitComp(first :: rest.toList, Array(scope)).map(s => (() => visitExpr(value)(s)): Lazy))

      case Arr(pos, value) => new Val.Arr(pos, value.map(v => (() => visitExpr(v)): Lazy))

      case SelectSuper(pos, selfIdx, name) =>
        val sup = scope.bindings(selfIdx+1).asInstanceOf[Val.Obj]
        if(sup == null) Error.fail("Cannot use `super` outside an object", pos)
        else sup.value(name, pos, scope.bindings(selfIdx).asInstanceOf[Val.Obj])

      case LookupSuper(pos, selfIdx, index) => visitLookupSuper(pos, selfIdx, index)

      case e: InSuper => visitInSuper(e)

      case ObjExtend(superPos, value, ext) => {
        if(strict && isObjLiteral(value))
          Error.fail("Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects", superPos)
        val original = visitExpr(value).cast[Val.Obj]
        ext match {
          case ObjBody.MemberList(pos, binds, fields, asserts) => visitMemberList(pos, superPos, binds, fields, asserts, original)
          case ObjBody.ObjComp(pos, preLocals, key, value, postLocals, first, rest) => visitObjComp(superPos, preLocals, key, value, postLocals, first, rest, original)
          case o: Val.Obj => o.addSuper(superPos, original)
        }
      }

      case ObjBody.ObjComp(pos, preLocals, key, value, postLocals, first, rest) => visitObjComp(pos, preLocals, key, value, postLocals, first, rest, null)

      case Slice(pos, value, start, end, stride) => visitSlice(pos, value, start, end, stride)

      case Import(pos, value) => visitImport(pos, value)

      case Apply0(pos, value) => visitApply0(pos, value)

      case ImportStr(pos, value) => visitImportStr(pos, value)

      case Expr.Error(pos, value) => visitError(pos, value)

      case Id(pos, name) =>
        Error.fail("Unknown variable " + name, pos)

      case Self(pos) =>
        Error.fail("Cannot use `self` outside an object", pos)

      case $(pos) =>
        Error.fail("Cannot use `$` outside an object", pos)

      case Super(pos) =>
        Error.fail("Cannot use `super` outside an object", pos)
    }
  } catch Error.tryCatch(expr.pos)

  private def isObjLiteral(expr: Expr): Boolean = expr match {
    case _: ObjBody.MemberList => true
    case _: ObjBody.ObjComp => true
    case _: ObjExtend => true
    case _: Val.Obj => true
    case _ => false
  }

  def visitIfElse(pos: Position,
                  cond: Expr,
                  then: Expr,
                  else0: Expr)
                 (implicit scope: ValScope): Val = {
    visitExpr(cond) match {
      case Val.True(_) => visitExpr(then)
      case Val.False(_) =>
        else0 match {
          case null => Val.Null(pos)
          case v => visitExpr(v)
        }
      case v => Error.fail("Need boolean, found " + v.prettyName, pos)
    }
  }

  def visitError(pos: Position, value: Expr)
                (implicit scope: ValScope): Nothing = {
    Error.fail(
      visitExpr(value) match {
        case Val.Str(_, s) => s
        case r =>
          try Materializer.stringify(r)
          catch Error.tryCatchWrap(pos)
      },
      pos
    )
  }

  def visitUnaryOp(pos: Position, op: Int, value: Expr)
                  (implicit scope: ValScope): Val = {
    val v = visitExpr(value)
    def fail() = Error.fail(s"Unknown unary operation: ${Expr.UnaryOp.name(op)} ${v.prettyName}", pos)
    op match {
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

  private def visitApply(pos: Position, value: Expr, args: Array[Expr], namedNames: Array[String])
                        (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    val argsL = new Array[Lazy](args.length)
    var idx = 0
    while (idx < args.length) {
      val a = args(idx)
      argsL(idx) = () => visitExpr(a)
      idx += 1
    }
    try lhs.cast[Val.Func].apply(argsL, namedNames, pos) catch Error.tryCatchWrap(pos)
  }

  private def visitApply0(pos: Position, value: Expr)
                         (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    try lhs.cast[Val.Func].apply0(pos) catch Error.tryCatchWrap(pos)
  }

  private def visitApply1(pos: Position, value: Expr, a1: Expr)
                         (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    val l1: Lazy = () => visitExpr(a1)
    try lhs.cast[Val.Func].apply1(l1, pos) catch Error.tryCatchWrap(pos)
  }

  private def visitApply2(pos: Position, value: Expr, a1: Expr, a2: Expr)
                         (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    val l1: Lazy = () => visitExpr(a1)
    val l2: Lazy = () => visitExpr(a2)
    try lhs.cast[Val.Func].apply2(l1, l2, pos) catch Error.tryCatchWrap(pos)
  }

  private def visitApply3(pos: Position, value: Expr, a1: Expr, a2: Expr, a3: Expr)
                         (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    val l1: Lazy = () => visitExpr(a1)
    val l2: Lazy = () => visitExpr(a2)
    val l3: Lazy = () => visitExpr(a3)
    try lhs.cast[Val.Func].apply3(l1, l2, l3, pos) catch Error.tryCatchWrap(pos)
  }

  private def visitApplyBuiltin1(pos: Position, func: Val.Builtin1, a1: Expr)
                                (implicit scope: ValScope) =
    try func.evalRhs(visitExpr(a1), this, pos) catch Error.tryCatchWrap(pos)

  private def visitApplyBuiltin2(pos: Position, func: Val.Builtin2, a1: Expr, a2: Expr)
                                (implicit scope: ValScope) =
    try func.evalRhs(visitExpr(a1), visitExpr(a2), this, pos) catch Error.tryCatchWrap(pos)

  private def visitApplyBuiltin(pos: Position, func: Val.Builtin, argExprs: Array[Expr])
                               (implicit scope: ValScope) = {
    val arr = new Array[Val](argExprs.length)
    var idx = 0
    while (idx < argExprs.length) {
      val boundIdx = idx
      arr(idx) = visitExpr(argExprs(boundIdx))
      idx += 1
    }
    try func.evalRhs(arr, this, pos) catch Error.tryCatchWrap(pos)
  }

  def visitAssert(pos: Position, value: Expr, msg: Expr, returned: Expr)
                 (implicit scope: ValScope): Val = {
    if (!visitExpr(value).isInstanceOf[Val.True]) {
      msg match {
        case null => Error.fail("Assertion failed", pos)
        case msg =>
          Error.fail(
            "Assertion failed: " + visitExpr(msg).cast[Val.Str].value,
            pos
          )
      }
    }
    visitExpr(returned)
  }

  private def visitSlice(pos: Position,
                         value: Expr,
                         start: Option[Expr],
                         end: Option[Expr],
                         stride: Option[Expr])
                        (implicit scope: ValScope)= {
    visitExpr(value) match {
      case a: Val.Arr =>
        a.slice(start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt),
          end.fold(a.length)(visitExpr(_).cast[Val.Num].value.toInt),
          stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt))
      case Val.Str(_, s) =>
        val range =
          start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt) until
            end.fold(s.length)(visitExpr(_).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
        Val.Str(pos, range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).mkString)
      case x => Error.fail("Can only slice array or string, not " + x.prettyName, pos)
    }
  }

  def visitLookup(pos: Position, value: Expr, index: Expr)
                 (implicit scope: ValScope): Val = {
    (visitExpr(value), visitExpr(index)) match {
      case (v: Val.Arr, i: Val.Num) =>
        if (i.value > v.length) Error.fail(s"array bounds error: ${i.value} not within [0, ${v.length})", pos)
        val int = i.value.toInt
        if (int != i.value) Error.fail("array index was not integer: " + i.value, pos)
        try v.force(int)
        catch Error.tryCatchWrap(pos)
      case (v: Val.Str, i: Val.Num) => Val.Str(pos, new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        val ref = v.value(i.value, pos)
        try ref
        catch Error.tryCatchWrap(pos)
      case (lhs, rhs) =>
        Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", pos)
    }
  }

  def visitLookupSuper(pos: Position, selfIdx: Int, index: Expr)
                 (implicit scope: ValScope): Val = {
    var sup = scope.bindings(selfIdx+1).asInstanceOf[Val.Obj]
    val key = visitExpr(index).cast[Val.Str]
    if(sup == null) sup = scope.bindings(selfIdx).asInstanceOf[Val.Obj]
    sup.value(key.value, pos)
  }

  def visitImportStr(pos: Position, value: String)(implicit scope: ValScope) =
    Val.Str(pos, importer.resolveAndReadOrFail(value, pos)._2)

  def visitImport(pos: Position, value: String)(implicit scope: ValScope) = {
    val (p, str) = importer.resolveAndReadOrFail(value, pos)
    cachedImports.getOrElseUpdate(
      p,
      {
        val (doc, newFileScope) = resolver.parseOrFail(pos, value, p, str)
        try visitExpr(doc)(ValScope.empty)
        catch Error.tryCatchWrap(pos)
      }
    )
  }

  def visitAnd(e: And)(implicit scope: ValScope) = {
    visitExpr(e.lhs) match {
      case Val.True(_) =>
        visitExpr(e.rhs) match{
          case b: Val.Bool => b
          case unknown =>
            Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case Val.False(_) => Val.False(e.pos)
      case unknown =>
        Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", e.pos)
    }
  }

  def visitOr(e: Or)(implicit scope: ValScope) = {
    visitExpr(e.lhs) match {
      case Val.True(_) => Val.True(e.pos)
      case Val.False(_) =>
        visitExpr(e.rhs) match{
          case b: Val.Bool => b
          case unknown =>
            Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
        }
      case unknown =>
        Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", e.pos)
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

  def visitBinaryOp(pos: Position, lhs: Expr, op: Int, rhs: Expr)(implicit scope: ValScope) = {
    val l = visitExpr(lhs)
    val r = visitExpr(rhs)
    def fail() = Error.fail(s"Unknown binary operation: ${l.prettyName} ${Expr.BinaryOp.name(op)} ${r.prettyName}", pos)
    op match {

      case Expr.BinaryOp.OP_== =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        try Val.bool(pos, equal(l, r))
        catch Error.tryCatchWrap(pos)

      case Expr.BinaryOp.OP_!= =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        try Val.bool(pos, !equal(l, r))
        catch Error.tryCatchWrap(pos)

      case Expr.BinaryOp.OP_+ => (l, r) match {
        case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l + r)
        case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
        case (Val.Str(_, l), r) =>
          try Val.Str(pos, l + Materializer.stringify(r))
          catch Error.tryCatchWrap(pos)
        case (l, Val.Str(_, r)) =>
          try Val.Str(pos, Materializer.stringify(l) + r)
          catch Error.tryCatchWrap(pos)
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
        case (Val.Str(_, l), r) =>
          try Val.Str(pos, Format.format(l, r, pos))
          catch Error.tryCatchWrap(pos)
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

  def visitMemberList(pos: Position, objPos: Position, binds: Array[Bind], fields: Array[Expr.Member.Field], asserts: Array[Expr.Member.AssertStmt], sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    var cachedSimpleScope: ValScope = null.asInstanceOf[ValScope]
    var cachedObj: Val.Obj = null
    var asserting: Boolean = false

    def makeNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      if((sup eq null) && (self eq cachedObj)) {
        if(cachedSimpleScope == null.asInstanceOf[ValScope]) cachedSimpleScope = createNewScope(self, sup)
        cachedSimpleScope
      } else createNewScope(self, sup)
    }

    def assertions(self: Val.Obj): Unit = if (!asserting) {
      asserting = true
      val newScope: ValScope = makeNewScope(self, self.getSuper)
      var i = 0
      while(i < asserts.length) {
        val a = asserts(i)
        if (!visitExpr(a.value)(newScope).isInstanceOf[Val.True]) {
          a.msg match {
            case null => Error.fail("Assertion failed", a.value.pos)
            case msg =>
              Error.fail(
                "Assertion failed: " + visitExpr(msg)(newScope).cast[Val.Str].value,
                a.value.pos
              )
          }
        }
        i += 1
      }
    }

    def createNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      val scopeLen = scope.length
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
              () => visitExpr(b.rhs)(newScope)
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

  def visitObjComp(objPos: Position, preLocals: Array[Bind], key: Expr, value: Expr, postLocals: Array[Bind], first: ForSpec, rest: List[CompSpec], sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val binds = preLocals ++ postLocals
    val compScope: ValScope = scope //.clearSuper

    lazy val newSelf: Val.Obj = {
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      for(s <- visitComp(first :: rest, Array(compScope))){
        lazy val newScope: ValScope = s.extend(newBindings, newSelf, null)

        lazy val newBindings = visitBindings(binds, (self, sup) => newScope)

        visitExpr(key)(s) match {
          case Val.Str(_, k) =>
            builder.put(k, new Val.Obj.Member(false, Visibility.Normal) {
              def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
                visitExpr(value)(
                  s.extend(newBindings, self, null)
                )
            })
          case Val.Null(_) => // do nothing
        }
      }
      new Val.Obj(objPos, builder, false, null, sup)
    }

    newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Array[ValScope]): Array[ValScope] = f match{
    case ForSpec(_, name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr)(s) match{
            case a: Val.Arr => a.asLazyArray
            case r => Error.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              expr.pos
            )
          }
        } yield s.extendSimple(e)
      )
    case IfSpec(offset, expr) :: rest =>
      visitComp(rest, scopes.filter(visitExpr(expr)(_) match {
        case Val.True(_) => true
        case Val.False(_) => false
        case other => Error.fail(
          "Condition must be boolean, got " + other.prettyName,
          expr.pos
        )
      }))
    case Nil => scopes
  }

  def equal(x: Val, y: Val): Boolean = (x eq y) || {
    (x, y) match {
      case (Val.True(_), y) => y.isInstanceOf[Val.True]
      case (Val.False(_), y) => y.isInstanceOf[Val.False]
      case (Val.Null(_), y) => y.isInstanceOf[Val.Null]
      case (Val.Num(_, n1), Val.Num(_, n2)) => n1 == n2
      case (Val.Str(_, s1), Val.Str(_, s2)) => s1 == s2
      case (xs: Val.Arr, ys: Val.Arr) =>
        if(xs.length != ys.length) return false
        var i = 0
        while(i < xs.length) {
          if(!equal(xs.force(i), ys.force(i))) return false
          i += 1
        }
        true
      case (o1: Val.Obj, o2: Val.Obj) =>
        val k1 = o1.visibleKeyNames
        val k2 = o2.visibleKeyNames
        if(k1.length != k2.length) return false
        o1.triggerAllAsserts(o1)
        o2.triggerAllAsserts(o2)
        var i = 0
        while(i < k1.length) {
          val k = k1(i)
          if(!o2.containsKey(k)) return false
          val v1 = o1.value(k, emptyMaterializeFileScopePos)
          val v2 = o2.value(k, emptyMaterializeFileScopePos)
          if(!equal(v1, v2)) return false
          i += 1
        }
        true
      case _ => false
    }
  }
}

object Evaluator {
  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Lazy](0)
}