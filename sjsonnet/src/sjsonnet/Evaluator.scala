package sjsonnet

import Expr.{Error => _, _}
import fastparse.Parsed
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
class Evaluator(parseCache: collection.mutable.HashMap[(Path, String), fastparse.Parsed[(Expr, FileScope)]],
                val extVars: Map[String, ujson.Value],
                val wd: Path,
                importer: (Path, String) => Option[(Path, String)],
                override val preserveOrder: Boolean = false,
                strict: Boolean) extends EvalScope{
  implicit def evalScope: EvalScope = this

  val loadedFileContents = mutable.HashMap.empty[Path, String]
  def loadCachedSource(p: Path) = loadedFileContents.get(p)
  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports = collection.mutable.HashMap.empty[Path, Val]
  val cachedImportedStrings = collection.mutable.HashMap.empty[Path, String]

  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val = try {
    expr match {
      case lit: Val.Literal => lit
      case Self(pos) =>
        val self = scope.self0
        if(self == null) Error.fail("Cannot use `self` outside an object", pos)
        self

      case BinaryOp(pos, lhs, Expr.BinaryOp.`in`, Super(_)) =>
        if(scope.super0 == null) Val.False(pos)
        else {
          val key = visitExpr(lhs).cast[Val.Str]
          Val.bool(pos, scope.super0.containsKey(key.value))
        }

      case BinaryOp(pos, lhs, Expr.BinaryOp.`&&`, rhs) =>
        visitExpr(lhs) match {
          case Val.True(_) =>
            visitExpr(rhs) match{
              case b: Val.Bool => b
              case unknown =>
                Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", pos)
            }
          case Val.False(_) => Val.False(pos)
          case unknown =>
            Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", pos)
        }

      case BinaryOp(pos, lhs, Expr.BinaryOp.`||`, rhs) =>
        visitExpr(lhs) match {
          case Val.True(_) => Val.True(pos)
          case Val.False(_) =>
            visitExpr(rhs) match{
              case b: Val.Bool => b
              case unknown =>
                Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", pos)
            }
          case unknown =>
            Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", pos)
        }

      case BinaryOp(pos, lhs, op, rhs) => visitBinaryOp(pos, lhs, op, rhs)

      case $(pos) =>
        val dollar = scope.dollar0
        if(dollar == null) Error.fail("Cannot use `$` outside an object", pos)
        dollar
      case Id(pos, value) => visitId(pos, value)

      case Arr(pos, value) => new Val.Arr(pos, value.map(v => (() => visitExpr(v)): Val.Lazy))
      case ObjBody.MemberList(pos, binds, fields, asserts) => visitMemberList(pos, pos, binds, fields, asserts, null)
      case ObjBody.ObjComp(pos, preLocals, key, value, postLocals, first, rest) => visitObjComp(pos, preLocals, key, value, postLocals, first, rest, null)

      case UnaryOp(pos, op, value) => visitUnaryOp(pos, op, value)

      case AssertExpr(pos, Member.AssertStmt(value, msg), returned) =>
        visitAssert(pos, value, msg, returned)

      case LocalExpr(pos, bindings, returned) =>
        val s =
          if(bindings == null) scope else {
            lazy val newScope: ValScope = {
              val f = visitBindings(bindings, (self, sup) => newScope)
              scope.extend(bindings, f)
            }
            newScope
          }
        visitExpr(returned)(s)

      case Import(pos, value) => visitImport(pos, value)
      case ImportStr(pos, value) => visitImportStr(pos, value)
      case Expr.Error(pos, value) => visitError(pos, value)
      case Apply(pos, value, argNames, argExprs) => visitApply(pos, value, argNames, argExprs)

      case Select(pos, value, name) => visitSelect(pos, value, name)

      case Lookup(pos, value, index) => visitLookup(pos, value, index)

      case Slice(pos, value, start, end, stride) => visitSlice(pos, value, start, end, stride)
      case Function(pos, params, body) => visitMethod(body, params, pos)
      case IfElse(pos, cond, then0, else0) => visitIfElse(pos, cond, then0, else0)
      case Comp(pos, value, first, rest) =>
        new Val.Arr(pos, visitComp(first :: rest.toList, Array(scope)).map(s => (() => visitExpr(value)(s)): Val.Lazy))
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
    }
  } catch Error.tryCatch(expr.pos)

  private def isObjLiteral(expr: Expr): Boolean = expr match {
    case _: ObjBody.MemberList => true
    case _: ObjBody.ObjComp => true
    case _: ObjExtend => true
    case _: Val.Obj => true
    case _ => false
  }

  def visitId(pos: Position, value: Int)(implicit scope: ValScope): Val = {
    val ref = scope.bindings(value)
    if(ref == null)
      Error.fail("Unknown variable " + pos.fileScope.indexNames(value), pos)
    try ref.force catch Error.tryCatchWrap(pos)
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

  def visitUnaryOp(pos: Position, op: UnaryOp.Op, value: Expr)
                  (implicit scope: ValScope): Val = {
    (op, visitExpr(value)) match {
      case (Expr.UnaryOp.`-`, Val.Num(_, v)) => Val.Num(pos, -v)
      case (Expr.UnaryOp.`+`, Val.Num(_, v)) => Val.Num(pos, v)
      case (Expr.UnaryOp.`~`, Val.Num(_, v)) => Val.Num(pos, ~v.toLong)
      case (Expr.UnaryOp.`!`, Val.True(_)) => Val.False(pos)
      case (Expr.UnaryOp.`!`, Val.False(_)) => Val.True(pos)
    }
  }

  private def visitApply(pos: Position, value: Expr, argNames: Array[String], argExprs: Array[Expr])
                        (implicit scope: ValScope) = {
    val lhs = visitExpr(value)
    val arr = new Array[Val.Lazy](argExprs.length)
    var idx = 0
    while (idx < argExprs.length) {
      val boundIdx = idx
      arr(idx) = () => visitExpr(argExprs(boundIdx))
      idx += 1
    }

    try lhs.cast[Val.Func].apply(
      argNames, arr,
      pos
    )
    catch Error.tryCatchWrap(pos)
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
    if (value.isInstanceOf[Super]) {
      val key = visitExpr(index).cast[Val.Str]
      var s = scope.super0
      if(s == null) s = scope.self0
      if(s == null) Error.fail("Cannot use `super` outside an object", pos)
      else s.value(key.value, pos)
    } else (visitExpr(value), visitExpr(index)) match {
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

  def visitSelect(pos: Position, value: Expr, name: String)(implicit scope: ValScope): Val = {
    if (value.isInstanceOf[Super]) {
      if(scope.super0 == null) Error.fail("Cannot use `super` outside an object", pos)
      else  scope.super0.value(name, pos, scope.self0)
    } else visitExpr(value) match {
      case obj: Val.Obj => obj.value(name, pos)
      case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${name}", pos)
    }
  }

  def visitImportStr(pos: Position, value: String)(implicit scope: ValScope) = {
    val (p, str) = resolveImport(value, pos)
    Val.Str(pos, cachedImportedStrings.getOrElseUpdate(p, str))
  }

  def visitImport(pos: Position, value: String)(implicit scope: ValScope) = {
    val key @ (p, str) = resolveImport(value, pos)
    loadedFileContents(p) = str
    cachedImports.getOrElseUpdate(
      p,
      {
        val (doc, newFileScope) = parseCache.getOrElseUpdate(
          key,
          fastparse.parse(str, new Parser(p).document(_))
        ) match {
          case Parsed.Success((doc, nameIndices), _) => (doc, nameIndices)
          case f @ Parsed.Failure(l, i, e) =>
            Error.fail(
              "Imported file " + pprint.Util.literalize(value) +
                " had Parse error. " + f.trace().msg,
              pos
            )
        }
        try visitExpr(doc)(Std.scope(newFileScope.nameIndices.size))
        catch Error.tryCatchWrap(pos)
      }
    )
  }

  def resolveImport(value: String, pos: Position)
                   (implicit scope: ValScope): (Path, String) = {
    importer(pos.fileScope.currentFile.parent(), value)
      .getOrElse(
        Error.fail(
          "Couldn't import file: " + pprint.Util.literalize(value),
          pos
        )
      )
  }

  def visitBinaryOp(pos: Position, lhs: Expr, op: BinaryOp.Op, rhs: Expr)(implicit scope: ValScope) = {
    (visitExpr(lhs), op, visitExpr(rhs)) match {

      case (l, Expr.BinaryOp.`==`, r) =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        try Val.bool(pos, equal(l, r))
        catch Error.tryCatchWrap(pos)

      case (l, Expr.BinaryOp.`!=`, r) =>
        if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
          Error.fail("cannot test equality of functions", pos)
        }
        try Val.bool(pos, !equal(l, r))
        catch Error.tryCatchWrap(pos)

      case (Val.Num(_, l), Expr.BinaryOp.`+`, Val.Num(_, r)) => Val.Num(pos, l + r)
      case (Val.Str(_, l), Expr.BinaryOp.`+`, Val.Str(_, r)) => Val.Str(pos, l + r)
      case (Val.Str(_, l), Expr.BinaryOp.`+`, r) =>
        try Val.Str(pos, l + Materializer.stringify(r))
        catch Error.tryCatchWrap(pos)
      case (l, Expr.BinaryOp.`+`, Val.Str(_, r)) =>
        try Val.Str(pos, Materializer.stringify(l) + r)
        catch Error.tryCatchWrap(pos)
      case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => r.addSuper(pos, l)
      case (l: Val.Arr, Expr.BinaryOp.`+`, r: Val.Arr) => l.concat(pos, r)

      case (Val.Num(_, l), Expr.BinaryOp.`-`, Val.Num(_, r)) => Val.Num(pos, l - r)

      case (Val.Num(_, l), Expr.BinaryOp.`*`, Val.Num(_, r)) => Val.Num(pos, l * r)

      case (Val.Num(_, l), Expr.BinaryOp.`/`, Val.Num(_, r)) =>
        if (r == 0) Error.fail("division by zero", pos)
        Val.Num(pos, l / r)

      case (Val.Num(_, l), Expr.BinaryOp.`%`, Val.Num(_, r)) => Val.Num(pos, l % r)
      case (Val.Str(_, l), Expr.BinaryOp.`%`, r) =>
        try Val.Str(pos, Format.format(l, r, pos))
        catch Error.tryCatchWrap(pos)

      case (Val.Str(_, l), Expr.BinaryOp.`<`, Val.Str(_, r)) => Val.bool(pos, l < r)
      case (Val.Num(_, l), Expr.BinaryOp.`<`, Val.Num(_, r)) => Val.bool(pos, l < r)

      case (Val.Str(_, l), Expr.BinaryOp.`>`, Val.Str(_, r)) => Val.bool(pos, l > r)
      case (Val.Num(_, l), Expr.BinaryOp.`>`, Val.Num(_, r)) => Val.bool(pos, l > r)

      case (Val.Str(_, l), Expr.BinaryOp.`<=`, Val.Str(_, r)) => Val.bool(pos, l <= r)
      case (Val.Num(_, l), Expr.BinaryOp.`<=`, Val.Num(_, r)) => Val.bool(pos, l <= r)

      case (Val.Str(_, l), Expr.BinaryOp.`>=`, Val.Str(_, r)) => Val.bool(pos, l >= r)
      case (Val.Num(_, l), Expr.BinaryOp.`>=`, Val.Num(_, r)) => Val.bool(pos, l >= r)

      case (Val.Num(_, l), Expr.BinaryOp.`<<`, Val.Num(_, r)) => Val.Num(pos, l.toLong << r.toLong)

      case (Val.Num(_, l), Expr.BinaryOp.`>>`, Val.Num(_, r)) => Val.Num(pos, l.toLong >> r.toLong)

      case (Val.Str(_, l), Expr.BinaryOp.`in`, o: Val.Obj) => Val.bool(pos, o.containsKey(l))

      case (Val.Num(_, l), Expr.BinaryOp.`&`, Val.Num(_, r)) => Val.Num(pos, l.toLong & r.toLong)

      case (Val.Num(_, l), Expr.BinaryOp.`^`, Val.Num(_, r)) => Val.Num(pos, l.toLong ^ r.toLong)

      case (Val.Num(_, l), Expr.BinaryOp.`|`, Val.Num(_, r)) => Val.Num(pos, l.toLong | r.toLong)

      case (l, op, r) =>
        Error.fail(s"Unknown binary operation: ${l.prettyName} $op ${r.prettyName}", pos)
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

  def visitMethod(rhs: Expr, params: Params, outerPos: Position)(implicit scope: ValScope) = {
    Val.Func(
      outerPos,
      scope,
      params,
      (s, _, fs, _) => visitExpr(rhs)(s),
      (default, s, e) => visitExpr(default)(s)
    )
  }

  def visitBindings(bindings: Array[Bind], scope: (Val.Obj, Val.Obj) => ValScope): Array[(Val.Obj, Val.Obj) => Val.Lazy] = {
    val arrF = new Array[(Val.Obj, Val.Obj) => Val.Lazy](bindings.length)
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
    var asserting: Boolean = false
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

    def makeNewScope(self: Val.Obj, sup: Val.Obj): ValScope = {
      scope.extend(
        binds,
        newBindings,
        newDollar = if(scope.dollar0 != null) scope.dollar0 else self,
        newSelf = self,
        newSuper = sup
      )
    }

    lazy val newBindings =
      if(binds == null) null
      else visitBindings(binds, (self, sup) => makeNewScope(self, sup))

    val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
    fields.foreach {
      case Member.Field(offset, fieldName, plus, null, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if(k != null) {
          val v = Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Val.Obj, _, _) => {
            if(asserts != null) assertions(self)
            visitExpr(rhs)(makeNewScope(self, sup))
          })
          builder.put(k, v)
        }
      case Member.Field(offset, fieldName, false, argSpec, sep, rhs) =>
        val k = visitFieldName(fieldName, offset)
        if(k != null) {
          val v = Val.Obj.Member(false, sep, (self: Val.Obj, sup: Val.Obj, _, _) => {
            if(asserts != null) assertions(self)
            visitMethod(rhs, argSpec, offset)(makeNewScope(self, sup))
          })
          builder.put(k, v)
        }
    }
    new Val.Obj(objPos, builder, false, if(asserts != null) assertions else null, sup)
  }

  def visitObjComp(objPos: Position, preLocals: Array[Bind], key: Expr, value: Expr, postLocals: Array[Bind], first: ForSpec, rest: List[CompSpec], sup: Val.Obj)(implicit scope: ValScope): Val.Obj = {
    val binds = preLocals ++ postLocals
    lazy val compScope: ValScope = scope.extend(
      newSuper = null
    )

    lazy val newSelf: Val.Obj = {
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      for(s <- visitComp(first :: rest, Array(compScope))){
        lazy val newScope: ValScope = s.extend(
          binds,
          newBindings,
          newDollar = if(scope.dollar0 != null) scope.dollar0 else newSelf,
          newSelf = newSelf,
          newSuper = null
        )

        lazy val newBindings = visitBindings(binds, (self, sup) => newScope)

        visitExpr(key)(s) match {
          case Val.Str(_, k) =>
            builder.put(k, Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Val.Obj, _, _) =>
              visitExpr(value)(
                s.extend(
                  binds,
                  newBindings,
                  newDollar = if(s.dollar0 != null) s.dollar0 else self,
                  newSelf = self,
                )
              )
            ))
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
        } yield s.extendSimple(name, e)
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
    def normalize(x: Val): Val = x match {
      case f: Val.Func => f.apply(Evaluator.emptyStringArray, Evaluator.emptyLazyArray, emptyMaterializeFileScopePos)
      case x => x
    }
    (normalize(x), normalize(y)) match {
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
  val emptyLazyArray = new Array[Val.Lazy](0)
}