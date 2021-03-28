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
class Evaluator(parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, FileScope)]],
                val extVars: Map[String, ujson.Value],
                val wd: Path,
                importer: (Path, String) => Option[(Path, String)],
                override val preserveOrder: Boolean = false,
                strict: Boolean) extends EvalScope{
  implicit def evalScope: EvalScope = this

  val loadedFileContents = mutable.Map.empty[Path, String]
  def loadCachedSource(p: Path) = loadedFileContents.get(p)
  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports = collection.mutable.Map.empty[Path, Val]
  val cachedImportedStrings = collection.mutable.Map.empty[Path, String]

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
          Val.bool(pos, scope.super0.visibleKeys.containsKey(key.value))
        }

      case $(pos) =>
        val dollar = scope.dollar0
        if(dollar == null) Error.fail("Cannot use `$` outside an object", pos)
        dollar
      case Id(pos, value) => visitId(pos, value)

      case Arr(pos, value) => Val.Arr(pos, value.map(v => (() => visitExpr(v)): Val.Lazy))
      case Obj(pos, value) => visitObjBody(pos, value)

      case UnaryOp(pos, op, value) => visitUnaryOp(pos, op, value)

      case BinaryOp(pos, lhs, op, rhs) => {
        visitBinaryOp(pos, lhs, op, rhs)
      }
      case AssertExpr(pos, Member.AssertStmt(value, msg), returned) =>
        visitAssert(pos, value, msg, returned)

      case LocalExpr(pos, bindings, returned) =>
        lazy val newScope: ValScope = {
          val f = visitBindings(bindings, (self, sup) => newScope)
          scope.extend(bindings, f)
        }
        visitExpr(returned)(newScope)

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
        Val.Arr(pos, visitComp(first :: rest.toList, Array(scope)).map(s => (() => visitExpr(value)(s)): Val.Lazy))
      case ObjExtend(pos, value, ext) => {
        if(strict && isObjLiteral(value))
          Error.fail("Adjacent object literals not allowed in strict mode - Use '+' to concatenate objects", pos)
        val original = visitExpr(value).cast[Val.Obj]
        val extension = visitObjBody(pos, ext)
        extension.addSuper(pos, original)
      }
    }
  } catch Error.tryCatch(expr.pos)

  private def isObjLiteral(expr: Expr): Boolean = expr match {
    case _: Obj => true
    case _: ObjExtend => true
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
      pos.fileScope.currentFileLastPathElement,
      pos
    )
    catch Error.tryCatchWrap(pos)
  }

  def visitAssert(pos: Position, value: Expr, msg: Option[Expr], returned: Expr)
                 (implicit scope: ValScope): Val = {
    if (!visitExpr(value).isInstanceOf[Val.True]) {
      msg match {
        case None => Error.fail("Assertion failed", pos)
        case Some(msg) =>
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
      case Val.Arr(_, a) =>

        val range =
          start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt) until
            end.fold(a.length)(visitExpr(_).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
        Val.Arr(pos, range.dropWhile(_ < 0).takeWhile(_ < a.length).map(a).toArray)
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
        if (i.value > v.value.length) Error.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", pos)
        val int = i.value.toInt
        if (int != i.value) Error.fail("array index was not integer: " + i.value, pos)
        try v.value(int).force
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
    val (p, str) = resolveImport(value, pos)
    loadedFileContents(p) = str
    cachedImports.getOrElseUpdate(
      p,
      {
        val (doc, newFileScope) = parseCache.getOrElseUpdate(
          str,
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
    op match {
      // && and || are handled specially because unlike the other operators,
      // these short-circuit during evaluation in some cases when the LHS is known.
      case Expr.BinaryOp.`&&` | Expr.BinaryOp.`||` =>
        (visitExpr(lhs), op) match {
          case (lhs, Expr.BinaryOp.`&&`) =>
            lhs match{
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
          case (lhs, Expr.BinaryOp.`||`) =>
            lhs match{
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
          case _ => visitExpr(rhs)

        }
      case _ =>
        (visitExpr(lhs), op, visitExpr(rhs)) match {
          case (Val.Num(_, l), Expr.BinaryOp.`*`, Val.Num(_, r)) => Val.Num(pos, l * r)
          case (Val.Num(_, l), Expr.BinaryOp.`/`, Val.Num(_, r)) =>
            if (r == 0) Error.fail("division by zero", pos)
            Val.Num(pos, l / r)
          case (Val.Num(_, l), Expr.BinaryOp.`%`, Val.Num(_, r)) => Val.Num(pos, l % r)
          case (Val.Num(_, l), Expr.BinaryOp.`+`, Val.Num(_, r)) => Val.Num(pos, l + r)
          case (Val.Str(_, l), Expr.BinaryOp.`%`, r) =>
            try Val.Str(pos, Format.format(l, r, pos))
            catch Error.tryCatchWrap(pos)

          case (Val.Str(_, l), Expr.BinaryOp.`+`, Val.Str(_, r)) => Val.Str(pos, l + r)
          case (Val.Str(_, l), Expr.BinaryOp.`<`, Val.Str(_, r)) => Val.bool(pos, l < r)
          case (Val.Str(_, l), Expr.BinaryOp.`>`, Val.Str(_, r)) => Val.bool(pos, l > r)
          case (Val.Str(_, l), Expr.BinaryOp.`<=`, Val.Str(_, r)) => Val.bool(pos, l <= r)
          case (Val.Str(_, l), Expr.BinaryOp.`>=`, Val.Str(_, r)) => Val.bool(pos, l >= r)
          case (Val.Str(_, l), Expr.BinaryOp.`+`, r) =>
            try Val.Str(pos, l + Materializer.stringify(r))
            catch Error.tryCatchWrap(pos)
          case (l, Expr.BinaryOp.`+`, Val.Str(_, r)) =>
            try Val.Str(pos, Materializer.stringify(l) + r)
            catch Error.tryCatchWrap(pos)
          case (Val.Num(_, l), Expr.BinaryOp.`-`, Val.Num(_, r)) => Val.Num(pos, l - r)
          case (Val.Num(_, l), Expr.BinaryOp.`<<`, Val.Num(_, r)) => Val.Num(pos, l.toLong << r.toLong)
          case (Val.Num(_, l), Expr.BinaryOp.`>>`, Val.Num(_, r)) => Val.Num(pos, l.toLong >> r.toLong)
          case (Val.Num(_, l), Expr.BinaryOp.`<`, Val.Num(_, r)) => Val.bool(pos, l < r)
          case (Val.Num(_, l), Expr.BinaryOp.`>`, Val.Num(_, r)) => Val.bool(pos, l > r)
          case (Val.Num(_, l), Expr.BinaryOp.`<=`, Val.Num(_, r)) => Val.bool(pos, l <= r)
          case (Val.Num(_, l), Expr.BinaryOp.`>=`, Val.Num(_, r)) => Val.bool(pos, l >= r)
          case (l, Expr.BinaryOp.`==`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Error.fail("cannot test equality of functions", pos)
            }
            try Val.bool(pos, Materializer(l) == Materializer(r))
            catch Error.tryCatchWrap(pos)
          case (l, Expr.BinaryOp.`!=`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Error.fail("cannot test equality of functions", pos)
            }
            try Val.bool(pos, Materializer(l) != Materializer(r))
            catch Error.tryCatchWrap(pos)
          case (Val.Str(_, l), Expr.BinaryOp.`in`, o: Val.Obj) => Val.bool(pos, o.visibleKeys.containsKey(l))
          case (Val.Num(_, l), Expr.BinaryOp.`&`, Val.Num(_, r)) => Val.Num(pos, l.toLong & r.toLong)
          case (Val.Num(_, l), Expr.BinaryOp.`^`, Val.Num(_, r)) => Val.Num(pos, l.toLong ^ r.toLong)
          case (Val.Num(_, l), Expr.BinaryOp.`|`, Val.Num(_, r)) => Val.Num(pos, l.toLong | r.toLong)
          case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => r.addSuper(pos, l)
          case (Val.Arr(_, l), Expr.BinaryOp.`+`, Val.Arr(_, r)) => Val.Arr(pos,  l ++ r)
          case (l, op, r) =>
            Error.fail(s"Unknown binary operation: ${l.prettyName} $op ${r.prettyName}", pos)
        }
    }
  }

  def visitFieldName(fieldName: FieldName, pos: Position)(implicit scope: ValScope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k) match{
        case Val.Str(_, k1) => Some(k1)
        case Val.Null(_) => None
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
      (s, _, _, fs, _) => visitExpr(rhs)(s),
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

  def visitObjBody(pos: Position, b: ObjBody)(implicit scope: ValScope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      val binds = value.collect{case Member.BindStmt(b) => b}
      var asserting: Boolean = false
      def assertions(self: Val.Obj) = if (!asserting) {
        asserting = true
        val newScope: ValScope = makeNewScope(self, self.getSuper)

        value.collect {
          case Member.AssertStmt(value, msg) =>

            if (!visitExpr(value)(newScope).isInstanceOf[Val.True]) {
              msg match{
                case None => Error.fail("Assertion failed", value.pos)
                case Some(msg) =>
                  Error.fail(
                    "Assertion failed: " + visitExpr(msg)(newScope).cast[Val.Str].value,
                    value.pos
                  )
              }
            }
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
        visitBindings(binds, (self, sup) => makeNewScope(self, sup))

      lazy val newSelf: Val.Obj = {
        val builder = mutable.LinkedHashMap.newBuilder[String, Val.Obj.Member]
        value.foreach {
          case Member.Field(offset, fieldName, plus, null, sep, rhs) =>
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Val.Obj, _, _) => {
              assertions(self)
              visitExpr(rhs)(makeNewScope(self, sup))
            })).foreach(builder.+=)
          case Member.Field(offset, fieldName, false, argSpec, sep, rhs) =>
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Val.Obj, _, _) => {
              assertions(self)
              visitMethod(rhs, argSpec, offset)(makeNewScope(self, sup))
            })).foreach(builder.+=)
          case _: Member.BindStmt => // do nothing
          case _: Member.AssertStmt => // do nothing
        }

        new Val.Obj(pos, builder.result(), assertions, null)
      }
      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      val binds = (preLocals.iterator ++ postLocals).collect{ case Member.BindStmt(b) => b}.toArray
      lazy val compScope: ValScope = scope.extend(
        newSuper = null
      )

      lazy val newSelf: Val.Obj = {
        val builder = mutable.LinkedHashMap.newBuilder[String, Val.Obj.Member]
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
              builder += (k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Val.Obj, _, _) =>
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
        new Val.Obj(pos, builder.result(), null, null)
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
            case Val.Arr(_, value) => value
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
}

