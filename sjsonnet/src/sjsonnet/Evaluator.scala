package sjsonnet

import Expr.{Error => _, _}
import fastparse.Parsed
import sjsonnet.Expr.Member.Visibility
object Evaluator {
  def mergeObjects(lhs: Val.Obj, rhs: Val.Obj) = {
    def rec(current: Val.Obj): Val.Obj = {
      current.`super` match{
        case None => Val.Obj(current.value0, _ => (), Some(lhs))
        case Some(x) => Val.Obj(current.value0, _ => (), Some(rec(x)))
      }
    }
    rec(rhs)
  }

  def tryCatch[T](scope: Scope,
                  wd: Path,
                  offset: Int): PartialFunction[Throwable, Nothing] = {
      case e: Error => throw e
      case e: DelegateError =>
        throw new Error(e.msg, Nil, None)
          .addFrame(scope.currentFile, wd, offset)
      case e: Throwable =>
        throw new Error("Internal Error", Nil, Some(e))
          .addFrame(scope.currentFile, wd, offset)
  }
  def tryCatch2[T](path: Path,
                   wd: Path,
                   offset: Int): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(path, wd, offset)
    case e: DelegateError =>
      throw new Error(e.msg, Nil, None)
        .addFrame(path, wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(path, wd, offset)
  }
  def fail(msg: String,
           path: Path,
           offset: Int,
           wd: Path) = {
    throw Error(msg, Nil, None).addFrame(path, wd, offset)
  }
}

class Evaluator(parseCache: collection.mutable.Map[String, fastparse.Parsed[Expr]],
                originalScope: Scope,
                extVars: Map[String, ujson.Value],
                wd: Path,
                importer: (Path, String) => Option[(Path, String)]) extends EvaluatorApi(extVars, wd){

  val imports = collection.mutable.Map.empty[Path, Val]
  val importStrs = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr, scope: Scope): Val = try expr match{
    case Null(offset) => Val.Null
    case Parened(offset, inner) => visitExpr(inner, scope)
    case True(offset) => Val.True
    case False(offset) => Val.False
    case Self(offset) => scope.self


    case BinaryOp(_, lhs, Expr.BinaryOp.`in`, Super(offset)) =>
      val key = visitExpr(lhs, scope).cast[Val.Str]
      Val.bool(scope.super0.get.value0.contains(key.value))

    case $(offset) => scope.dollar
    case Str(offset, value) => Val.Str(value)
    case Num(offset, value) => Val.Num(value)
    case Id(offset, value) => visitId(scope, offset, value)

    case Arr(offset, value) => Val.Arr(value.map(v => Lazy(visitExpr(v, scope))))
    case Obj(offset, value) => visitObjBody(value, scope)

    case UnaryOp(offset, op, value) => visitUnaryOp(scope, op, value)

    case BinaryOp(offset, lhs, op, rhs) => {
      visitBinaryOp(scope, offset, lhs, op, rhs)
    }
    case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
      visitAssert(scope, offset, value, msg, returned)

    case LocalExpr(offset, bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings.iterator, (self, sup) => newScope)
      visitExpr(returned, newScope)

    case Import(offset, value) => visitImport(scope, offset, value)
    case ImportStr(offset, value) => visitImportStr(scope, offset, value)
    case Expr.Error(offset, value) => visitError(scope, offset, value)
    case Apply(offset, value, Args(args)) => visitApply(scope, offset, value, args)

    case Select(offset, value, name) => visitSelect(scope, offset, value, name)

    case Lookup(offset, value, index) => visitLookup(scope, offset, value, index)

    case Slice(offset, value, start, end, stride) => visitSlice(scope, offset, value, start, end, stride)
    case Function(offset, params, body) => visitMethod(scope, body, params, offset)
    case IfElse(offset, cond, then, else0) => visitIfElse(scope, offset, cond, then, else0)
    case Comp(offset, value, first, rest) =>
      Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Lazy(visitExpr(value, s))))
    case ObjExtend(offset, value, ext) => {
      val original = visitExpr(value, scope).cast[Val.Obj]
      val extension = visitObjBody(ext, scope)
      Evaluator.mergeObjects(original, extension)
    }
  } catch Evaluator.tryCatch(scope, wd, expr.offset)

  def visitId(scope: Scope, offset: Int, value: String): Val = {
    val ref = scope.bindings(value)
      .getOrElse(Evaluator.fail("Unknown variable " + value, scope.currentFile, offset, wd))

    try ref.force catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
  }

  def visitIfElse(scope: Scope, offset: Int, cond: Expr, then: Expr, else0: Option[Expr]): Val = {
    visitExpr(cond, scope) match {
      case Val.True => visitExpr(then, scope)
      case Val.False => else0.fold[Val](Val.Null)(visitExpr(_, scope))
      case v => Evaluator.fail("Need boolean, found " + v.prettyName, scope.currentFile, offset, wd)
    }
  }

  def visitError(scope: Scope, offset: Int, value: Expr): Nothing = {
    Evaluator.fail(
      visitExpr(value, scope) match {
        case Val.Str(s) => s
        case r =>
          try Materializer(r, this).toString()
          catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
      },
      scope.currentFile,
      offset,
      wd
    )
  }

  def visitUnaryOp(scope: Scope, op: UnaryOp.Op, value: Expr): Val = {
    (op, visitExpr(value, scope)) match {
      case (Expr.UnaryOp.`-`, Val.Num(v)) => Val.Num(-v)
      case (Expr.UnaryOp.`+`, Val.Num(v)) => Val.Num(v)
      case (Expr.UnaryOp.`~`, Val.Num(v)) => Val.Num(~v.toLong)
      case (Expr.UnaryOp.`!`, Val.True) => Val.False
      case (Expr.UnaryOp.`!`, Val.False) => Val.True
    }
  }

  private def visitApply(scope: Scope, offset: Int, value: Expr, args: Seq[(Option[String], Expr)]) = {
    val lhs = visitExpr(value, scope)
    try lhs.cast[Val.Func].apply(
      args.map { case (k, v) => (k, Lazy(visitExpr(v, scope))) },
      scope.currentFile.last,
      this,
      offset,
      scope.currentFile
    )
    catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
  }

  def visitAssert(scope: Scope, offset: Int, value: Expr, msg: Option[Expr], returned: Expr): Val = {
    if (visitExpr(value, scope) != Val.True) {
      msg match {
        case None => Evaluator.fail("Assertion failed", scope.currentFile, offset, wd)
        case Some(msg) =>
          Evaluator.fail(
            "Assertion failed: " + visitExpr(msg, scope).cast[Val.Str].value,
            scope.currentFile,
            offset,
            wd
          )
      }
    }
    visitExpr(returned, scope)
  }

  private def visitSlice(scope: Scope, offset: Int, value: Expr, start: Option[Expr], end: Option[Expr], stride: Option[Expr]) = {
    visitExpr(value, scope) match {
      case Val.Arr(a) =>

        val range =
          start.fold(0)(visitExpr(_, scope).cast[Val.Num].value.toInt) until
            end.fold(a.length)(visitExpr(_, scope).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_, scope).cast[Val.Num].value.toInt)
        Val.Arr(range.dropWhile(_ < 0).takeWhile(_ < a.length).map(a))
      case Val.Str(s) =>
        val range =
          start.fold(0)(visitExpr(_, scope).cast[Val.Num].value.toInt) until
            end.fold(s.length)(visitExpr(_, scope).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_, scope).cast[Val.Num].value.toInt)
        Val.Str(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).mkString)
      case x => Evaluator.fail("Can only slice array or string, not " + x.prettyName, scope.currentFile, offset, wd)
    }
  }

  def visitLookup(scope: Scope, offset: Int, value: Expr, index: Expr): Val = {
    if (value.isInstanceOf[Super]) {
      val key = visitExpr(index, scope).cast[Val.Str]
      scope.super0.get.value(key.value, scope, offset, this)
    } else (visitExpr(value, scope), visitExpr(index, scope)) match {
      case (v: Val.Arr, i: Val.Num) =>
        if (i.value > v.value.length) Evaluator.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", scope.currentFile, offset, wd)
        val int = i.value.toInt
        if (int != i.value) Evaluator.fail("array index was not integer: " + i.value, scope.currentFile, offset, wd)
        try v.value(int).force
        catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
      case (v: Val.Str, i: Val.Num) => Val.Str(new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        val ref = v.value(i.value, scope, offset, this)
        try ref
        catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
      case (lhs, rhs) =>
        Evaluator.fail(s"attemped to index a ${lhs.prettyName} with ${rhs.prettyName}", scope.currentFile, offset, wd)
    }
  }

  def visitSelect(scope: Scope, offset: Int, value: Expr, name: String): Val = {
    if (value.isInstanceOf[Super]) {
      val ref = scope.super0.get.value(name, scope, offset, this, scope.self)
      try ref catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
    } else visitExpr(value, scope) match {
      case obj: Val.Obj =>
        val ref = obj.value(name, scope, offset, this)
        try ref
        catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
      case r =>
        Evaluator.fail(
          s"attemped to index a ${r.prettyName} with string ${name}",
          scope.currentFile,
          offset,
          wd
        )
    }
  }

  def visitImportStr(scope: Scope, offset: Int, value: String) = {
    val (p, str) = resolveImport(scope, value, offset)
    Val.Str(importStrs.getOrElseUpdate(p, str))
  }

  def visitImport(scope: Scope, offset: Int, value: String) = {
    val (p, str) = resolveImport(scope, value, offset)
    imports.getOrElseUpdate(
      p,
      visitExpr(
        parseCache.getOrElseUpdate(
          str,
          fastparse.parse(str, Parser.document(_))
        ) match {
          case Parsed.Success(x, _) => x
          case f @ Parsed.Failure(l, i, e) =>
            Evaluator.fail(
              "Imported file " + pprint.Util.literalize(value) +
              " had Parse error. " + f.trace().msg,
              scope.currentFile,
              offset,
              wd
            )
        },
        originalScope.copy(currentFile = p)
      )
    )
  }

  def resolveImport(scope: Scope, value: String, offset: Int): (Path, String) = {
    importer(scope.currentFile.parent(), value)
      .getOrElse(
        Evaluator.fail("Couldn't import file: " + pprint.Util.literalize(value), scope.currentFile, offset, wd)
      )
  }

  def visitBinaryOp(scope: Scope, offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr) = {
    op match {
      // && and || are handled specially because unlike the other operators,
      // these short-circuit during evaluation in some cases when the LHS is known.
      case Expr.BinaryOp.`&&` | Expr.BinaryOp.`||` =>
        (visitExpr(lhs, scope), op) match {
          case (Val.False, Expr.BinaryOp.`&&`) => Val.False
          case (Val.True, Expr.BinaryOp.`||`) => Val.True
          case _ => visitExpr(rhs, scope)

        }
      case _ =>
        (visitExpr(lhs, scope), op, visitExpr(rhs, scope)) match {
          case (Val.Num(l), Expr.BinaryOp.`*`, Val.Num(r)) => Val.Num(l * r)
          case (Val.Num(l), Expr.BinaryOp.`/`, Val.Num(r)) =>
            if (r == 0) Evaluator.fail("division by zero", scope.currentFile, offset, wd)
            Val.Num(l / r)
          case (Val.Num(l), Expr.BinaryOp.`%`, Val.Num(r)) => Val.Num(l % r)
          case (Val.Num(l), Expr.BinaryOp.`+`, Val.Num(r)) => Val.Num(l + r)
          case (Val.Str(l), Expr.BinaryOp.`%`, r) =>

            try Val.Str(Format.format(l, r, scope, offset, this))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (Val.Str(l), Expr.BinaryOp.`+`, Val.Str(r)) => Val.Str(l + r)
          case (Val.Str(l), Expr.BinaryOp.`<`, Val.Str(r)) => Val.bool(l < r)
          case (Val.Str(l), Expr.BinaryOp.`>`, Val.Str(r)) => Val.bool(l > r)
          case (Val.Str(l), Expr.BinaryOp.`<=`, Val.Str(r)) => Val.bool(l <= r)
          case (Val.Str(l), Expr.BinaryOp.`>=`, Val.Str(r)) => Val.bool(l >= r)
          case (Val.Str(l), Expr.BinaryOp.`+`, r) =>
            try Val.Str(l + Materializer.apply(r, this).transform(new Renderer()).toString)
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (l, Expr.BinaryOp.`+`, Val.Str(r)) =>
            try Val.Str(Materializer.apply(l, this).transform(new Renderer()).toString + r)
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (Val.Num(l), Expr.BinaryOp.`-`, Val.Num(r)) => Val.Num(l - r)
          case (Val.Num(l), Expr.BinaryOp.`<<`, Val.Num(r)) => Val.Num(l.toLong << r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`>>`, Val.Num(r)) => Val.Num(l.toLong >> r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`<`, Val.Num(r)) => Val.bool(l < r)
          case (Val.Num(l), Expr.BinaryOp.`>`, Val.Num(r)) => Val.bool(l > r)
          case (Val.Num(l), Expr.BinaryOp.`<=`, Val.Num(r)) => Val.bool(l <= r)
          case (Val.Num(l), Expr.BinaryOp.`>=`, Val.Num(r)) => Val.bool(l >= r)
          case (l, Expr.BinaryOp.`==`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Evaluator.fail("cannot test equality of functions", scope.currentFile, offset, wd)
            }
            try Val.bool(Materializer(l, this) == Materializer(r, this))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (l, Expr.BinaryOp.`!=`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Evaluator.fail("cannot test equality of functions", scope.currentFile, offset, wd)
            }
            try Val.bool(Materializer(l, this) != Materializer(r, this))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (Val.Str(l), Expr.BinaryOp.`in`, Val.Obj(r, _, _)) => Val.bool(r.contains(l))
          case (Val.Num(l), Expr.BinaryOp.`&`, Val.Num(r)) => Val.Num(l.toLong & r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`^`, Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`|`, Val.Num(r)) => Val.Num(l.toLong | r.toLong)
          case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => Evaluator.mergeObjects(l, r)
          case (Val.Arr(l), Expr.BinaryOp.`+`, Val.Arr(r)) => Val.Arr(l ++ r)
          case (l, op, r) =>
            Evaluator.fail(s"Unknown binary operation: ${l.prettyName} $op ${r.prettyName}", scope.currentFile, offset, wd)
        }
    }
  }

  def visitFieldName(fieldName: FieldName, scope: Scope, offset: Int) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k, scope) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
        case x => Evaluator.fail(
          s"Field name must be string or null, not ${x.prettyName}",
          scope.currentFile,
          offset,
          wd
        )
      }
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, params: Params, outerOffset: Int) = {

    Val.Func(
      Some(scope),
      params,
      (scope, thisFile, evaluator, outerOffset) => visitExpr(rhs, scope),
      (default, scope) => visitExpr(default, scope)
    )
  }
  def visitBindings(bindings: Iterator[Bind], scope: (Val.Obj, Option[Val.Obj]) => Scope) = {

    bindings.collect{
      case Bind(offset, fieldName, None, rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(visitExpr(rhs, scope(self, sup))))
      case Bind(offset, fieldName, Some(argSpec), rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(visitMethod(scope(self, sup), rhs, argSpec, offset)))
    }
  }
  def visitObjBody(b: ObjBody, scope: Scope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      var asserting: Boolean = false
      def assertions(self: Val.Obj) = if (!asserting) {
        asserting = true
        val newScope: Scope = makeNewScope(self, self.`super`)

        value.collect {
          case Member.AssertStmt(value, msg) =>

            if (visitExpr(value, newScope) != Val.True) {
              msg match{
                case None => Evaluator.fail("Assertion failed", scope.currentFile, value.offset, wd)
                case Some(msg) =>
                  Evaluator.fail(
                    "Assertion failed: " + visitExpr(msg, newScope).cast[Val.Str].value,
                    scope.currentFile,
                    value.offset,
                    wd
                  )
              }
            }
        }
      }

      def makeNewScope0(self: Val.Obj, sup: Option[Val.Obj]): Scope = new Scope(
        scope.dollar0.orElse(Some(self)),
        Some(self),
        sup,
        if (newBindings.isEmpty) Map.empty
        else newBindings.iterator.map { case (k, v) => (k, v.apply(self, sup)) }.toMap,
        scope.currentFile,
        scope.currentRoot,
        Some(scope)
      )

      lazy val defaultScope = makeNewScope0(newSelf, newSelf.`super`)
      def makeNewScope(self: Val.Obj, sup: Option[Val.Obj]): Scope =
        if ((self eq newSelf) &&
            (sup.isDefined && newSelf.`super`.isDefined && (sup.get eq newSelf.`super`.get)) &&
            (sup.isEmpty && newSelf.`super`.isEmpty)) {
          // Fast path: in the common case where the `self` and `super` are
          // unchanged by inheritence or other trickery, we can share the
          // new scope between all members and sub-scopes.
          defaultScope
        }else makeNewScope0(self, sup)

      lazy val newBindings = visitBindings(
        value.iterator.collect{case Member.BindStmt(b) => b},
        (self, sup) => makeNewScope(self, sup)
      ).toArray

      lazy val newSelf: Val.Obj = Val.Obj(
        value.flatMap {
          case Member.Field(offset, fieldName, plus, None, sep, rhs) =>
            visitFieldName(fieldName, scope, offset).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Option[Val.Obj], _) => {
              assertions(self)
              visitExpr(rhs, makeNewScope(self, sup))
            }))
          case Member.Field(offset, fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, scope, offset).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Option[Val.Obj], _) => {
              assertions(self)
              visitMethod(makeNewScope(self, sup), rhs, argSpec, offset)
            }))

          case _: Member.BindStmt => None
          case _: Member.AssertStmt => None
        }.toMap,
        self => assertions(self),
        None
      )
      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      lazy val compScope: Scope = new Scope(
        scope.dollar0,
        scope.self0,
        None,
        Map(),
        scope.currentFile,
        scope.currentRoot,
        Some(scope),
      )

      lazy val newSelf: Val.Obj = Val.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .flatMap { s =>

            lazy val newScope: Scope = new Scope(
              scope.dollar0.orElse(Some(newSelf)),
              Some(newSelf),
              None,
              newBindings.map{case (k, v) => (k, v.apply(scope.self0.orNull, None))}.toMap,
              scope.currentFile,
              scope.currentRoot,
              Some(s),
            )

            lazy val newBindings = visitBindings(
              (preLocals.iterator ++ postLocals).collect{ case Member.BindStmt(b) => b},
              (self, sup) => newScope
            ).toArray

            visitExpr(key, s) match {
              case Val.Str(k) =>
                Some(k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _) =>
                  visitExpr(
                    value,
                    s.copy(self0 = Some(self), dollar0 = Some(s.dollar0.getOrElse(self))) ++
                      newBindings
                  )
                ))
              case Val.Null => None
            }

          }
          .toMap,
        _ => (),
        None
      )

      newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Seq[Scope]): Seq[Scope] = f match{
    case ForSpec(offset, name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr, s) match{
            case Val.Arr(value) => value
            case r => Evaluator.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              s.currentFile,
              expr.offset,
              wd
            )
          }
        } yield s ++ Seq(name -> ((self: Val.Obj, sup: Option[Val.Obj]) => e))
      )
    case IfSpec(offset, expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr, _) == Val.True))
    case Nil => scopes
  }
}

