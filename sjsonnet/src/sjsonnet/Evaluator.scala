package sjsonnet
import Expr._
import ammonite.ops.{Path, RelPath}
import fastparse.StringReprOps
import fastparse.core.Parsed
import fastparse.utils.IndexedParserInput
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

  def tryCatch[T](scope: Scope, wd: Path, offset: Int): PartialFunction[Throwable, Nothing] = {
      case e: Error => throw e
      case e: DelegateError =>
        throw new Error(e.msg, Nil, None)
          .addFrame(scope.currentFile, wd, offset)
      case e: Throwable =>
        throw new Error("Internal Error", Nil, Some(e))
          .addFrame(scope.currentFile, wd, offset)
  }
  def tryCatch2[T](path: Path, wd: Path, offset: Int): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(path, wd, offset)
    case e: DelegateError =>
      throw new Error(e.msg, Nil, None)
        .addFrame(path, wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(path, wd, offset)
  }
  def fail(msg: String, path: Path, offset: Int, wd: Path) = {
    throw new Error(msg, Nil, None).addFrame(path, wd, offset)
  }

}

class Evaluator(parser: Parser,
                originalScope: Scope,
                extVars: Map[String, ujson.Js],
                wd: Path) {
  val imports = collection.mutable.Map.empty[Path, Val]
  val importStrs = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr, scope: Scope): Val = try expr match{
    case Null(offset) => Val.Null
    case Parened(offset, inner) => visitExpr(inner, scope)
    case True(offset) => Val.True
    case False(offset) => Val.False
    case Self(offset) => scope.self


    case BinaryOp(_, lhs, Expr.BinaryOp.`in`, Super(offset)) =>
      val key = visitExpr(lhs, scope).asInstanceOf[Val.Str]
      Val.bool(scope.super0.get.value0.contains(key.value))

    case $(offset) => scope.dollar
    case Str(offset, value) => Val.Str(value)
    case Num(offset, value) => Val.Num(value)
    case Id(offset, value) =>
      val ref = scope.bindings(value)
        .getOrElse(Evaluator.fail("Unknown variable " + value, scope.currentFile, offset, wd))

      try ref.force catch Evaluator.tryCatch2(scope.currentFile, wd, offset)

    case Arr(offset, value) => Val.Arr(value.map(v => Lazy(visitExpr(v, scope))))
    case Obj(offset, value) => visitObjBody(value, scope)

    case UnaryOp(offset, op, value) => (op, visitExpr(value, scope)) match{
      case (Expr.UnaryOp.`-`, Val.Num(v)) => Val.Num(-v)
      case (Expr.UnaryOp.`+`, Val.Num(v)) => Val.Num(v)
      case (Expr.UnaryOp.`~`, Val.Num(v)) => Val.Num(~v.toLong)
      case (Expr.UnaryOp.`!`, Val.True) => Val.False
      case (Expr.UnaryOp.`!`, Val.False) => Val.True
    }

    case BinaryOp(offset, lhs, op, rhs) => {
      visitBinaryOp(scope, offset, lhs, op, rhs)
    }
    case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
      if (visitExpr(value, scope) != Val.True) {
        msg match{
          case None => Evaluator.fail("Assertion failed", scope.currentFile, offset, wd)
          case Some(msg) =>
            Evaluator.fail(
              "Assertion failed: " + visitExpr(msg, scope).asInstanceOf[Val.Str].value,
              scope.currentFile,
              offset,
              wd
            )
        }
      }
      visitExpr(returned, scope)

    case LocalExpr(offset, bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings, (self, sup) => newScope)
      visitExpr(returned, newScope)

    case Import(offset, value) => visitImport(scope, offset, value)
    case ImportStr(offset, value) => visitImportStr(scope, offset, value)
    case Expr.Error(offset, value) =>
      Evaluator.fail(
        visitExpr(value, scope) match{
          case Val.Str(s) => s
          case r =>
            try Materializer(r, extVars, wd).toString()
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
        },
        scope.currentFile,
        offset,
        wd
      )
    case Apply(offset, value, Args(args)) =>
      val lhs = visitExpr(value, scope)
      try lhs.asInstanceOf[Val.Func].apply(
        args.map{case (k, v) => (k, Lazy(visitExpr(v, scope)))},
        scope.currentFile.last,
        extVars,
        offset,
        wd
      )
      catch Evaluator.tryCatch2(scope.currentFile, wd, offset)

    case Select(offset, value, name) =>
      if (value.isInstanceOf[Super]){
        val ref = scope.super0.get.value(name, scope.currentFile, scope.currentRoot, offset, wd, extVars, self = scope.self)
        try ref.force catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
      }else visitExpr(value, scope) match{
        case obj: Val.Obj =>
          val ref = obj.value(name, scope.currentFile, scope.currentRoot, offset, wd, extVars)
          try ref.force
          catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
        case r =>
          Evaluator.fail(
            s"attemped to index a ${r.prettyName} with string ${name}",
            scope.currentFile,
            offset,
            wd
          )
      }

    case Lookup(offset, value, index) =>
      if (value.isInstanceOf[Super]){
        val key = visitExpr(index, scope).asInstanceOf[Val.Str]
        scope.super0.get.value(key.value, scope.currentFile, scope.currentRoot, offset, wd, extVars).force
      }else (visitExpr(value, scope), visitExpr(index, scope)) match {
        case (v: Val.Arr, i: Val.Num) =>
          if (i.value > v.value.length) Evaluator.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", scope.currentFile, offset, wd)
          val int = i.value.toInt
          if (int != i.value) Evaluator.fail("array index was not integer: " + i.value, scope.currentFile, offset, wd)
          try v.value(int).force
          catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
        case (v: Val.Str, i: Val.Num) => Val.Str(new String(Array(v.value(i.value.toInt))))
        case (v: Val.Obj, i: Val.Str) =>
          val ref = v.value(i.value, scope.currentFile, scope.currentRoot, offset, wd, extVars)
          try ref.force
          catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
        case (lhs, rhs) =>
          Evaluator.fail(s"attemped to index a ${lhs.prettyName} with ${rhs.prettyName}", scope.currentFile, offset, wd)
      }

    case Slice(offset, value, start, end, stride) =>
      visitExpr(value, scope) match{
        case Val.Arr(a) =>

          val range = start.fold(0)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) until end.fold(a.length)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) by stride.fold(1)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt)
          Val.Arr(range.dropWhile(_ < 0).takeWhile(_ < a.length).map(a))
        case Val.Str(s) =>
          val range = start.fold(0)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) until end.fold(s.length)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt) by stride.fold(1)(visitExpr(_, scope).asInstanceOf[Val.Num].value.toInt)
          Val.Str(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).mkString)
      }
    case Function(offset, params, body) => visitMethod(scope, body, params, offset)
    case IfElse(offset, cond, then, else0) =>
      visitExpr(cond, scope) match{
        case Val.True => visitExpr(then, scope)
        case Val.False => else0.fold[Val](Val.Null)(visitExpr(_, scope))
      }
    case Comp(offset, value, first, rest) =>
      Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Lazy(visitExpr(value, s))))
    case ObjExtend(offset, value, ext) => {
      val original = visitExpr(value, scope).asInstanceOf[Val.Obj]
      val extension = visitObjBody(ext, scope)
      Evaluator.mergeObjects(original, extension)
    }
  } catch Evaluator.tryCatch(scope, wd, expr.offset)

  def visitImportStr(scope: Scope, offset: Int, value: String) = {
    val p = resolveImport(scope, value)
    Val.Str(
      importStrs.getOrElseUpdate(
        p,
        importString(scope, offset, value, p)
      )
    )
  }

  def visitImport(scope: Scope, offset: Int, value: String) = {
    val p = resolveImport(scope, value)
    imports.getOrElseUpdate(
      p,
      visitExpr(
        parser.parse(importString(scope, offset, value, p)) match {
          case Parsed.Success(x, _) => x
          case f@Parsed.Failure(l, i, e) =>
            Evaluator.fail(
              "Imported file " + pprint.Util.literalize(value) +
              " had syntax error " + f.msg,
              scope.currentFile,
              offset,
              wd
            )
        },
        originalScope.copy(currentFile = p)
      )
    )
  }

  def resolveImport(scope: Scope, value: String) = {
    (scope.currentFile / ammonite.ops.up :: scope.searchRoots).map(_ / RelPath(value)).find(ammonite.ops.exists).get
  }
  def importString(scope: Scope, offset: Int, value: String, p: Path) = {
    try ammonite.ops.read(p)
    catch {
      case e: Throwable =>
        Evaluator.fail("Couldn't import file: " + pprint.Util.literalize(value), scope.currentFile, offset, wd)
    }
  }

  def visitBinaryOp(scope: Scope, offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr) = {
    op match {
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

            try Val.Str(Format.format(l, r, scope.currentFile, scope.currentRoot, offset, extVars, wd))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (Val.Str(l), Expr.BinaryOp.`+`, Val.Str(r)) => Val.Str(l + r)
          case (Val.Str(l), Expr.BinaryOp.`<`, Val.Str(r)) => Val.bool(l < r)
          case (Val.Str(l), Expr.BinaryOp.`>`, Val.Str(r)) => Val.bool(l > r)
          case (Val.Str(l), Expr.BinaryOp.`<=`, Val.Str(r)) => Val.bool(l <= r)
          case (Val.Str(l), Expr.BinaryOp.`>=`, Val.Str(r)) => Val.bool(l >= r)
          case (Val.Str(l), Expr.BinaryOp.`+`, r) =>
            try Val.Str(l + Materializer.apply(r, extVars, wd).transform(new Renderer()).toString)
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (l, Expr.BinaryOp.`+`, Val.Str(r)) =>
            try Val.Str(Materializer.apply(l, extVars, wd).transform(new Renderer()).toString + r)
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
            try Val.bool(Materializer(l, extVars, wd) == Materializer(r, extVars, wd))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (l, Expr.BinaryOp.`!=`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Evaluator.fail("cannot test equality of functions", scope.currentFile, offset, wd)
            }
            try Val.bool(Materializer(l, extVars, wd) != Materializer(r, extVars, wd))
            catch Evaluator.tryCatch2(scope.currentFile, wd, offset)
          case (Val.Str(l), Expr.BinaryOp.`in`, Val.Obj(r, _, _)) => Val.bool(r.contains(l))
          case (Val.Num(l), Expr.BinaryOp.`&`, Val.Num(r)) => Val.Num(l.toLong & r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`^`, Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`|`, Val.Num(r)) => Val.Num(l.toLong | r.toLong)
          case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => Evaluator.mergeObjects(l, r)
          case (Val.Arr(l), Expr.BinaryOp.`+`, Val.Arr(r)) => Val.Arr(l ++ r)
        }
    }
  }

  def visitFieldName(fieldName: FieldName, scope: Scope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k, scope) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
      }
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, params: Params, outerOffset: Int) = {

    Val.Func(
      scope,
      params,
      (scope, thisFile, extVars, outerOffset, wd) => visitExpr(rhs, scope),
      (default, scope) => visitExpr(default, scope)
    )
  }
  def visitBindings(bindings: Seq[Bind], scope: (Val.Obj, Option[Val.Obj]) => Scope) = {

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
                    "Assertion failed: " + visitExpr(msg, newScope).asInstanceOf[Val.Str].value,
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
        else newBindings.map { case (k, v) => (k, v.apply(self, sup)) }.toMap,
        scope.currentFile,
        scope.currentRoot,
        scope.searchRoots,
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
        value.collect{case Member.BindStmt(b) => b},
        (self, sup) => makeNewScope(self, sup)
      )

      lazy val newSelf: Val.Obj = Val.Obj(
        value.flatMap {
          case Member.Field(offset, fieldName, plus, None, sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Option[Val.Obj], thisFile: String) => {
              Lazy {
                assertions(self)
                visitExpr(rhs, makeNewScope(self, sup))
              }
            }))
          case Member.Field(offset, fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Option[Val.Obj], thisFile: String) => {
              Lazy {
                assertions(self)
                visitMethod(makeNewScope(self, sup), rhs, argSpec, offset)
              }
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
        scope.searchRoots,
        Some(scope),
      )

      lazy val newSelf: Val.Obj = Val.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .flatMap { s =>

            lazy val newScope: Scope = new Scope(
              scope.dollar0.orElse(Some(newSelf)),
              Some(newSelf),
              None,
              newBindings.map{case (k, v) => (k, v.apply(scope.self0.getOrElse(null), None))}.toMap,
              scope.currentFile,
              scope.currentRoot,
              scope.searchRoots,
              Some(s),
            )

            lazy val newBindings = visitBindings(
              (preLocals ++ postLocals).collect{ case Member.BindStmt(b) => b},
              (self, sup) => newScope
            )

            visitExpr(key, s) match {
              case Val.Str(k) =>
                Some(k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], thisFile: String) =>
                  Lazy(visitExpr(
                    value,
                    s.copy(self0 = Some(self), dollar0 = Some(s.dollar0.getOrElse(self))) ++
                      newBindings
                  ))
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

