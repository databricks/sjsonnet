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

  def tryCatch[T](offset: Int)
                 (implicit fileScope: FileScope, evaluator: EvalScope): PartialFunction[Throwable, Nothing] = {
      case e: Error => throw e
      case e: DelegateError =>
        throw new Error(e.msg, Nil, None)
          .addFrame(fileScope.currentFile, evaluator.wd, offset)
      case e: Throwable =>
        throw new Error("Internal Error", Nil, Some(e))
          .addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
  def tryCatch2[T](offset: Int)
                  (implicit fileScope: FileScope, evaluator: EvalScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: DelegateError =>
      throw new Error(e.msg, Nil, None)
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
  def fail(msg: String, offset: Int)
          (implicit fileScope: FileScope, evaluator: EvalScope) = {
    throw Error(msg, Nil, None).addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
}

class Evaluator(parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]],
                extVars: Map[String, ujson.Value],
                wd: Path,
                importer: (Path, String) => Option[(Path, String)]) extends EvalScope(extVars, wd){
  implicit def evalScope: EvalScope = this

  val imports = collection.mutable.Map.empty[Path, Val]
  val importStrs = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr)
               (implicit scope: Scope, fileScope: FileScope): Val = try expr match{
    case Null(offset) => Val.Null
    case Parened(offset, inner) => visitExpr(inner)
    case True(offset) => Val.True
    case False(offset) => Val.False
    case Self(offset) => scope.self


    case BinaryOp(_, lhs, Expr.BinaryOp.`in`, Super(offset)) =>
      val key = visitExpr(lhs).cast[Val.Str]
      Val.bool(scope.super0.get.value0.contains(key.value))

    case $(offset) => scope.dollar
    case Str(offset, value) => Val.Str(value)
    case Num(offset, value) => Val.Num(value)
    case Id(offset, value) => visitId(offset, value)

    case Arr(offset, value) => Val.Arr(value.map(v => Lazy(visitExpr(v))))
    case Obj(offset, value) => visitObjBody(value)

    case UnaryOp(offset, op, value) => visitUnaryOp(op, value)

    case BinaryOp(offset, lhs, op, rhs) => {
      visitBinaryOp(offset, lhs, op, rhs)
    }
    case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
      visitAssert(offset, value, msg, returned)

    case LocalExpr(offset, bindings, returned) =>
      lazy val newScope: Scope = scope ++ visitBindings(bindings.iterator, (self, sup) => newScope)
      visitExpr(returned)(newScope, implicitly)

    case Import(offset, value) => visitImport(offset, value)
    case ImportStr(offset, value) => visitImportStr(offset, value)
    case Expr.Error(offset, value) => visitError(offset, value)
    case Apply(offset, value, Args(args)) => visitApply(offset, value, args)

    case Select(offset, value, name) => visitSelect(offset, value, name)

    case Lookup(offset, value, index) => visitLookup(offset, value, index)

    case Slice(offset, value, start, end, stride) => visitSlice(offset, value, start, end, stride)
    case Function(offset, params, body) => visitMethod(body, params, offset)
    case IfElse(offset, cond, then, else0) => visitIfElse(offset, cond, then, else0)
    case Comp(offset, value, first, rest) =>
      Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Lazy(visitExpr(value)(s, implicitly))))
    case ObjExtend(offset, value, ext) => {
      val original = visitExpr(value).cast[Val.Obj]
      val extension = visitObjBody(ext)
      Evaluator.mergeObjects(original, extension)
    }
  } catch Evaluator.tryCatch(expr.offset)

  def visitId(offset: Int, value: Int)(implicit scope: Scope, fileScope: FileScope): Val = {
    val ref = scope.bindings(value)
      .getOrElse(
        Evaluator.fail(
          "Unknown variable " + fileScope.indexNames(value),
          offset
        )
      )

    try ref.force catch Evaluator.tryCatch2(offset)
  }

  def visitIfElse(offset: Int,
                  cond: Expr,
                  then: Expr,
                  else0: Option[Expr])
                 (implicit scope: Scope,
                  fileScope: FileScope): Val = {
    visitExpr(cond) match {
      case Val.True => visitExpr(then)
      case Val.False => else0.fold[Val](Val.Null)(visitExpr(_))
      case v => Evaluator.fail("Need boolean, found " + v.prettyName, offset)
    }
  }

  def visitError(offset: Int, value: Expr)
                (implicit scope: Scope, fileScope: FileScope): Nothing = {
    Evaluator.fail(
      visitExpr(value) match {
        case Val.Str(s) => s
        case r =>
          try Materializer(r).toString()
          catch Evaluator.tryCatch2(offset)
      },
      offset
    )
  }

  def visitUnaryOp(op: UnaryOp.Op, value: Expr)
                  (implicit scope: Scope, fileScope: FileScope): Val = {
    (op, visitExpr(value)) match {
      case (Expr.UnaryOp.`-`, Val.Num(v)) => Val.Num(-v)
      case (Expr.UnaryOp.`+`, Val.Num(v)) => Val.Num(v)
      case (Expr.UnaryOp.`~`, Val.Num(v)) => Val.Num(~v.toLong)
      case (Expr.UnaryOp.`!`, Val.True) => Val.False
      case (Expr.UnaryOp.`!`, Val.False) => Val.True
    }
  }

  private def visitApply(offset: Int, value: Expr, args: Seq[(Option[String], Expr)])
                        (implicit scope: Scope, fileScope: FileScope) = {
    val lhs = visitExpr(value)
    try lhs.cast[Val.Func].apply(
      args.map { case (k, v) => (k, Lazy(visitExpr(v))) },
      fileScope.currentFile.last,
      offset
    )
    catch Evaluator.tryCatch2(offset)
  }

  def visitAssert(offset: Int, value: Expr, msg: Option[Expr], returned: Expr)
                 (implicit scope: Scope, fileScope: FileScope): Val = {
    if (visitExpr(value) != Val.True) {
      msg match {
        case None => Evaluator.fail("Assertion failed", offset)
        case Some(msg) =>
          Evaluator.fail(
            "Assertion failed: " + visitExpr(msg).cast[Val.Str].value,
            offset
          )
      }
    }
    visitExpr(returned)
  }

  private def visitSlice(offset: Int,
                         value: Expr,
                         start: Option[Expr],
                         end: Option[Expr],
                         stride: Option[Expr])
                        (implicit scope: Scope, fileScope: FileScope)= {
    visitExpr(value) match {
      case Val.Arr(a) =>

        val range =
          start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt) until
            end.fold(a.length)(visitExpr(_).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
        Val.Arr(range.dropWhile(_ < 0).takeWhile(_ < a.length).map(a))
      case Val.Str(s) =>
        val range =
          start.fold(0)(visitExpr(_).cast[Val.Num].value.toInt) until
            end.fold(s.length)(visitExpr(_).cast[Val.Num].value.toInt) by
            stride.fold(1)(visitExpr(_).cast[Val.Num].value.toInt)
        Val.Str(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).mkString)
      case x => Evaluator.fail("Can only slice array or string, not " + x.prettyName, offset)
    }
  }

  def visitLookup(offset: Int, value: Expr, index: Expr)
                 (implicit scope: Scope, fileScope: FileScope): Val = {
    if (value.isInstanceOf[Super]) {
      val key = visitExpr(index).cast[Val.Str]
      scope.super0.get.value(key.value, offset)
    } else (visitExpr(value), visitExpr(index)) match {
      case (v: Val.Arr, i: Val.Num) =>
        if (i.value > v.value.length) Evaluator.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", offset)
        val int = i.value.toInt
        if (int != i.value) Evaluator.fail("array index was not integer: " + i.value, offset)
        try v.value(int).force
        catch Evaluator.tryCatch2(offset)
      case (v: Val.Str, i: Val.Num) => Val.Str(new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        val ref = v.value(i.value, offset)
        try ref
        catch Evaluator.tryCatch2(offset)
      case (lhs, rhs) =>
        Evaluator.fail(s"attemped to index a ${lhs.prettyName} with ${rhs.prettyName}", offset)
    }
  }

  def visitSelect(offset: Int, value: Expr, name: String)
                 (implicit scope: Scope, fileScope: FileScope): Val = {
    if (value.isInstanceOf[Super]) {
      val ref = scope.super0.get.value(name, offset, scope.self)
      try ref catch Evaluator.tryCatch2(offset)
    } else visitExpr(value) match {
      case obj: Val.Obj =>
        val ref = obj.value(name, offset)
        try ref
        catch Evaluator.tryCatch2(offset)
      case r =>
        Evaluator.fail(
          s"attemped to index a ${r.prettyName} with string ${name}",
          offset
        )
    }
  }

  def visitImportStr(offset: Int, value: String)(implicit scope: Scope, fileScope: FileScope) = {
    val (p, str) = resolveImport(value, offset)
    Val.Str(importStrs.getOrElseUpdate(p, str))
  }

  def visitImport(offset: Int, value: String)(implicit scope: Scope, fileScope: FileScope) = {
    val (p, str) = resolveImport(value, offset)
    imports.getOrElseUpdate(
      p,
      {
        val (doc, nameIndices) = parseCache.getOrElseUpdate(
          str,
          fastparse.parse(str, Parser.document(_))
        ) match {
          case Parsed.Success((doc, nameIndices), _) => (doc, nameIndices)
          case f @ Parsed.Failure(l, i, e) =>
            Evaluator.fail(
              "Imported file " + pprint.Util.literalize(value) +
                " had Parse error. " + f.trace().msg,
              offset
            )
        }
        visitExpr(doc)(
          Scope.standard(nameIndices.size),
          new FileScope(p, fileScope.currentRoot, nameIndices)
        )
      }
    )
  }

  def resolveImport(value: String, offset: Int)
                   (implicit scope: Scope, fileScope: FileScope): (Path, String) = {
    importer(fileScope.currentFile.parent(), value)
      .getOrElse(
        Evaluator.fail(
          "Couldn't import file: " + pprint.Util.literalize(value),
          offset
        )
      )
  }

  def visitBinaryOp(offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr)
                   (implicit scope: Scope, fileScope: FileScope) = {
    op match {
      // && and || are handled specially because unlike the other operators,
      // these short-circuit during evaluation in some cases when the LHS is known.
      case Expr.BinaryOp.`&&` | Expr.BinaryOp.`||` =>
        (visitExpr(lhs), op) match {
          case (Val.False, Expr.BinaryOp.`&&`) => Val.False
          case (Val.True, Expr.BinaryOp.`||`) => Val.True
          case _ => visitExpr(rhs)

        }
      case _ =>
        (visitExpr(lhs), op, visitExpr(rhs)) match {
          case (Val.Num(l), Expr.BinaryOp.`*`, Val.Num(r)) => Val.Num(l * r)
          case (Val.Num(l), Expr.BinaryOp.`/`, Val.Num(r)) =>
            if (r == 0) Evaluator.fail("division by zero", offset)
            Val.Num(l / r)
          case (Val.Num(l), Expr.BinaryOp.`%`, Val.Num(r)) => Val.Num(l % r)
          case (Val.Num(l), Expr.BinaryOp.`+`, Val.Num(r)) => Val.Num(l + r)
          case (Val.Str(l), Expr.BinaryOp.`%`, r) =>

            try Val.Str(Format.format(l, r, offset))
            catch Evaluator.tryCatch2(offset)
          case (Val.Str(l), Expr.BinaryOp.`+`, Val.Str(r)) => Val.Str(l + r)
          case (Val.Str(l), Expr.BinaryOp.`<`, Val.Str(r)) => Val.bool(l < r)
          case (Val.Str(l), Expr.BinaryOp.`>`, Val.Str(r)) => Val.bool(l > r)
          case (Val.Str(l), Expr.BinaryOp.`<=`, Val.Str(r)) => Val.bool(l <= r)
          case (Val.Str(l), Expr.BinaryOp.`>=`, Val.Str(r)) => Val.bool(l >= r)
          case (Val.Str(l), Expr.BinaryOp.`+`, r) =>
            try Val.Str(l + Materializer.apply(r).transform(new Renderer()).toString)
            catch Evaluator.tryCatch2(offset)
          case (l, Expr.BinaryOp.`+`, Val.Str(r)) =>
            try Val.Str(Materializer.apply(l).transform(new Renderer()).toString + r)
            catch Evaluator.tryCatch2(offset)
          case (Val.Num(l), Expr.BinaryOp.`-`, Val.Num(r)) => Val.Num(l - r)
          case (Val.Num(l), Expr.BinaryOp.`<<`, Val.Num(r)) => Val.Num(l.toLong << r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`>>`, Val.Num(r)) => Val.Num(l.toLong >> r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`<`, Val.Num(r)) => Val.bool(l < r)
          case (Val.Num(l), Expr.BinaryOp.`>`, Val.Num(r)) => Val.bool(l > r)
          case (Val.Num(l), Expr.BinaryOp.`<=`, Val.Num(r)) => Val.bool(l <= r)
          case (Val.Num(l), Expr.BinaryOp.`>=`, Val.Num(r)) => Val.bool(l >= r)
          case (l, Expr.BinaryOp.`==`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Evaluator.fail("cannot test equality of functions", offset)
            }
            try Val.bool(Materializer(l) == Materializer(r))
            catch Evaluator.tryCatch2(offset)
          case (l, Expr.BinaryOp.`!=`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Evaluator.fail("cannot test equality of functions", offset)
            }
            try Val.bool(Materializer(l) != Materializer(r))
            catch Evaluator.tryCatch2(offset)
          case (Val.Str(l), Expr.BinaryOp.`in`, Val.Obj(r, _, _)) => Val.bool(r.contains(l))
          case (Val.Num(l), Expr.BinaryOp.`&`, Val.Num(r)) => Val.Num(l.toLong & r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`^`, Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`|`, Val.Num(r)) => Val.Num(l.toLong | r.toLong)
          case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => Evaluator.mergeObjects(l, r)
          case (Val.Arr(l), Expr.BinaryOp.`+`, Val.Arr(r)) => Val.Arr(l ++ r)
          case (l, op, r) =>
            Evaluator.fail(s"Unknown binary operation: ${l.prettyName} $op ${r.prettyName}", offset)
        }
    }
  }

  def visitFieldName(fieldName: FieldName, offset: Int)
                    (implicit scope: Scope, fileScope: FileScope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
        case x => Evaluator.fail(
          s"Field name must be string or null, not ${x.prettyName}",
          offset
        )
      }
    }
  }

  def visitMethod(rhs: Expr, params: Params, outerOffset: Int)
                 (implicit scope: Scope, fileScope: FileScope) = {
    Val.Func(
      Some(scope -> fileScope),
      params,
      (s, _, _, fs, _) => visitExpr(rhs)(s, fs),
      (default, s) => visitExpr(default)(s, fileScope)
    )
  }

  def visitBindings(bindings: Iterator[Bind], scope: (Val.Obj, Option[Val.Obj]) => Scope)
                   (implicit fileScope: FileScope)= {
    bindings.map{ b: Bind =>
      b.args match{
        case None =>
          (
            b.name,
            (self: Val.Obj, sup: Option[Val.Obj]) =>
              Lazy(visitExpr(b.rhs)(scope(self, sup), implicitly))
          )
        case Some(argSpec) =>
          (
            b.name,
            (self: Val.Obj, sup: Option[Val.Obj]) =>
              Lazy(visitMethod(b.rhs, argSpec, b.offset)(scope(self, sup), implicitly))

          )
      }
    }
  }

  def visitObjBody(b: ObjBody)(implicit scope: Scope, fileScope: FileScope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      var asserting: Boolean = false
      def assertions(self: Val.Obj) = if (!asserting) {
        asserting = true
        val newScope: Scope = makeNewScope(self, self.`super`)

        value.collect {
          case Member.AssertStmt(value, msg) =>

            if (visitExpr(value) != Val.True) {
              msg match{
                case None => Evaluator.fail("Assertion failed", value.offset)
                case Some(msg) =>
                  Evaluator.fail(
                    "Assertion failed: " + visitExpr(msg)(newScope, implicitly).cast[Val.Str].value,
                    value.offset
                  )
              }
            }
        }
      }

      def makeNewScope0(self: Val.Obj, sup: Option[Val.Obj]): Scope = new Scope(
        scope.dollar0.orElse(Some(self)),
        Some(self),
        sup,
        if (newBindings.isEmpty) Array.empty
        else {
          val arr = new Array[Lazy](scope.bindings0.length)
          for((i, v) <- newBindings) arr(i) = v.apply(self, sup)
          arr
        },
        Some(scope),
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
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Option[Val.Obj], _) => {
              assertions(self)
              visitExpr(rhs)(makeNewScope(self, sup), implicitly)
            }))
          case Member.Field(offset, fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Option[Val.Obj], _) => {
              assertions(self)
              visitMethod(rhs, argSpec, offset)(makeNewScope(self, sup), implicitly)
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
        new Array[Lazy](scope.bindings0.length),
        Some(scope),
      )

      lazy val newSelf: Val.Obj = Val.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .flatMap { s =>

            lazy val newScope: Scope = new Scope(
              scope.dollar0.orElse(Some(newSelf)),
              Some(newSelf),
              None,
              {
                val arr = new Array[Lazy](scope.bindings0.length)
                for((i, v) <- newBindings) arr(i) = v.apply(scope.self0.orNull, None)
                arr
              },
              Some(s),
            )

            lazy val newBindings = visitBindings(
              (preLocals.iterator ++ postLocals).collect{ case Member.BindStmt(b) => b},
              (self, sup) => newScope
            ).toArray

            visitExpr(key)(s, implicitly) match {
              case Val.Str(k) =>
                Some(k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _) =>
                  visitExpr(value)(
                    s.copy(self0 = Some(self), dollar0 = Some(s.dollar0.getOrElse(self))) ++
                      newBindings,
                    implicitly
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

  def visitComp(f: List[CompSpec], scopes: Seq[Scope])
               (implicit fileScope: FileScope): Seq[Scope] = f match{
    case ForSpec(offset, name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr)(s, implicitly) match{
            case Val.Arr(value) => value
            case r => Evaluator.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              expr.offset
            )
          }
        } yield s ++ Seq(name -> ((self: Val.Obj, sup: Option[Val.Obj]) => e))
      )
    case IfSpec(offset, expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr)(_, implicitly) == Val.True))
    case Nil => scopes
  }
}

