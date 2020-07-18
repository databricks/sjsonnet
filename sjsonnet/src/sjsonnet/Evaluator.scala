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
class Evaluator(parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]],
                val extVars: Map[String, ujson.Value],
                val wd: Path,
                importer: (Path, String) => Option[(Path, String)],
                override val preserveOrder: Boolean = false) extends EvalScope{
  implicit def evalScope: EvalScope = this

  val loadedFileContents = mutable.Map.empty[Path, String]
  def loadCachedSource(p: Path) = loadedFileContents.get(p)
  def materialize(v: Val): Value = Materializer.apply(v)
  val cachedImports = collection.mutable.Map.empty[Path, Val]

  val cachedImportedStrings = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr)
               (implicit scope: ValScope, fileScope: FileScope): Val = try expr match{
    case Null(offset) => Val.Null
    case Parened(offset, inner) => visitExpr(inner)
    case True(offset) => Val.True
    case False(offset) => Val.False
    case Self(offset) => scope.self0.getOrElse(Error.fail("Cannot use `self` outside an object", offset))

    case BinaryOp(offset, lhs, Expr.BinaryOp.`in`, Super(_)) =>
      scope.super0 match{
        case None => Val.False
        case Some(sup) =>
          val key = visitExpr(lhs).cast[Val.Str]
          Val.bool(sup.containsKey(key.value))
      }

    case $(offset) => scope.dollar0.getOrElse(Error.fail("Cannot use `$` outside an object", offset))
    case Str(offset, value) => Val.Str(value)
    case Num(offset, value) => Val.Num(value)
    case Id(offset, value) => visitId(offset, value)

    case Arr(offset, value) => Val.Arr(value.map(v => Val.Lazy(visitExpr(v))))
    case Obj(offset, value) => visitObjBody(value)

    case UnaryOp(offset, op, value) => visitUnaryOp(op, value)

    case BinaryOp(offset, lhs, op, rhs) => {
      visitBinaryOp(offset, lhs, op, rhs)
    }
    case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
      visitAssert(offset, value, msg, returned)

    case LocalExpr(offset, bindings, returned) =>
      lazy val newScope: ValScope = scope.extend(visitBindings(bindings.iterator, (self, sup) => newScope))
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
      Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Val.Lazy(visitExpr(value)(s, implicitly))))
    case ObjExtend(offset, value, ext) => {
      val original = visitExpr(value).cast[Val.Obj]
      val extension = visitObjBody(ext)
      extension.addSuper(original)
    }
  } catch Error.tryCatch(expr.offset)

  def visitId(offset: Int, value: Int)(implicit scope: ValScope, fileScope: FileScope): Val = {
    val ref = scope.bindings(value)
      .getOrElse(
        Error.fail(
          "Unknown variable " + fileScope.indexNames(value),
          offset
        )
      )

    try ref.force catch Error.tryCatchWrap(offset)
  }

  def visitIfElse(offset: Int,
                  cond: Expr,
                  then: Expr,
                  else0: Option[Expr])
                 (implicit scope: ValScope,
                  fileScope: FileScope): Val = {
    visitExpr(cond) match {
      case Val.True => visitExpr(then)
      case Val.False =>
        else0 match{
          case None => Val.Null
          case Some(v) => visitExpr(v)
        }
      case v => Error.fail("Need boolean, found " + v.prettyName, offset)
    }
  }

  def visitError(offset: Int, value: Expr)
                (implicit scope: ValScope, fileScope: FileScope): Nothing = {
    Error.fail(
      visitExpr(value) match {
        case Val.Str(s) => s
        case r =>
          try Materializer.stringify(r)
          catch Error.tryCatchWrap(offset)
      },
      offset
    )
  }

  def visitUnaryOp(op: UnaryOp.Op, value: Expr)
                  (implicit scope: ValScope, fileScope: FileScope): Val = {
    (op, visitExpr(value)) match {
      case (Expr.UnaryOp.`-`, Val.Num(v)) => Val.Num(-v)
      case (Expr.UnaryOp.`+`, Val.Num(v)) => Val.Num(v)
      case (Expr.UnaryOp.`~`, Val.Num(v)) => Val.Num(~v.toLong)
      case (Expr.UnaryOp.`!`, Val.True) => Val.False
      case (Expr.UnaryOp.`!`, Val.False) => Val.True
    }
  }

  private def visitApply(offset: Int, value: Expr, args: Seq[(Option[String], Expr)])
                        (implicit scope: ValScope, fileScope: FileScope) = {
    val lhs = visitExpr(value)
    try lhs.cast[Val.Func].apply(
      args.map { case (k, v) => (k, Val.Lazy(visitExpr(v))) },
      fileScope.currentFile.last,
      offset
    )
    catch Error.tryCatchWrap(offset)
  }

  def visitAssert(offset: Int, value: Expr, msg: Option[Expr], returned: Expr)
                 (implicit scope: ValScope, fileScope: FileScope): Val = {
    if (visitExpr(value) != Val.True) {
      msg match {
        case None => Error.fail("Assertion failed", offset)
        case Some(msg) =>
          Error.fail(
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
                        (implicit scope: ValScope, fileScope: FileScope)= {
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
      case x => Error.fail("Can only slice array or string, not " + x.prettyName, offset)
    }
  }

  def visitLookup(offset: Int, value: Expr, index: Expr)
                 (implicit scope: ValScope, fileScope: FileScope): Val = {
    if (value.isInstanceOf[Super]) {
      val key = visitExpr(index).cast[Val.Str]
      scope.super0.getOrElse(scope.self0.getOrElse(Error.fail("Cannot use `super` outside an object", offset))).value(key.value, offset)
    } else (visitExpr(value), visitExpr(index)) match {
      case (v: Val.Arr, i: Val.Num) =>
        if (i.value > v.value.length) Error.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", offset)
        val int = i.value.toInt
        if (int != i.value) Error.fail("array index was not integer: " + i.value, offset)
        try v.value(int).force
        catch Error.tryCatchWrap(offset)
      case (v: Val.Str, i: Val.Num) => Val.Str(new String(Array(v.value(i.value.toInt))))
      case (v: Val.Obj, i: Val.Str) =>
        val ref = v.value(i.value, offset)
        try ref
        catch Error.tryCatchWrap(offset)
      case (lhs, rhs) =>
        Error.fail(s"attempted to index a ${lhs.prettyName} with ${rhs.prettyName}", offset)
    }
  }

  def visitSelect(offset: Int, value: Expr, name: String)
                 (implicit scope: ValScope, fileScope: FileScope): Val = {
    if (value.isInstanceOf[Super]) {
      scope.super0
        .getOrElse(Error.fail("Cannot use `super` outside an object", offset))
        .value(name, offset, scope.self0.get)
    } else visitExpr(value) match {
      case obj: Val.Obj => obj.value(name, offset)
      case r => Error.fail(s"attempted to index a ${r.prettyName} with string ${name}", offset)
    }
  }

  def visitImportStr(offset: Int, value: String)(implicit scope: ValScope, fileScope: FileScope) = {
    val (p, str) = resolveImport(value, offset)
    Val.Str(cachedImportedStrings.getOrElseUpdate(p, str))
  }

  def visitImport(offset: Int, value: String)(implicit scope: ValScope, fileScope: FileScope) = {
    val (p, str) = resolveImport(value, offset)
    loadedFileContents(p) = str
    cachedImports.getOrElseUpdate(
      p,
      {
        val (doc, nameIndices) = parseCache.getOrElseUpdate(
          str,
          fastparse.parse(str, Parser.document(_))
        ) match {
          case Parsed.Success((doc, nameIndices), _) => (doc, nameIndices)
          case f @ Parsed.Failure(l, i, e) =>
            Error.fail(
              "Imported file " + pprint.Util.literalize(value) +
                " had Parse error. " + f.trace().msg,
              offset
            )
        }
        val newFileScope = new FileScope(p, nameIndices)
        try visitExpr(doc)(Std.scope(nameIndices.size), newFileScope)
        catch Error.tryCatchWrap(offset)
      }
    )
  }

  def resolveImport(value: String, offset: Int)
                   (implicit scope: ValScope, fileScope: FileScope): (Path, String) = {
    importer(fileScope.currentFile.parent(), value)
      .getOrElse(
        Error.fail(
          "Couldn't import file: " + pprint.Util.literalize(value),
          offset
        )
      )
  }

  def visitBinaryOp(offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr)
                   (implicit scope: ValScope, fileScope: FileScope) = {
    op match {
      // && and || are handled specially because unlike the other operators,
      // these short-circuit during evaluation in some cases when the LHS is known.
      case Expr.BinaryOp.`&&` | Expr.BinaryOp.`||` =>
        (visitExpr(lhs), op) match {
          case (lhs, Expr.BinaryOp.`&&`) =>
            lhs match{
              case Val.True =>
                visitExpr(rhs) match{
                  case b: Val.Bool => b
                  case unknown =>
                    Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", offset)
                }
              case Val.False => Val.False
              case unknown =>
                Error.fail(s"binary operator && does not operate on ${unknown.prettyName}s.", offset)
            }
          case (lhs, Expr.BinaryOp.`||`) =>
            lhs match{
              case Val.True => Val.True
              case Val.False =>
                visitExpr(rhs) match{
                  case b: Val.Bool => b
                  case unknown =>
                    Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", offset)
                }
              case unknown =>
                Error.fail(s"binary operator || does not operate on ${unknown.prettyName}s.", offset)
            }
          case _ => visitExpr(rhs)

        }
      case _ =>
        (visitExpr(lhs), op, visitExpr(rhs)) match {
          case (Val.Num(l), Expr.BinaryOp.`*`, Val.Num(r)) => Val.Num(l * r)
          case (Val.Num(l), Expr.BinaryOp.`/`, Val.Num(r)) =>
            if (r == 0) Error.fail("division by zero", offset)
            Val.Num(l / r)
          case (Val.Num(l), Expr.BinaryOp.`%`, Val.Num(r)) => Val.Num(l % r)
          case (Val.Num(l), Expr.BinaryOp.`+`, Val.Num(r)) => Val.Num(l + r)
          case (Val.Str(l), Expr.BinaryOp.`%`, r) =>
            try Val.Str(Format.format(l, r, offset))
            catch Error.tryCatchWrap(offset)

          case (Val.Str(l), Expr.BinaryOp.`+`, Val.Str(r)) => Val.Str(l + r)
          case (Val.Str(l), Expr.BinaryOp.`<`, Val.Str(r)) => Val.bool(l < r)
          case (Val.Str(l), Expr.BinaryOp.`>`, Val.Str(r)) => Val.bool(l > r)
          case (Val.Str(l), Expr.BinaryOp.`<=`, Val.Str(r)) => Val.bool(l <= r)
          case (Val.Str(l), Expr.BinaryOp.`>=`, Val.Str(r)) => Val.bool(l >= r)
          case (Val.Str(l), Expr.BinaryOp.`+`, r) =>
            try Val.Str(l + Materializer.stringify(r))
            catch Error.tryCatchWrap(offset)
          case (l, Expr.BinaryOp.`+`, Val.Str(r)) =>
            try Val.Str(Materializer.stringify(l) + r)
            catch Error.tryCatchWrap(offset)
          case (Val.Num(l), Expr.BinaryOp.`-`, Val.Num(r)) => Val.Num(l - r)
          case (Val.Num(l), Expr.BinaryOp.`<<`, Val.Num(r)) => Val.Num(l.toLong << r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`>>`, Val.Num(r)) => Val.Num(l.toLong >> r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`<`, Val.Num(r)) => Val.bool(l < r)
          case (Val.Num(l), Expr.BinaryOp.`>`, Val.Num(r)) => Val.bool(l > r)
          case (Val.Num(l), Expr.BinaryOp.`<=`, Val.Num(r)) => Val.bool(l <= r)
          case (Val.Num(l), Expr.BinaryOp.`>=`, Val.Num(r)) => Val.bool(l >= r)
          case (l, Expr.BinaryOp.`==`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Error.fail("cannot test equality of functions", offset)
            }
            try Val.bool(Materializer(l) == Materializer(r))
            catch Error.tryCatchWrap(offset)
          case (l, Expr.BinaryOp.`!=`, r) =>
            if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]) {
              Error.fail("cannot test equality of functions", offset)
            }
            try Val.bool(Materializer(l) != Materializer(r))
            catch Error.tryCatchWrap(offset)
          case (Val.Str(l), Expr.BinaryOp.`in`, o: Val.Obj) => Val.bool(o.containsKey(l))
          case (Val.Num(l), Expr.BinaryOp.`&`, Val.Num(r)) => Val.Num(l.toLong & r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`^`, Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
          case (Val.Num(l), Expr.BinaryOp.`|`, Val.Num(r)) => Val.Num(l.toLong | r.toLong)
          case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => r.addSuper(l)
          case (Val.Arr(l), Expr.BinaryOp.`+`, Val.Arr(r)) => Val.Arr(l ++ r)
          case (l, op, r) =>
            Error.fail(s"Unknown binary operation: ${l.prettyName} $op ${r.prettyName}", offset)
        }
    }
  }

  def visitFieldName(fieldName: FieldName, offset: Int)
                    (implicit scope: ValScope, fileScope: FileScope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
        case x => Error.fail(
          s"Field name must be string or null, not ${x.prettyName}",
          offset
        )
      }
    }
  }

  def visitMethod(rhs: Expr, params: Params, outerOffset: Int)
                 (implicit scope: ValScope, fileScope: FileScope) = {
    Val.Func(
      Some(scope -> fileScope),
      params,
      (s, _, _, fs, _) => visitExpr(rhs)(s, fs),
      (default, s, e) => visitExpr(default)(s, fileScope)
    )
  }

  def visitBindings(bindings: Iterator[Bind], scope: (Option[Val.Obj], Option[Val.Obj]) => ValScope)
                   (implicit fileScope: FileScope)= {
    bindings.map{ b: Bind =>
      b.args match{
        case None =>
          (
            b.name,
            (self: Option[Val.Obj], sup: Option[Val.Obj]) =>
              Val.Lazy(visitExpr(b.rhs)(scope(self, sup), implicitly))
          )
        case Some(argSpec) =>
          (
            b.name,
            (self: Option[Val.Obj], sup: Option[Val.Obj]) =>
              Val.Lazy(visitMethod(b.rhs, argSpec, b.offset)(scope(self, sup), implicitly))

          )
      }
    }
  }

  def visitObjBody(b: ObjBody)(implicit scope: ValScope, fileScope: FileScope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      var asserting: Boolean = false
      def assertions(self: Val.Obj) = if (!asserting) {
        asserting = true
        val newScope: ValScope = makeNewScope(Some(self), self.getSuper)

        value.collect {
          case Member.AssertStmt(value, msg) =>

            if (visitExpr(value)(newScope, fileScope) != Val.True) {
              msg match{
                case None => Error.fail("Assertion failed", value.offset)
                case Some(msg) =>
                  Error.fail(
                    "Assertion failed: " + visitExpr(msg)(newScope, implicitly).cast[Val.Str].value,
                    value.offset
                  )
              }
            }
        }
      }

      def makeNewScope(self: Option[Val.Obj], sup: Option[Val.Obj]): ValScope = {
        scope.extend(
          newBindings,
          newDollar = scope.dollar0.orElse(self),
          newSelf = self,
          newSuper = sup
        )
      }

      lazy val newBindings = visitBindings(
        value.iterator.collect{case Member.BindStmt(b) => b},
        (self, sup) => makeNewScope(self, sup)
      ).toArray

      lazy val newSelf: Val.Obj = {
        val builder = mutable.LinkedHashMap.newBuilder[String, Val.Obj.Member]
        value.foreach {
          case Member.Field(offset, fieldName, plus, None, sep, rhs) =>
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Option[Val.Obj], _, _) => {
              assertions(self)
              visitExpr(rhs)(makeNewScope(Some(self), sup), implicitly)
            })).foreach(builder.+=)
          case Member.Field(offset, fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, offset).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Option[Val.Obj], _, _) => {
              assertions(self)
              visitMethod(rhs, argSpec, offset)(makeNewScope(Some(self), sup), implicitly)
            })).foreach(builder.+=)
          case _: Member.BindStmt => // do nothing
          case _: Member.AssertStmt => // do nothing
        }

        new Val.Obj(builder.result(), self => assertions(self), None)
      }
      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      lazy val compScope: ValScope = scope.extend(
        newSuper = None
      )

      lazy val newSelf: Val.Obj = {
        val builder = mutable.LinkedHashMap.newBuilder[String, Val.Obj.Member]
        for(s <- visitComp(first :: rest.toList, Seq(compScope))){
          lazy val newScope: ValScope = s.extend(
            newBindings,
            newDollar = scope.dollar0.orElse(Some(newSelf)),
            newSelf = Some(newSelf),
            newSuper = None
          )

          lazy val newBindings = visitBindings(
            (preLocals.iterator ++ postLocals).collect{ case Member.BindStmt(b) => b},
            (self, sup) => newScope
          ).toArray

          visitExpr(key)(s, implicitly) match {
            case Val.Str(k) =>
              builder += (k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj], _, _) =>
                visitExpr(value)(
                  s.extend(
                    newBindings,
                    newDollar = Some(s.dollar0.getOrElse(self)),
                    newSelf = Some(self),
                  ),
                  implicitly
                )
              ))
            case Val.Null => // do nothing
          }
        }
        new Val.Obj(builder.result(), _ => (), None)
      }

      newSelf
  }

  def visitComp(f: List[CompSpec], scopes: Seq[ValScope])
               (implicit fileScope: FileScope): Seq[ValScope] = f match{
    case ForSpec(offset, name, expr) :: rest =>
      visitComp(
        rest,
        for{
          s <- scopes
          e <- visitExpr(expr)(s, implicitly) match{
            case Val.Arr(value) => value
            case r => Error.fail(
              "In comprehension, can only iterate over array, not " + r.prettyName,
              expr.offset
            )
          }
        } yield s.extend(Seq(name -> ((self: Option[Val.Obj], sup: Option[Val.Obj]) => e)))
      )
    case IfSpec(offset, expr) :: rest =>
      visitComp(rest, scopes.filter(visitExpr(expr)(_, implicitly) match {
        case Val.True => true
        case Val.False => false
        case other => Error.fail(
          "Condition must be boolean, got " + other.prettyName,
          expr.offset
        )
      }))
    case Nil => scopes
  }
}

