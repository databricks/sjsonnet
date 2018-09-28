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
        case None => Val.Obj(current.value0, Some(lhs))
        case Some(x) => Val.Obj(current.value0, Some(rec(x)))
      }
    }
    rec(rhs)
  }

  def tryCatch[T](scope: Scope, offset: Int)(t: => T) = {
    try t catch{
      case e: EvaluatorError => throw e
      case e: Throwable =>
        throw new EvaluatorError("Internal Error", Nil, Some(e))
          .addFrame(scope.fileName, offset)
    }
  }
  def tryCatch2[T](path: Path, offset: Int)(t: => T) = {
    try t catch{
      case e: EvaluatorError => throw e.addFrame(path, offset)
      case e: Throwable =>
        throw new EvaluatorError("Internal Error", Nil, Some(e))
          .addFrame(path, offset)
    }
  }
  def fail(msg: String, path: Path, offset: Int) = {
    throw new EvaluatorError(msg, Nil, None).addFrame(path, offset)
  }
}
case class EvaluatorError(msg: String,
                          stack: List[StackTraceElement],
                          underlying: Option[Throwable])
  extends Exception(msg){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: Path, offset: Int) = {
    val Array(line, col) = StringReprOps.prettyIndex(
      new IndexedParserInput(ammonite.ops.read(fileName))(StringReprOps), offset
    ).split(':')

    val newFrame = new StackTraceElement(
      "", "",
      fileName.relativeTo(ammonite.ops.pwd).toString + ":" + line,
      col.toInt
    )

    this.copy(stack = newFrame :: this.stack)
  }
}
class Evaluator(parser: Parser, originalScope: Scope) {
  val imports = collection.mutable.Map.empty[Path, Val]
  val importStrs = collection.mutable.Map.empty[Path, String]
  def visitExpr(expr: Expr, scope: => Scope): Val = Evaluator.tryCatch(scope, expr.offset){
    expr match{
      case Null(offset) => Val.Null
      case Parened(offset, inner) => visitExpr(inner, scope)
      case True(offset) => Val.True
      case False(offset) => Val.False
      case Self(offset) => scope.self

      case Select(_, Super(offset), name) =>
        val ref = scope.super0.get.value(name, scope.fileName, offset, self = scope.self)
        Evaluator.tryCatch2(scope.fileName, offset) {ref.calc}

      case Lookup(_, Super(offset), index) =>
        val key = visitExpr(index, scope).asInstanceOf[Val.Str]
        scope.super0.get.value(key.value, scope.fileName, offset).calc

      case BinaryOp(_, lhs, Expr.BinaryOp.`in`, Super(offset)) =>
        val key = visitExpr(lhs, scope).asInstanceOf[Val.Str]
        Val.bool(scope.super0.get.value0.contains(key.value))

      case $(offset) => scope.dollar
      case Str(offset, value) => Val.Str(value)
      case Num(offset, value) => Val.Num(value)
      case Id(offset, value) =>
        val ref = scope.bindings(value)
          .getOrElse(Evaluator.fail("Unknown variable " + value, scope.fileName, offset))

        Evaluator.tryCatch2(scope.fileName, offset){ref.force(scope.dollar0)}

      case Arr(offset, value) => Val.Arr(value.map(v => Ref(visitExpr(v, scope))))
      case Obj(offset, value) => visitObjBody(value, scope)

      case UnaryOp(offset, op, value) => (op, visitExpr(value, scope)) match{
        case (Expr.UnaryOp.`-`, Val.Num(v)) => Val.Num(-v)
        case (Expr.UnaryOp.`+`, Val.Num(v)) => Val.Num(v)
        case (Expr.UnaryOp.`~`, Val.Num(v)) => Val.Num(~v.toLong)
        case (Expr.UnaryOp.`!`, Val.True) => Val.False
        case (Expr.UnaryOp.`!`, Val.False) => Val.True
      }

      case BinaryOp(offset, lhs, op, rhs) => {
        op match{
          case Expr.BinaryOp.`&&` | Expr.BinaryOp.`||` =>
            (visitExpr(lhs, scope), op) match {
              case (Val.False, Expr.BinaryOp.`&&`) => Val.False
              case (Val.True, Expr.BinaryOp.`||`) => Val.True
              case _ => visitExpr(rhs, scope)

            }
          case _ =>
            (visitExpr(lhs, scope), op, visitExpr(rhs, scope)) match{
              case (Val.Num(l), Expr.BinaryOp.`*`, Val.Num(r)) => Val.Num(l * r)
              case (Val.Num(l), Expr.BinaryOp.`/`, Val.Num(r)) =>
                if (r == 0) Evaluator.fail("division by zero", scope.fileName, offset)
                Val.Num(l / r)
              case (Val.Num(l), Expr.BinaryOp.`%`, Val.Num(r)) => Val.Num(l % r)
              case (Val.Num(l), Expr.BinaryOp.`+`, Val.Num(r)) => Val.Num(l + r)
              case (Val.Str(l), Expr.BinaryOp.`%`, r) =>
                Evaluator.tryCatch2(scope.fileName, offset){
                  Val.Str(Format.format(l, Materializer.apply(r)))
                }
              case (Val.Str(l), Expr.BinaryOp.`+`, Val.Str(r)) => Val.Str(l + r)
              case (Val.Str(l), Expr.BinaryOp.`<`, Val.Str(r)) => Val.bool(l < r)
              case (Val.Str(l), Expr.BinaryOp.`>`, Val.Str(r)) => Val.bool(l > r)
              case (Val.Str(l), Expr.BinaryOp.`<=`, Val.Str(r)) => Val.bool(l <= r)
              case (Val.Str(l), Expr.BinaryOp.`>=`, Val.Str(r)) => Val.bool(l >= r)
              case (Val.Str(l), Expr.BinaryOp.`+`, r) =>
                Evaluator.tryCatch2(scope.fileName, offset){
                  Val.Str(l + Materializer.apply(r).transform(new Renderer()).toString)
                }
              case (l, Expr.BinaryOp.`+`, Val.Str(r)) =>
                Evaluator.tryCatch2(scope.fileName, offset){
                  Val.Str(Materializer.apply(l).transform(new Renderer()).toString + r)
                }
              case (Val.Num(l), Expr.BinaryOp.`-`, Val.Num(r)) => Val.Num(l - r)
              case (Val.Num(l), Expr.BinaryOp.`<<`, Val.Num(r)) => Val.Num(l.toLong << r.toLong)
              case (Val.Num(l), Expr.BinaryOp.`>>`, Val.Num(r)) => Val.Num(l.toLong >> r.toLong)
              case (Val.Num(l), Expr.BinaryOp.`<`, Val.Num(r)) => Val.bool(l < r)
              case (Val.Num(l), Expr.BinaryOp.`>`, Val.Num(r)) => Val.bool(l > r)
              case (Val.Num(l), Expr.BinaryOp.`<=`, Val.Num(r)) => Val.bool(l <= r)
              case (Val.Num(l), Expr.BinaryOp.`>=`, Val.Num(r)) => Val.bool(l >= r)
              case (l, Expr.BinaryOp.`==`, r) =>
                if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]){
                  Evaluator.fail("cannot test equality of functions", scope.fileName, offset)
                }
                Evaluator.tryCatch2(scope.fileName, offset){
                  Val.bool(Materializer(l) == Materializer(r))
                }
              case (l, Expr.BinaryOp.`!=`, r) =>
                if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func]){
                  Evaluator.fail("cannot test equality of functions", scope.fileName, offset)
                }
                Evaluator.tryCatch2(scope.fileName, offset) {
                  Val.bool(Materializer(l) != Materializer(r))
                }
              case (Val.Str(l), Expr.BinaryOp.`in`, Val.Obj(r, _)) => Val.bool(r.contains(l))
              case (Val.Num(l), Expr.BinaryOp.`&`, Val.Num(r)) => Val.Num(l.toLong & r.toLong)
              case (Val.Num(l), Expr.BinaryOp.`^`, Val.Num(r)) => Val.Num(l.toLong ^ r.toLong)
              case (Val.Num(l), Expr.BinaryOp.`|`, Val.Num(r)) => Val.Num(l.toLong | r.toLong)
              case (l: Val.Obj, Expr.BinaryOp.`+`, r: Val.Obj) => Evaluator.mergeObjects(l, r)
              case (Val.Arr(l), Expr.BinaryOp.`+`, Val.Arr(r)) => Val.Arr(l ++ r)
            }
        }
      }
      case AssertExpr(offset, Member.AssertStmt(value, msg), returned) =>
        if (visitExpr(value, scope) != Val.True) {
          msg match{
            case None => Evaluator.fail("Assertion failed", scope.fileName, offset)
            case Some(msg) => Evaluator.fail("Assertion failed: " + visitExpr(msg, scope).asInstanceOf[Val.Str].value, scope.fileName, offset)
          }
        }
        visitExpr(returned, scope)

      case LocalExpr(offset, bindings, returned) =>
        lazy val newScope: Scope = scope ++ visitBindings(bindings, (self, sup) => newScope)
        visitExpr(returned, newScope)

      case Import(offset, value) =>



        val p = (scope.fileName/ammonite.ops.up :: scope.searchRoots).map(_  / RelPath(value)).find(ammonite.ops.exists).get
        imports.getOrElseUpdate(
          p,
          visitExpr(
            parser.expr.parse(
              try ammonite.ops.read(p)
              catch{case e: Throwable =>
                Evaluator.fail("Couldn't import file: " + pprint.Util.literalize(value), scope.fileName, offset)
              }
            )match{
              case Parsed.Success(x, _) => x
              case f @ Parsed.Failure(l, i, e) => Evaluator.fail("Imported file " + pprint.Util.literalize(value) + " had syntax error " + f.msg, scope.fileName, offset)
            },
            originalScope.copy(fileName = p)
          )
        )


      case ImportStr(offset, value) =>

        val p = (scope.fileName/ammonite.ops.up :: scope.searchRoots).map(_  / RelPath(value)).find(ammonite.ops.exists).get
        Val.Str(
          importStrs.getOrElseUpdate(
            p,
            try ammonite.ops.read(p)
            catch{case e: Throwable =>
              Evaluator.fail("Couldn't import file: " + pprint.Util.literalize(value), scope.fileName, offset)
            }
          )
        )
      case Error(offset, value) =>
        Evaluator.fail(
          visitExpr(value, scope) match{
            case Val.Str(s) => s
            case r =>
              Evaluator.tryCatch2(scope.fileName, offset){
                Materializer(r).toString()
              }
          },
          scope.fileName, offset
        )
      case Apply(offset, value, Args(args)) =>
        Evaluator.tryCatch2(scope.fileName, offset){
          visitExpr(value, scope).asInstanceOf[Val.Func].value(args.map{case (k, v) => (k, Ref(visitExpr(v, scope)))})
        }

      case Select(offset, value, name) =>
        visitExpr(value, scope) match{
          case obj: Val.Obj =>
            val ref = obj.value(name, scope.fileName, offset)
            Evaluator.tryCatch2(scope.fileName, offset) {ref.force(scope.dollar0)}
          case r =>
            Evaluator.fail(s"attemped to index a ${r.prettyName} with string ${name}", scope.fileName, offset)
        }

      case Lookup(offset, value, index) =>

        (visitExpr(value, scope), visitExpr(index, scope)) match {
          case (v: Val.Arr, i: Val.Num) =>
            if (i.value > v.value.length) Evaluator.fail(s"array bounds error: ${i.value} not within [0, ${v.value.length})", scope.fileName, offset)
            val int = i.value.toInt
            if (int != i.value) Evaluator.fail("array index was not integer: " + i.value, scope.fileName, offset)
            Evaluator.tryCatch2(scope.fileName, offset) {
              v.value(int).force(scope.dollar0)
            }
          case (v: Val.Str, i: Val.Num) => Val.Str(new String(Array(v.value(i.value.toInt))))
          case (v: Val.Obj, i: Val.Str) =>
            val ref = v.value(i.value, scope.fileName, offset)
            Evaluator.tryCatch2(scope.fileName, offset) { ref.force(scope.dollar0) }
          case (lhs, rhs) =>
            Evaluator.fail(s"attemped to index a ${lhs.prettyName} with ${rhs.prettyName}", scope.fileName, offset)
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
        Val.Arr(visitComp(first :: rest.toList, Seq(scope)).map(s => Ref(visitExpr(value, s))))
      case ObjExtend(offset, value, ext) => {
        val original = visitExpr(value, scope).asInstanceOf[Val.Obj]
        val extension = visitObjBody(ext, scope)
        Evaluator.mergeObjects(original, extension)
      }
    }
  }
  def visitFieldName(fieldName: FieldName, scope: => Scope) = {
    fieldName match{
      case FieldName.Fixed(s) => Some(s)
      case FieldName.Dyn(k) => visitExpr(k, scope) match{
        case Val.Str(k1) => Some(k1)
        case Val.Null => None
      }
    }
  }

  def visitMethod(scope: Scope, rhs: Expr, argSpec: Params, outerOffset: Int) = {
    Val.Func(
      argSpec.args.length,
      { args =>
        lazy val newScope1 =
          argSpec.args.collect{
            case (k, Some(default)) => (k, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitExpr(default, newScope)))
          }
        lazy val newScope2 = try
          args.zipWithIndex.map{
            case ((Some(name), v), _) => (name, (self: Val.Obj, sup: Option[Val.Obj]) => v)
            case ((None, v), i) => (argSpec.args(i)._1, (self: Val.Obj, sup: Option[Val.Obj]) => v)
          }
        catch{
          case e: IndexOutOfBoundsException =>
            Evaluator.fail("Too many args, function has " + argSpec.args.length + " parameter(s)", scope.fileName, outerOffset)
        }
        lazy val seen = collection.mutable.Set.empty[String]
        for((k, v) <- newScope2){
          if (seen(k)) Evaluator.fail("Parameter passed more than once: " + k, scope.fileName, outerOffset)
          else seen.add(k)
        }

        lazy val newScope: Scope  = scope ++ newScope1 ++ newScope2
        visitExpr(rhs, newScope)
      }
    )
  }
  def visitBindings(bindings: Seq[Bind], scope: (Val.Obj, Option[Val.Obj]) => Scope) = {

    bindings.collect{
      case Bind(offset, fieldName, None, rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitExpr(rhs, scope(self, sup))))
      case Bind(offset, fieldName, Some(argSpec), rhs) =>
        (fieldName, (self: Val.Obj, sup: Option[Val.Obj]) => Ref(visitMethod(scope(self, sup), rhs, argSpec, offset)))
    }
  }
  def visitObjBody(b: ObjBody, scope: => Scope): Val.Obj = b match{
    case ObjBody.MemberList(value) =>
      def makeNewScope(self: => Val.Obj, sup: => Option[Val.Obj]): Scope = new Scope(
        scope.dollar0.orElse(Some(self)),
        Some(self),
        sup,
        newBindings.map{case (k, v) => (k, v.apply(self, sup))}.toMap,
        scope.fileName,
        scope.searchRoots,
        Some(scope)
      )


      lazy val newBindings = visitBindings(
        value.collect{case Member.BindStmt(b) => b},
        (self, sup) => makeNewScope(self, sup)
      )

      lazy val newSelf: Val.Obj = Val.Obj(
        value.flatMap {
          case Member.Field(offset, fieldName, plus, None, sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> Val.Obj.Member(plus, sep, (self: Val.Obj, sup: Option[Val.Obj]) => {
              Ref(visitExpr(rhs, makeNewScope(self, sup)))
            }))
          case Member.Field(offset, fieldName, false, Some(argSpec), sep, rhs) =>
            visitFieldName(fieldName, scope).map(_ -> Val.Obj.Member(false, sep, (self: Val.Obj, sup: Option[Val.Obj]) =>
              Ref(visitMethod(makeNewScope(self, sup), rhs, argSpec, offset))
            ))

          case _: Member.BindStmt => None
          case Member.AssertStmt(value, msg) =>
            val newScope: Scope = scope//makeNewScope(newSelf, None)
            if (visitExpr(value, newScope) != Val.True) {
              msg match{
                case None => Evaluator.fail("Assertion failed", scope.fileName, value.offset)
                case Some(msg) => Evaluator.fail("Assertion failed: " + visitExpr(msg, newScope).asInstanceOf[Val.Str].value, scope.fileName, value.offset)
              }
            }
            None
        }.toMap,
        None
      )
      newSelf

    case ObjBody.ObjComp(preLocals, key, value, postLocals, first, rest) =>
      lazy val compScope: Scope = new Scope(
        scope.dollar0,
        scope.self0,
        None,
        Map(),
        scope.fileName,
        scope.searchRoots,
        Some(scope)
      )

      lazy val newScope: Scope = new Scope(
        scope.dollar0.orElse(Some(newSelf)),
        Some(newSelf),
        None,
        newBindings.map{case (k, v) => (k, v.apply(scope.self0.getOrElse(null), None))}.toMap,
        scope.fileName,
        scope.searchRoots,
        Some(scope)
      )

      lazy val newBindings = visitBindings(
        (preLocals ++ postLocals).collect{ case Member.BindStmt(b) => b},
        (self, sup) => newScope
      )

      lazy val newSelf = Val.Obj(
        visitComp(first :: rest.toList, Seq(compScope))
          .flatMap(s =>
            visitExpr(key, s) match{
              case Val.Str(k) =>
                Some(k -> Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Option[Val.Obj]) =>
                  Ref(visitExpr(
                    value,
                    s.copy(self0 = Some(self), dollar0 = Some(s.dollar0.getOrElse(self))) ++
                    newBindings
                  ))
                ))
              case Val.Null => None
            }

          )
          .toMap,
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
            case r => Evaluator.fail("In comprehension, can only iterate over array, not " + r.prettyName, s.fileName, expr.offset)
          }
        } yield s ++ Seq(name -> ((self: Val.Obj, sup: Option[Val.Obj]) => e))
      )
    case IfSpec(offset, expr) :: rest => visitComp(rest, scopes.filter(visitExpr(expr, _) == Val.True))
    case Nil => scopes
  }
}

