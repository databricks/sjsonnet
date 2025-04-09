package sjsonnet

import fastparse.JsonnetWhitespace._
import fastparse._
import Expr.Member.Visibility

import scala.annotation.switch
import scala.collection.mutable

/**
  * Parses Jsonnet source code `String`s into a [[Expr]] syntax tree, using the
  * FastParse parsing library. Uses precedence climbing to handle infix
  * operators, and resolves local variable names to array indices during parsing
  * to allow better performance at runtime.
  */

object Parser {
  val precedenceTable: Seq[Seq[String]] = Seq(
    Seq("*", "/", "%"),
    Seq("+", "-"),
    Seq("<<", ">>"),
    Seq("<", ">", "<=", ">=", "in"),
    Seq("==", "!="),
    Seq("&"),
    Seq("^"),
    Seq("|"),
    Seq("&&"),
    Seq("||")
  )

  val precedence: Map[String,Int] = precedenceTable
    .reverse
    .zipWithIndex
    .flatMap{case (ops, idx) => ops.map(_ -> idx)}
    .toMap

  val keywords: Set[String] = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true", "importbin"
  )

  def idStartChar(c: Char): Boolean = c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private val emptyLazyArray = new Array[Lazy](0)
}

class Parser(val currentFile: Path,
             strictImportSyntax: Boolean,
             internedStrings: mutable.HashMap[String, String],
             internedStaticFieldSets: mutable.HashMap[Val.StaticObjectFieldSet, java.util.LinkedHashMap[String, java.lang.Boolean]]) {
  import Parser._

  private val fileScope = new FileScope(currentFile)

  def Pos(implicit p: P[?]): P[Position] = Index.map(offset => new Position(fileScope, offset))

  def id(implicit p: P[?]): P[String] = P(
    CharIn("_a-zA-Z") ~~
    CharsWhileIn("_a-zA-Z0-9", 0)
  ).!.filter(s => !keywords.contains(s))

  def break(implicit p: P[?]): P[Unit] = P(!CharIn("_a-zA-Z0-9"))
  def number(implicit p: P[?]): P[Val.Num] = P(
    Pos ~~ (
      CharsWhileIn("0-9") ~~
      ("." ~ CharsWhileIn("0-9")).? ~~
      (CharIn("eE") ~ CharIn("+\\-").? ~~ CharsWhileIn("0-9")).?
    ).!
  ).map(s => Val.Num(s._1, s._2.toDouble))

  def escape(implicit p: P[?]): P[String] = P( escape0 | escape1 )
  def escape0(implicit p: P[?]): P[String] = P("\\" ~~ !"u" ~~ AnyChar.!).map{
    case "\"" => "\""
    case "'" => "\'"
    case "\\" => "\\"
    case "/" => "/"
    case "b" => "\b"
    case "f" => "\f"
    case "n" => "\n"
    case "r" => "\r"
    case "t" => "\t"
  }
  def escape1(implicit p: P[?]): P[String] = P( "\\u" ~~ CharIn("0-9a-fA-F").repX(min=4, max=4).! ).map{
    s => Integer.parseInt(s, 16).toChar.toString
  }
  def doubleString(implicit p: P[?]): P[Seq[String]] =
    P( (CharsWhile(x => x != '"' && x != '\\').! | escape).repX ~~ "\"" )
  def singleString(implicit p: P[?]): P[Seq[String]] =
    P( (CharsWhile(x => x != '\'' && x != '\\').! | escape).repX ~~ "'" )
  def literalDoubleString(implicit p: P[?]): P[Seq[String]] =
    P( (CharsWhile(_ != '"').! | "\"\"".!.map(_ => "\"")).repX ~~ "\""  )
  def literalSingleString(implicit p: P[?]): P[Seq[String]] =
    P( (CharsWhile(_ != '\'').! | "''".!.map(_ => "'")).repX ~~ "'" )

  def tripleBarStringLines(implicit p: P[?]): P[Seq[String]] = P(
    tripleBarStringHead.flatMapX { case (pre, w, head) =>
      tripleBarStringBody(w).map(pre ++ Seq(head, "\n") ++ _)
    }
  )

  def maybeChompedTripleBarString(implicit p: P[?]): P[Seq[String]] = tripleBarString.map{
    case (true, lines) =>
      Seq(lines.mkString.stripLineEnd)
    case (false, lines) =>
      lines
  }

  def tripleBarString(implicit p: P[?]): P[(Boolean, Seq[String])] = P(
    ("||-" | "||").?.!.map(_.last == '-')./ ~~ CharsWhileIn(" \t", 0) ~~
    "\n" ~~ tripleBarStringLines ~~ "\n" ~~
    CharsWhileIn(" \t", 0) ~~ "|||"
  )

  def string(implicit p: P[?]): P[String] = P(
    SingleChar.flatMapX{
      case '\"' => doubleString
      case '\'' => singleString
      case '@' => SingleChar./.flatMapX{
        case '\"' => literalDoubleString
        case '\'' => literalSingleString
        case _ => Fail
      }
      case '|' => maybeChompedTripleBarString
      case _ => Fail
    }
  ).map(_.mkString)

  def tripleBarStringHead(implicit p: P[?]): P[(Seq[String], String, String)] = P(
    (CharsWhileIn(" \t", 0) ~~ "\n".!).repX ~~
      CharsWhileIn(" \t", 1).! ~~
      CharsWhile(_ != '\n').!
  )
  def tripleBarBlankHead(implicit p: P[?]): P[String] =
    P( CharsWhileIn(" \t", 0) ~~ &("\n").map(_ => "\n") )

  def tripleBarBlank(implicit p: P[?]): P[String] = P( "\n" ~~ tripleBarBlankHead )

  def tripleBarStringBody(w: String)(implicit p: P[?]): P[Seq[String]] = P(
    (tripleBarBlank | "\n" ~~ w ~~ CharsWhile(_ != '\n').!.map(_ + "\n")).repX
  )


  def arr(implicit p: P[?]): P[Expr] = P( (Pos ~~ &("]")).map(new Val.Arr(_, emptyLazyArray)) | arrBody )
  def compSuffix(implicit p: P[?]): P[Left[(Expr.ForSpec, Seq[Expr.CompSpec]),Nothing]] = P( forspec ~ compspec ).map(Left(_))
  def arrBody(implicit p: P[?]): P[Expr] = P(
    Pos ~~ expr ~
    (compSuffix | "," ~ (compSuffix | (expr.rep(0, sep = ",") ~ ",".?).map(Right(_)))).?
  ).map{
    case (offset, first: Val, None) => new Val.Arr(offset, Array(first))
    case (offset, first, None) => Expr.Arr(offset, Array(first))
    case (offset, first, Some(Left(comp))) => Expr.Comp(offset, first, comp._1, comp._2.toArray)
    case (offset, first: Val, Some(Right(rest))) if rest.forall(_.isInstanceOf[Val]) =>
      val a = new Array[Lazy](rest.length + 1)
      a(0) = first
      var i = 1
      rest.foreach { v =>
        a(i) = v.asInstanceOf[Val]
        i += 1
      }
      new Val.Arr(offset, a)
    case (offset, first, Some(Right(rest))) => Expr.Arr(offset, Array(first) ++ rest)
  }

  def assertExpr(pos: Position)(implicit p: P[?]): P[Expr] =
    P( assertStmt ~ ";" ~ expr ).map(t => Expr.AssertExpr(pos, t._1, t._2))

  def function(pos: Position)(implicit p: P[?]): P[Expr] =
    P( "(" ~/ params ~ ")" ~ expr ).map(t => Expr.Function(pos, t._1, t._2))

  def ifElse(pos: Position)(implicit p: P[?]): P[Expr] =
    P( Pos ~~ expr ~ "then" ~~ break ~ expr ~ ("else" ~~ break ~ expr).?.map(_.getOrElse(null)) ).map{case (pos, cond, t, e) =>  Expr.IfElse(pos, cond, t, e)}

  def localExpr(implicit p: P[?]): P[Expr] =
    P( Pos ~~ bind.rep(min=1, sep = ","./).map(s => if(s.isEmpty) null else s.toArray) ~ ";" ~ expr ).map{case (pos, bind, ret) => Expr.LocalExpr(pos, bind, ret)}

  def expr(implicit p: P[?]): P[Expr] =
    P("" ~ expr1 ~ (Pos ~~ binaryop ~/ expr1).rep ~ "").map{ case (pre, fs) =>
      var remaining = fs
      def climb(minPrec: Int, current: Expr): Expr = {
        var result = current
        while(
          remaining.headOption match{
            case None => false
            case Some((offset, op, next)) =>
              val prec: Int = precedence(op)
              if (prec < minPrec) false
              else{
                remaining = remaining.tail
                val rhs = climb(prec + 1, next)
                val op1 = op match{
                  case "*" => Expr.BinaryOp.OP_*
                  case "/" => Expr.BinaryOp.OP_/
                  case "%" => Expr.BinaryOp.OP_%
                  case "+" => Expr.BinaryOp.OP_+
                  case "-" => Expr.BinaryOp.OP_-
                  case "<<" => Expr.BinaryOp.OP_<<
                  case ">>" => Expr.BinaryOp.OP_>>
                  case "<" => Expr.BinaryOp.OP_<
                  case ">" => Expr.BinaryOp.OP_>
                  case "<=" => Expr.BinaryOp.OP_<=
                  case ">=" => Expr.BinaryOp.OP_>=
                  case "in" => Expr.BinaryOp.OP_in
                  case "==" => Expr.BinaryOp.OP_==
                  case "!=" => Expr.BinaryOp.OP_!=
                  case "&" => Expr.BinaryOp.OP_&
                  case "^" => Expr.BinaryOp.OP_^
                  case "|" => Expr.BinaryOp.OP_|
                  case "&&" => Expr.BinaryOp.OP_&&
                  case "||" => Expr.BinaryOp.OP_||
                }
                result = op1 match {
                  case Expr.BinaryOp.OP_&& => Expr.And(offset, result, rhs)
                  case Expr.BinaryOp.OP_|| => Expr.Or(offset, result, rhs)
                  case _ => Expr.BinaryOp(offset, result, op1, rhs)
                }
                true
              }
          }
        )()
        result
      }

      climb(0, pre)
    }

  def expr1(implicit p: P[?]): P[Expr] = P(expr2 ~ exprSuffix2.rep).map{
    case (pre, fs) => fs.foldLeft(pre){case (p, f) => f(p) }
  }

  def exprSuffix2(implicit p: P[?]): P[Expr => Expr] = P(
    Pos.flatMapX{i =>
      CharIn(".[({")./.!.map(_(0)).flatMapX{ c =>
        (c: @switch) match{
          case '.' => Pass ~ id.map(x => Expr.Select(i, _: Expr, x))
          case '[' => Pass ~ (expr.? ~ (":" ~ expr.?).rep ~ "]").map{
            case (Some(tree), Seq()) => Expr.Lookup(i, _: Expr, tree)
            case (start, ins) => Expr.Slice(i, _: Expr, start, ins.lift(0).flatten, ins.lift(1).flatten)
          }
          case '(' => Pass ~ (args ~ ")" ~ "tailstrict".!.?).map {
            case (args, namedNames, tailstrict) => Expr.Apply(i, _: Expr, args, if(namedNames.length == 0) null else namedNames, tailstrict.nonEmpty)
          }
          case '{' => Pass ~ (objinside ~ "}").map(x => Expr.ObjExtend(i, _: Expr, x))
          case _ => Fail
        }
      }
    }
  )

  def local(implicit p: P[?]): P[Expr] = P( localExpr )
  def importStr(pos: Position)(implicit p: P[?]): P[Expr.ImportStr] = P( importExpr.map(Expr.ImportStr(pos, _)) )
  def importBin(pos: Position)(implicit p: P[?]): P[Expr.ImportBin] = P( importExpr.map(Expr.ImportBin(pos, _)) )
  def `import`(pos: Position)(implicit p: P[?]): P[Expr.Import] = P( importExpr.map(Expr.Import(pos, _)) )
  def error(pos: Position)(implicit p: P[?]): P[Expr.Error] = P(expr.map(Expr.Error(pos, _)) )

  def importExpr(implicit p: P[?]): P[String] = P(
    if (!strictImportSyntax) string
    else expr.flatMap {
      case Val.Str(_, s) => Pass(s)
      case _ => Fail.opaque("string literal (computed imports are not allowed)")
    }
  )

  def unaryOpExpr(pos: Position, op: Char)(implicit p: P[?]): P[Expr.UnaryOp] = P(
    expr1.map{ e =>
      def k2 = op match{
        case '+' => Expr.UnaryOp.OP_+
        case '-' => Expr.UnaryOp.OP_-
        case '~' => Expr.UnaryOp.OP_~
        case '!' => Expr.UnaryOp.OP_!
      }
      Expr.UnaryOp(pos, k2, e)
    }
  )

  def constructString(pos: Position, lines: Seq[String]): Val.Str = {
    val s = lines.mkString
    val unique = internedStrings.getOrElseUpdate(s, s)
    Val.Str(pos, unique)
  }

  // Any `expr` that isn't naively left-recursive
  def expr2(implicit p: P[?]): P[Expr] = P(
    Pos.flatMapX{ pos =>
      SingleChar.flatMapX{ c =>
        c match {
          case '{' => Pass ~ objinside ~ "}"
          case '+' | '-' | '~' | '!' => Pass ~ unaryOpExpr(pos, c)
          case '[' => Pass ~ arr ~ "]"
          case '(' => Pass ~ expr ~ ")"
          case '\"' => doubleString.map(constructString(pos, _))
          case '\'' => singleString.map(constructString(pos, _))
          case '@' => SingleChar./.flatMapX{
            case '\"' => literalDoubleString.map(constructString(pos, _))
            case '\'' => literalSingleString.map(constructString(pos, _))
            case _ => Fail
          }
          case '|' => maybeChompedTripleBarString.map(constructString(pos, _))
          case '$' => Pass(Expr.$(pos))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            P.current.index = pos.offset; number
          case x if idStartChar(x) => CharsWhileIn("_a-zA-Z0-9", 0).!.flatMapX { y =>
            "" + x + y match {
              case "null"      => Pass(Val.Null(pos))
              case "true"      => Pass(Val.True(pos))
              case "false"     => Pass(Val.False(pos))
              case "self"      => Pass(Expr.Self(pos))
              case "super"     => Pass(Expr.Super(pos))
              case "if"        => Pass ~ ifElse(pos)
              case "function"  => Pass ~ function(pos)
              case "importstr" => Pass ~ importStr(pos)
              case "importbin" => Pass ~ importBin(pos)
              case "import"    => Pass ~ `import`(pos)
              case "error"     => Pass ~ error(pos)
              case "assert"    => Pass ~ assertExpr(pos)
              case "local"     => Pass ~ local
              case x           => Pass(Expr.Id(pos, x))
            }
          }
          case _ => Fail
        }
      }
    }
  )

  def objinside(implicit p: P[?]): P[Expr.ObjBody] = P(
    Pos ~ member.rep(sep = ",") ~ ",".? ~ (forspec ~ compspec).?
  ).flatMap { case t @ (pos, exprs, _) =>
    val seen = collection.mutable.Set.empty[String]
    var overlap: String = null
    exprs.foreach {
      case Expr.Member.Field(_, Expr.FieldName.Fixed(n), _, _, _, _) =>
        if(seen(n)) overlap = n
        else seen.add(n)
      case _ =>
    }
    if (overlap == null) Pass(t)
    else Fail.opaque("no duplicate field: " + overlap)
  }.map{
    case (pos, exprs, None) =>
      val binds = {
        val b = exprs.iterator.filter(_.isInstanceOf[Expr.Bind]).asInstanceOf[Iterator[Expr.Bind]].toArray
        val seen = collection.mutable.Set.empty[String]
        var overlap: String = null
        b.foreach {
          case Expr.Bind(_, n, _, _) =>
            if (seen(n)) overlap = n
            else seen.add(n)
          case null =>
        }
        if (overlap != null) Fail.opaque("no duplicate local: " + overlap)
        if(b.isEmpty) null else b
      }
      val fields = exprs.iterator.filter(_.isInstanceOf[Expr.Member.Field]).asInstanceOf[Iterator[Expr.Member.Field]].toArray
      val asserts = {
        val a = exprs.iterator.filter(_.isInstanceOf[Expr.Member.AssertStmt]).asInstanceOf[Iterator[Expr.Member.AssertStmt]].toArray
        if(a.isEmpty) null else a
      }
      if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields, internedStaticFieldSets, internedStrings)
      else Expr.ObjBody.MemberList(pos, binds, fields, asserts)
    case (pos, exprs, Some(comps)) =>
      val preLocals = exprs
        .takeWhile(_.isInstanceOf[Expr.Bind])
        .map(_.asInstanceOf[Expr.Bind])
      val Expr.Member.Field(offset, Expr.FieldName.Dyn(lhs), plus, null, Visibility.Normal, rhs) =
        exprs(preLocals.length)
      val postLocals = exprs.drop(preLocals.length+1).takeWhile(_.isInstanceOf[Expr.Bind])
        .map(_.asInstanceOf[Expr.Bind])
      
      /* 
       * Prevent duplicate fields in list comprehension. See: https://github.com/databricks/sjsonnet/issues/99
       * 
       * If comps._1 is a forspec with value greater than one lhs cannot be a Expr.Str
       * Otherwise the field value will be overriden by the multiple iterations of forspec
       */
      (lhs, comps) match {
        case (Val.Str(_, _), (Expr.ForSpec(_, _, Expr.Arr(_, values)), _)) if values.length > 1 =>
          Fail.opaque(s"""no duplicate field: "${lhs.asInstanceOf[Val.Str].value}" """)
        case (Val.Str(_, _), (Expr.ForSpec(_, _, arr: Val.Arr), _)) if arr.length > 1 =>
          Fail.opaque(s"""no duplicate field: "${lhs.asInstanceOf[Val.Str].value}" """)
        case _ => // do nothing
      }
      Expr.ObjBody.ObjComp(pos, preLocals.toArray, lhs, rhs, plus, postLocals.toArray, comps._1, comps._2.toList)
  }

  def member(implicit p: P[?]): P[Expr.Member] = P( objlocal | "assert" ~~ break ~ assertStmt | field )
  def field(implicit p: P[?]): P[Expr.Member.Field] = P(
    (Pos ~~ fieldname ~/ "+".!.? ~ ("(" ~ params ~ ")").? ~ fieldKeySep ~/ expr).map{
      case (pos, name, plus, p, h2, e) =>
        Expr.Member.Field(pos, name, plus.nonEmpty, p.getOrElse(null), h2, e)
    }
  )
  def fieldKeySep(implicit p: P[?]): P[Visibility] = P( StringIn(":::", "::", ":") ).!.map{
    case ":" => Visibility.Normal
    case "::" => Visibility.Hidden
    case ":::" => Visibility.Unhide
  }
  def objlocal(implicit p: P[?]): P[Expr.Bind] = P( "local" ~~ break ~/ bind )
  def compspec(implicit p: P[?]): P[Seq[Expr.CompSpec]] = P( (forspec | ifspec).rep )
  def forspec(implicit p: P[?]): P[Expr.ForSpec] =
    P( Pos ~~ "for" ~~ break ~/ id ~ "in" ~~ break ~ expr ).map{case (pos, name, cond) => Expr.ForSpec(pos, name, cond) }
  def ifspec(implicit p: P[?]): P[Expr.IfSpec] = P( Pos ~~ "if" ~~ break  ~/ expr ).map{case (pos, cond) => Expr.IfSpec(pos, cond) }
  def fieldname(implicit p: P[?]): P[Expr.FieldName] = P(
    id.map(Expr.FieldName.Fixed.apply) |
    string.map(Expr.FieldName.Fixed.apply) |
    "[" ~ expr.map(Expr.FieldName.Dyn.apply) ~ "]"
  )
  def assertStmt(implicit p: P[?]): P[Expr.Member.AssertStmt] =
    P( expr ~ (":" ~ expr).?.map(_.getOrElse(null)) ).map{case (value, msg) => Expr.Member.AssertStmt(value, msg) }

  def bind(implicit p: P[?]): P[Expr.Bind] =
    P( Pos ~~ id ~ ("(" ~/ params.? ~ ")").?.map(_.flatten).map(_.getOrElse(null)) ~ "=" ~ expr ).map{case (pos, name, args, rhs )=> Expr.Bind(pos, name, args, rhs)}

  def args(implicit p: P[?]): P[(Array[Expr], Array[String])] = P( ((id ~ "=" ~ !"=").? ~ expr).rep(sep = ",") ~ ",".? ).flatMapX{ x =>
    if (x.sliding(2).exists{case Seq(l, r) => l._1.isDefined && r._1.isEmpty case _ => false}) {
      Fail.opaque("no positional params after named params")
    } else {
      val args = x.iterator.map(_._2).toArray
      val namedNames = x.iterator.dropWhile(_._1.isEmpty).map(_._1.get).toArray
      Pass((args, namedNames))
    }
  }

  def params(implicit p: P[?]): P[Expr.Params] = P( (id ~ ("=" ~ expr).?).rep(sep = ",") ~ ",".? ).flatMapX{ x =>
    val seen = collection.mutable.Set.empty[String]
    var overlap: String = null
    for((k, v) <- x){
      if (seen(k)) overlap = k
      else seen.add(k)
    }
    if (overlap == null) {
      val names = x.map(_._1).toArray[String]
      val exprs = x.map(_._2.getOrElse(null)).toArray[Expr]
      Pass(Expr.Params(names, exprs))
    }
    else Fail.opaque("no duplicate parameter: " + overlap)

  }

  def binaryop(implicit p: P[?]): P[String] = P(
    StringIn(
      "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
      "*", "/", "%", "+", "-", "<", ">", "&", "^", "|"
    )

  ).!

  def document(implicit p: P[?]): P[(Expr, FileScope)] = P( expr ~  Pass(fileScope) ~ End )
}

final class Position(val fileScope: FileScope, val offset: Int) {
  def currentFile = fileScope.currentFile
  def noOffset = fileScope.noOffsetPos
  override def equals(o: Any): Boolean = o match {
    case o: Position => currentFile == o.currentFile && offset == o.offset
    case _ => false
  }
  override def toString: String = {
    val name = if(fileScope == null) "null" else fileScope.currentFileLastPathElement
    s"Position($name, $offset)"
  }
}

/**
  * FileScope models the per-file context that is propagated throughout the
  * evaluation of a single Jsonnet file. Contains the current file path.
  */
class FileScope(val currentFile: Path) {
  lazy val currentFileLastPathElement: String = if(currentFile == null) null else currentFile.last
  val noOffsetPos: Position = new Position(this, -1)
}
