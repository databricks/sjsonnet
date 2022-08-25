package sjsonnet

/*-
 * Changed:
 * - 7187ded408ff8ea68822297079549c1c323bcdee: implement support for ?? binary op
 * - 80f58d4e2d5e4d4ea94ef828962ef5d8cba1a625: implements support for safe select operator ?.
 * - b78178f48a289622e65d5d0d2416898b7dadddbc: avoid string comparison for null safe select
 * - enable indexing objects with a string
 */

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
  val precedenceTable = Seq(
    Seq("*", "/", "%"),
    Seq("+", "-"),
    Seq("<<", ">>"),
    Seq("<", ">", "<=", ">=", "in"),
    Seq("==", "!="),
    Seq("&"),
    Seq("^"),
    Seq("|"),
    Seq("&&"),
    Seq("||"),
    Seq("??")
  )

  val precedence = precedenceTable
    .reverse
    .zipWithIndex
    .flatMap{case (ops, idx) => ops.map(_ -> idx)}
    .toMap

  val keywords = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true"
  )

  def idStartChar(c: Char) = c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private val emptyLazyArray = new Array[Lazy](0)
}

class Parser(val currentFile: Path,
             strictImportSyntax: Boolean) {
  import Parser._

  private val fileScope = new FileScope(currentFile)

  private val strings = new mutable.HashMap[String, String]

  def Pos[_: P]: P[Position] = Index.map(offset => new Position(fileScope, offset))

  def id[_: P] = P(
    CharIn("_a-zA-Z") ~~
    CharsWhileIn("_a-zA-Z0-9", 0)
  ).!.filter(s => !keywords.contains(s)).opaque("identifier")

  def break[_: P] = P(!CharIn("_a-zA-Z0-9"))
  def number[_: P]: P[Val.Num] = P(
    Pos ~~ (
      CharsWhileIn("0-9") ~~
      ("." ~ CharsWhileIn("0-9")).? ~~
      (CharIn("eE") ~ CharIn("+\\-").? ~~ CharsWhileIn("0-9")).?
    ).!
  ).map(s => Val.Num(s._1, s._2.toDouble))

  def escape[_: P] = P( escape0 | escape1 )
  def escape0[_: P] = P("\\" ~~ !"u" ~~ AnyChar.!).map{
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
  def escape1[_: P] = P( "\\u" ~~ CharIn("0-9a-fA-F").repX(min=4, max=4).! ).map{
    s => Integer.parseInt(s, 16).toChar.toString
  }
  def doubleString[_: P]: P[Seq[String]] =
    P( (CharsWhile(x => x != '"' && x != '\\').! | escape).repX ~~ "\"" )
  def singleString[_: P]: P[Seq[String]] =
    P( (CharsWhile(x => x != '\'' && x != '\\').! | escape).repX ~~ "'" )
  def literalDoubleString[_: P]: P[Seq[String]] =
    P( (CharsWhile(_ != '"').! | "\"\"".!.map(_ => "\"")).repX ~~ "\""  )
  def literalSingleString[_: P]: P[Seq[String]] =
    P( (CharsWhile(_ != '\'').! | "''".!.map(_ => "'")).repX ~~ "'" )

  def tripleBarStringLines[_: P]: P[Seq[String]] = P(
    tripleBarStringHead.flatMapX { case (pre, w, head) =>
      tripleBarStringBody(w).map(pre ++ Seq(head, "\n") ++ _)
    }
  )
  def tripleBarString[_: P]: P[Seq[String]] = P(
    "||"./ ~~ CharsWhileIn(" \t", 0) ~~
    "\n" ~~ tripleBarStringLines ~~ "\n" ~~
    CharsWhileIn(" \t", 0) ~~ "|||"
  )
  def string[_: P]: P[String] = P(
    SingleChar.flatMapX{
      case '\"' => doubleString
      case '\'' => singleString
      case '@' => SingleChar./.flatMapX{
        case '\"' => literalDoubleString
        case '\'' => literalSingleString
        case _ => Fail
      }
      case '|' => tripleBarString
      case _ => Fail
    }
  ).map(_.mkString)

  def tripleBarStringHead[_: P] = P(
    (CharsWhileIn(" \t", 0) ~~ "\n".!).repX ~~
      CharsWhileIn(" \t", 1).! ~~
      CharsWhile(_ != '\n').!
  )
  def tripleBarBlankHead[_: P]: P[String] =
    P( CharsWhileIn(" \t", 0) ~~ &("\n").map(_ => "\n") )

  def tripleBarBlank[_: P]: P[String] = P( "\n" ~~ tripleBarBlankHead )

  def tripleBarStringBody[_: P](w: String): P[Seq[String]] = P(
    (tripleBarBlank | "\n" ~~ w ~~ CharsWhile(_ != '\n').!.map(_ + "\n")).repX
  )


  def arr[_: P]: P[Expr] = P( (Pos ~~ &("]")).map(new Val.Arr(_, emptyLazyArray)) | arrBody )
  def compSuffix[_: P] = P( forspec ~ compspec ).map(Left(_))
  def arrBody[_: P]: P[Expr] = P(
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

  def assertExpr[_: P](pos: Position): P[Expr] =
    P( assertStmt ~ ";" ~ expr ).map(t => Expr.AssertExpr(pos, t._1, t._2))

  def function[_: P](pos: Position): P[Expr] =
    P( "(" ~/ params ~ ")" ~ expr ).map(t => Expr.Function(pos, t._1, t._2))

  def ifElse[_: P](pos: Position): P[Expr] =
    P( Pos ~~ expr ~ "then" ~~ break ~ expr ~ ("else" ~~ break ~ expr).?.map(_.getOrElse(null)) ).map(Expr.IfElse.tupled)

  def localExpr[_: P]: P[Expr] =
    P( Pos ~~ bind.rep(min=1, sep = ","./).map(s => if(s.isEmpty) null else s.toArray) ~ ";" ~ expr ).map(Expr.LocalExpr.tupled)

  def expr[_: P]: P[Expr] =
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
                  case "??" => Expr.BinaryOp.OP_??
                }
                result = op1 match {
                  case Expr.BinaryOp.OP_&& => Expr.And(offset, result, rhs)
                  case Expr.BinaryOp.OP_|| => Expr.Or(offset, result, rhs)
                  case Expr.BinaryOp.OP_?? => Expr.NullCoal(offset, result, rhs)
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

  def expr1[_: P]: P[Expr] = P(expr2 ~ exprSuffix2.rep).map{
    case (pre, fs) => fs.foldLeft(pre){case (p, f) => f(p) }
  }

  def exprSuffix2[_: P]: P[Expr => Expr] = P(
    Pos.flatMapX { i =>
      (CharIn(".[({") | StringIn("?."))./.!.map(_ (0)).flatMapX { c =>
        (c: @switch) match {
          case '.' => Pass ~ (id.map(x => Expr.Select(i, _: Expr, x)) | string.map(x => Expr.Select(i, _: Expr, x)))
          case '?' => Pass ~ (id.map(x => Expr.Select(i, _: Expr, x, safe = true)) | string.map(x => Expr.Select(i, _: Expr, x, safe = true)))
          case '[' => Pass ~ (expr.? ~ (":" ~ expr.?).rep ~ "]").map {
            case (Some(tree), Seq()) => Expr.Lookup(i, _: Expr, tree)
            case (start, ins) => Expr.Slice(i, _: Expr, start, ins.lift(0).flatten, ins.lift(1).flatten)
          }
          case '(' => Pass ~ (args ~ ")").map { case (args, namedNames) =>
            Expr.Apply(i, _: Expr, args, if (namedNames.length == 0) null else namedNames)
          }
          case '{' => Pass ~ (objinside ~ "}").map(x => Expr.ObjExtend(i, _: Expr, x))
          case _ => Fail
        }
      }
    }
  )

  def local[_: P] = P( localExpr )
  def importStr[_: P](pos: Position) = P( importExpr.map(Expr.ImportStr(pos, _)) )
  def `import`[_: P](pos: Position) = P( importExpr.map(Expr.Import(pos, _)) )
  def error[_: P](pos: Position) = P(expr.map(Expr.Error(pos, _)) )

  def importExpr[_: P]: P[String] = P(
    if (!strictImportSyntax) string
    else expr.flatMap {
      case Val.Str(_, s) => Pass(s)
      case _ => Fail.opaque("string literal (computed imports are not allowed)")
    }
  )

  def unaryOpExpr[_: P](pos: Position, op: Char) = P(
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

  def constructString(pos: Position, lines: Seq[String]) = {
    val s = lines.mkString
    val unique = strings.getOrElseUpdate(s, s)
    Val.Str(pos, unique)
  }

  // Any `expr` that isn't naively left-recursive
  def expr2[_: P]: P[Expr] = P(
    Pos.flatMapX{ pos =>
      SingleChar.flatMapX{ c =>
        (c: @switch) match {
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
          case '|' => tripleBarString.map(constructString(pos, _))
          case '$' => Pass(Expr.$(pos))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            P.current.index = pos.offset; number
          case x if idStartChar(x) => CharsWhileIn("_a-zA-Z0-9", 0).!.flatMapX { y =>
            x + y match {
              case "null"      => Pass(Val.Null(pos))
              case "true"      => Pass(Val.True(pos))
              case "false"     => Pass(Val.False(pos))
              case "self"      => Pass(Expr.Self(pos))
              case "super"     => Pass(Expr.Super(pos))
              case "if"        => Pass ~ ifElse(pos)
              case "function"  => Pass ~ function(pos)
              case "importstr" => Pass ~ importStr(pos)
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

  def objinside[_: P]: P[Expr.ObjBody] = P(
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
        if(b.isEmpty) null else b
      }
      val fields = exprs.iterator.filter(_.isInstanceOf[Expr.Member.Field]).asInstanceOf[Iterator[Expr.Member.Field]].toArray
      val asserts = {
        val a = exprs.iterator.filter(_.isInstanceOf[Expr.Member.AssertStmt]).asInstanceOf[Iterator[Expr.Member.AssertStmt]].toArray
        if(a.isEmpty) null else a
      }
      if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields)
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

  def member[_: P]: P[Expr.Member] = P( objlocal | "assert" ~~ break ~ assertStmt | field )
  def field[_: P] = P(
    (Pos ~~ fieldname ~/ "+".!.? ~ ("(" ~ params ~ ")").? ~ fieldKeySep ~/ expr).map{
      case (pos, name, plus, p, h2, e) =>
        Expr.Member.Field(pos, name, plus.nonEmpty, p.getOrElse(null), h2, e)
    }
  )
  def fieldKeySep[_: P] = P( StringIn(":::", "::", ":") ).!.map{
    case ":" => Visibility.Normal
    case "::" => Visibility.Hidden
    case ":::" => Visibility.Unhide
  }
  def objlocal[_: P] = P( "local" ~~ break ~/ bind )
  def compspec[_: P]: P[Seq[Expr.CompSpec]] = P( (forspec | ifspec).rep )
  def forspec[_: P] =
    P( Pos ~~ "for" ~~ break ~/ id ~ "in" ~~ break ~ expr ).map(Expr.ForSpec.tupled)
  def ifspec[_: P] = P( Pos ~~ "if" ~~ break  ~/ expr ).map(Expr.IfSpec.tupled)
  def fieldname[_: P] = P(
    id.map(Expr.FieldName.Fixed) |
    string.map(Expr.FieldName.Fixed) |
    "[" ~ expr.map(Expr.FieldName.Dyn) ~ "]"
  )
  def assertStmt[_: P] =
    P( expr ~ (":" ~ expr).?.map(_.getOrElse(null)) ).map(Expr.Member.AssertStmt.tupled)

  def bind[_: P] =
    P( Pos ~~ id ~ ("(" ~/ params.? ~ ")").?.map(_.flatten).map(_.getOrElse(null)) ~ "=" ~ expr ).map(Expr.Bind.tupled)

  def args[_: P] = P( ((id ~ "=" ~ !"=").? ~ expr).rep(sep = ",") ~ ",".? ).flatMapX{ x =>
    if (x.sliding(2).exists{case Seq(l, r) => l._1.isDefined && r._1.isEmpty case _ => false}) {
      Fail.opaque("no positional params after named params")
    } else {
      val args = x.iterator.map(_._2).toArray
      val namedNames = x.iterator.dropWhile(_._1.isEmpty).map(_._1.get).toArray
      Pass((args, namedNames))
    }
  }

  def params[_: P]: P[Expr.Params] = P( (id ~ ("=" ~ expr).?).rep(sep = ",") ~ ",".? ).flatMapX{ x =>
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

  def binaryop[_: P] = P(
    StringIn(
      "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
      "*", "/", "%", "+", "-", "<", ">", "&", "^", "|", "??"
    )

  ).!

  def document[_: P]: P[(Expr, FileScope)] = P( expr ~  Pass(fileScope) ~ End )
}

final class Position(val fileScope: FileScope, val offset: Int) {
  def currentFile = fileScope.currentFile
  def noOffset = fileScope.noOffsetPos
  override def equals(o: Any) = o match {
    case o: Position => currentFile == o.currentFile && offset == o.offset
    case _ => false
  }
  override def toString = {
    val name = if(fileScope == null) "null" else fileScope.currentFileLastPathElement
    s"Position($name, $offset)"
  }
}

/**
  * FileScope models the per-file context that is propagated throughout the
  * evaluation of a single Jsonnet file. Contains the current file path.
  */
class FileScope(val currentFile: Path) {
  lazy val currentFileLastPathElement = if(currentFile == null) null else currentFile.last
  val noOffsetPos: Position = new Position(this, -1)
}
