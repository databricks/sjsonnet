package sjsonnet

import fastparse.JsonnetWhitespace._
import fastparse._
import Expr.Member.Visibility

import scala.annotation.switch
import scala.collection.mutable

/**
 * Parses Jsonnet source code `String`s into a [[Expr]] syntax tree, using the FastParse parsing
 * library. Uses precedence climbing to handle infix operators, and resolves local variable names to
 * array indices during parsing to allow better performance at runtime.
 */

object Parser {
  private def precedence(op: String): Int = {
    if (op.length == 1) {
      (op.charAt(0): @switch) match {
        case '|'             => 2
        case '^'             => 3
        case '&'             => 4
        case '<' | '>'       => 6
        case '+' | '-'       => 8
        case '*' | '/' | '%' => 9
        case _               => throw new IllegalArgumentException("Unknown operator: " + op)
      }
      // here we keep all the origin precedences
    } else
      op match {
        case "||"                           => 0
        case "&&"                           => 1
        case "|"                            => 2
        case "^"                            => 3
        case "&"                            => 4
        case "==" | "!="                    => 5
        case "<" | ">" | "<=" | ">=" | "in" => 6
        case "<<" | ">>"                    => 7
        case "+" | "-"                      => 8
        case "*" | "/" | "%"                => 9
        case _ => throw new IllegalArgumentException("Unknown operator: " + op)
      }
  }

  val keywords: Set[String] = Set(
    "assert",
    "else",
    "error",
    "false",
    "for",
    "function",
    "if",
    "import",
    "importstr",
    "in",
    "local",
    "null",
    "tailstrict",
    "then",
    "self",
    "super",
    "true",
    "importbin"
  )

  def idStartChar(c: Char): Boolean = c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private val emptyLazyArray = new Array[Lazy](0)
  private val isSpaceOrTab: Char => Boolean = c => c == ' ' || c == '\t'
}

class Parser(
    val currentFile: Path,
    internedStrings: mutable.HashMap[String, String],
    internedStaticFieldSets: mutable.HashMap[
      Val.StaticObjectFieldSet,
      java.util.LinkedHashMap[String, java.lang.Boolean]
    ]) {
  import Parser._

  private val fileScope = new FileScope(currentFile)

  def Pos[$: P]: P[Position] = Index.map(offset => new Position(fileScope, offset))

  def id[$: P]: P[String] = P(
    CharIn("_a-zA-Z") ~~
    CharsWhileIn("_a-zA-Z0-9", 0)
  ).!.filter(s => !keywords.contains(s))

  def break[$: P]: P[Unit] = P(!CharIn("_a-zA-Z0-9"))
  def number[$: P]: P[Val.Num] = P(
    Pos ~~ (
      CharsWhileIn("0-9") ~~
      ("." ~ CharsWhileIn("0-9")).? ~~
      (CharIn("eE") ~ CharIn("+\\-").? ~~ CharsWhileIn("0-9")).?
    ).!
  ).flatMapX(s => {
    if (s._2.length > 1 && Character.isDigit(s._2.charAt(1)) && s._2.charAt(0) == '0') {
      Fail.opaque("numbers cannot start with a 0 digit")
    } else {
      Pass(Val.Num(s._1, s._2.toDouble))
    }
  })

  def escape[$: P]: P[String] = P(escape0 | escape1)
  def escape0[$: P]: P[String] = P("\\" ~~ !"u" ~~ AnyChar.!).flatMapX {
    case "\"" => Pass("\"")
    case "'"  => Pass("\'")
    case "\\" => Pass("\\")
    case "/"  => Pass("/")
    case "b"  => Pass("\b")
    case "f"  => Pass("\f")
    case "n"  => Pass("\n")
    case "r"  => Pass("\r")
    case "t"  => Pass("\t")
    case s    => Fail.opaque(f"Unknown escape sequence in string literal: $s")
  }
  def escape1[$: P]: P[String] = P("\\u" ~~ CharIn("0-9a-fA-F").repX(min = 4, max = 4).!).map { s =>
    Integer.parseInt(s, 16).toChar.toString
  }
  def doubleString[$: P]: P[Seq[String]] =
    P((CharsWhile(x => x != '"' && x != '\\').! | escape).repX ~~ "\"")
  def singleString[$: P]: P[Seq[String]] =
    P((CharsWhile(x => x != '\'' && x != '\\').! | escape).repX ~~ "'")
  def literalDoubleString[$: P]: P[Seq[String]] =
    P((CharsWhile(_ != '"').! | "\"\"".!.map(_ => "\"")).repX ~~ "\"")
  def literalSingleString[$: P]: P[Seq[String]] =
    P((CharsWhile(_ != '\'').! | "''".!.map(_ => "'")).repX ~~ "'")

  def string[$: P]: P[String] = P(
    SingleChar.flatMapX {
      case '\"' => doubleString
      case '\'' => singleString
      case '@'  =>
        SingleChar./.flatMapX {
          case '\"' => literalDoubleString
          case '\'' => literalSingleString
          case _    => Fail
        }
      case '|' => maybeChompedTripleBarString
      case _   => Fail
    }
  ).map(_.mkString)

  def maybeChompedTripleBarString[$: P]: P[Seq[String]] = tripleBarString.map {
    case (true, lines) =>
      lines.dropRight(1) ++ Seq(lines.last.stripLineEnd)
    case (false, lines) =>
      lines
  }

  def tripleBarJunk[$: P]: P[Unit] = CharsWhile(isSpaceOrTab, 0)

  def tripleBarIndent[$: P]: P[String] = P(
    CharsWhile(isSpaceOrTab, 1)
      .opaque("|||-block line must either be an empty line or start with at least one whitespace")
      .!
  )

  def tripleBarString[$: P]: P[(Boolean, Seq[String])] = P(
    ("||-" | "||").!.map(
      _.last == '-'
    )./ ~~ tripleBarJunk ~~ tripleBarStringLines ~~ tripleBarJunk ~~ "|||"
  )

  def tripleBarStringLines[$: P]: P[Seq[String]] = P(
    // First, we skip an empty lines until we reach the first line with indentation.
    // This will become the indentation for the rest of the block.
    ("\r\n" | "\n").opaque("|||-blocks require multiple lines").!./.flatMapX { case sep =>
      (sep.!.repX ~~ tripleBarIndent)
        .flatMapX { case (before, indent) =>
          tripleBarStringBody(indent, sep).map(before ++ _)
        }
    }
  )

  def tripleBarStringBody[$: P](indent: String, sep: String): P[Seq[String]] = P(
    // Because we already parsed the indentation, we special case the first line and only look for the content.
    (CharsWhile(!sep.contains(_), 0) ~~ sep).!.flatMapX { firstLine =>
      // That's the core of the parsing. Either we have an empty line or we have indentation + content + new line separator.
      (sep.! | (indent.! ~~ (CharsWhile(!sep.contains(_), 0) ~~ sep).!).map(_._2))
        .opaque("|||-block line must either be an empty line or start with at least one whitespace")
        .repX
        .map(Seq(firstLine) ++ _)
    }
  )

  def arr[$: P]: P[Expr] = P((Pos ~~ &("]")).map(Val.Arr(_, emptyLazyArray)) | arrBody)
  def compSuffix[$: P]: P[Left[(Expr.ForSpec, Seq[Expr.CompSpec]), Nothing]] =
    P(forspec ~ compspec).map(Left(_))
  def arrBody[$: P]: P[Expr] = P(
    Pos ~~ expr ~
    (compSuffix | "," ~ (compSuffix | (expr.rep(0, sep = ",") ~ ",".?).map(Right(_)))).?
  ).map {
    case (offset, first: Val, None)        => Val.Arr(offset, Array(first))
    case (offset, first, None)             => Expr.Arr(offset, Array(first))
    case (offset, first, Some(Left(comp))) => Expr.Comp(offset, first, comp._1, comp._2.toArray)
    case (offset, first: Val, Some(Right(rest))) if rest.forall(_.isInstanceOf[Val]) =>
      val a = new Array[Lazy](rest.length + 1)
      a(0) = first
      var i = 1
      rest.foreach { v =>
        a(i) = v.asInstanceOf[Val]
        i += 1
      }
      Val.Arr(offset, a)
    case (offset, first, Some(Right(rest))) => Expr.Arr(offset, Array(first) ++ rest)
  }

  def assertExpr[$: P](pos: Position): P[Expr] =
    P(assertStmt ~ ";" ~ expr).map(t => Expr.AssertExpr(pos, t._1, t._2))

  def function[$: P](pos: Position): P[Expr] =
    P("(" ~/ params ~ ")" ~ expr).map(t => Expr.Function(pos, t._1, t._2))

  def ifElse[$: P]: P[Expr] =
    P(Pos ~~ expr ~ "then" ~~ break ~ expr ~ ("else" ~~ break ~ expr).?.map(_.orNull))
      .map { case (pos, cond, t, e) => Expr.IfElse(pos, cond, t, e) }

  def localExpr[$: P]: P[Expr] =
    P(
      Pos ~~ bind
        .rep(min = 1, sep = ","./)
        .map(s => if (s.isEmpty) null else s.toArray) ~ ";" ~ expr
    ).map { case (pos, bind, ret) => Expr.LocalExpr(pos, bind, ret) }

  def expr[$: P]: P[Expr] =
    P("" ~ expr1 ~ (Pos ~~ binaryop ~/ expr1).rep ~ "").map { case (pre, fs) =>
      var remaining = fs
      def climb(minPrec: Int, current: Expr): Expr = {
        var result = current
        while (
          remaining.headOption match {
            case None                     => false
            case Some((offset, op, next)) =>
              val prec: Int = precedence(op)
              if (prec < minPrec) false
              else {
                remaining = remaining.tail
                val rhs = climb(prec + 1, next)
                val op1 = if (op.length == 1) {
                  (op.charAt(0): @switch) match {
                    case '|' => Expr.BinaryOp.OP_|
                    case '^' => Expr.BinaryOp.OP_^
                    case '&' => Expr.BinaryOp.OP_&
                    case '<' => Expr.BinaryOp.OP_<
                    case '>' => Expr.BinaryOp.OP_>
                    case '+' => Expr.BinaryOp.OP_+
                    case '-' => Expr.BinaryOp.OP_-
                    case '*' => Expr.BinaryOp.OP_*
                    case '/' => Expr.BinaryOp.OP_/
                    case '%' => Expr.BinaryOp.OP_%
                    case _   => throw new IllegalArgumentException("Unknown operator: " + op)
                  }
                } else
                  op match {
                    case "*"  => Expr.BinaryOp.OP_*
                    case "/"  => Expr.BinaryOp.OP_/
                    case "%"  => Expr.BinaryOp.OP_%
                    case "+"  => Expr.BinaryOp.OP_+
                    case "-"  => Expr.BinaryOp.OP_-
                    case "<<" => Expr.BinaryOp.OP_<<
                    case ">>" => Expr.BinaryOp.OP_>>
                    case "<"  => Expr.BinaryOp.OP_<
                    case ">"  => Expr.BinaryOp.OP_>
                    case "<=" => Expr.BinaryOp.OP_<=
                    case ">=" => Expr.BinaryOp.OP_>=
                    case "in" => Expr.BinaryOp.OP_in
                    case "==" => Expr.BinaryOp.OP_==
                    case "!=" => Expr.BinaryOp.OP_!=
                    case "&"  => Expr.BinaryOp.OP_&
                    case "^"  => Expr.BinaryOp.OP_^
                    case "|"  => Expr.BinaryOp.OP_|
                    case "&&" => Expr.BinaryOp.OP_&&
                    case "||" => Expr.BinaryOp.OP_||
                  }
                result = op1 match {
                  case Expr.BinaryOp.OP_&& => Expr.And(offset, result, rhs)
                  case Expr.BinaryOp.OP_|| => Expr.Or(offset, result, rhs)
                  case _                   => Expr.BinaryOp(offset, result, op1, rhs)
                }
                true
              }
          }
        ) ()
        result
      }

      climb(0, pre)
    }

  def expr1[$: P]: P[Expr] = P(expr2 ~ exprSuffix2.rep).map { case (pre, fs) =>
    fs.foldLeft(pre) { case (p, f) => f(p) }
  }

  def exprSuffix2[$: P]: P[Expr => Expr] = P(
    Pos.flatMapX { i =>
      CharIn(".[({")./.!.map(_(0)).flatMapX { c =>
        (c: @switch) match {
          case '.' => Pass ~ id.map(x => Expr.Select(i, _: Expr, x))
          case '[' =>
            Pass ~ (expr.? ~ (":" ~ expr.?).rep ~ "]").map {
              case (Some(tree), Seq()) => Expr.Lookup(i, _: Expr, tree)
              case (start, ins)        =>
                Expr.Slice(i, _: Expr, start, ins.headOption.flatten, ins.lift(1).flatten)
            }
          case '(' =>
            Pass ~ (args ~ ")" ~ "tailstrict".!.?).map { case (args, namedNames, tailstrict) =>
              Expr.Apply(
                i,
                _: Expr,
                args,
                if (namedNames.length == 0) null else namedNames,
                tailstrict.nonEmpty
              )
            }
          case '{' => Pass ~ (objinside ~ "}").map(x => Expr.ObjExtend(i, _: Expr, x))
          case _   => Fail
        }
      }
    }
  )

  def local[$: P]: P[Expr] = P(localExpr)
  def importStr[$: P](pos: Position): P[Expr.ImportStr] = P(importExpr.map(Expr.ImportStr(pos, _)))
  def importBin[$: P](pos: Position): P[Expr.ImportBin] = P(importExpr.map(Expr.ImportBin(pos, _)))
  def `import`[$: P](pos: Position): P[Expr.Import] = P(importExpr.map(Expr.Import(pos, _)))
  def error[$: P](pos: Position): P[Expr.Error] = P(expr.map(Expr.Error(pos, _)))

  def importExpr[$: P]: P[String] = P(
    expr.flatMap {
      case Val.Str(_, s) => Pass(s)
      case _             => Fail.opaque("string literal (computed imports are not allowed)")
    }
  )

  def unaryOpExpr[$: P](pos: Position, op: Char): P[Expr.UnaryOp] = P(
    expr1.map { e =>
      def k2 = (op: @switch) match {
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
  def expr2[$: P]: P[Expr] = P(
    Pos.flatMapX { pos =>
      SingleChar.flatMapX { c =>
        (c: @switch) match {
          case '{'                   => Pass ~ objinside ~ "}"
          case '+' | '-' | '~' | '!' => Pass ~ unaryOpExpr(pos, c)
          case '['                   => Pass ~ arr ~ "]"
          case '('                   => Pass ~ expr ~ ")"
          case '\"'                  => doubleString.map(constructString(pos, _))
          case '\''                  => singleString.map(constructString(pos, _))
          case '@'                   =>
            SingleChar./.flatMapX {
              case '\"' => literalDoubleString.map(constructString(pos, _))
              case '\'' => literalSingleString.map(constructString(pos, _))
              case _    => Fail
            }
          case '|' => maybeChompedTripleBarString.map(constructString(pos, _))
          case '$' => Pass(Expr.$(pos))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            P.current.index = pos.offset; number
          case x =>
            if (idStartChar(x)) CharsWhileIn("_a-zA-Z0-9", 0).!.flatMapX { y =>
              "" + x + y match {
                case "null"      => Pass(Val.Null(pos))
                case "true"      => Pass(Val.True(pos))
                case "false"     => Pass(Val.False(pos))
                case "self"      => Pass(Expr.Self(pos))
                case "super"     => Pass(Expr.Super(pos))
                case "if"        => Pass ~ ifElse
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
            else Fail
        }
      }
    }
  )

  def objinside[$: P]: P[Expr.ObjBody] = P(
    Pos ~ member.rep(sep = ",") ~ ",".? ~ (forspec ~ compspec).?
  ).flatMap { case t @ (_, exprs, _) =>
    val seen = collection.mutable.Set.empty[String]
    var overlap: String = null
    exprs.foreach {
      case Expr.Member.Field(_, Expr.FieldName.Fixed(n), _, _, _, _) =>
        if (seen(n)) overlap = n
        else seen.add(n)
      case _ =>
    }
    if (overlap == null) Pass(t)
    else Fail.opaque("no duplicate field: " + overlap)
  }.flatMapX {
    case (pos, exprs, None) =>
      val b =
        exprs.iterator.filter(_.isInstanceOf[Expr.Bind]).asInstanceOf[Iterator[Expr.Bind]].toArray
      val seen = collection.mutable.Set.empty[String]
      var overlap: String = null
      b.foreach {
        case Expr.Bind(_, n, _, _) =>
          if (seen(n)) overlap = n
          else seen.add(n)
        case null =>
      }
      if (overlap != null) {
        Fail.opaque("no duplicate local: " + overlap)
      } else {
        val binds = if (b.isEmpty) null else b

        val fields = exprs.iterator
          .filter(_.isInstanceOf[Expr.Member.Field])
          .asInstanceOf[Iterator[Expr.Member.Field]]
          .toArray
        val asserts = {
          val a = exprs.iterator
            .filter(_.isInstanceOf[Expr.Member.AssertStmt])
            .asInstanceOf[Iterator[Expr.Member.AssertStmt]]
            .toArray
          if (a.isEmpty) null else a
        }
        if (binds == null && asserts == null && fields.forall(_.isStatic))
          Pass(Val.staticObject(pos, fields, internedStaticFieldSets, internedStrings))
        else Pass(Expr.ObjBody.MemberList(pos, binds, fields, asserts))
      }
    case (pos, exprs, Some(comps)) =>
      val preLocals = exprs
        .takeWhile(_.isInstanceOf[Expr.Bind])
        .map(_.asInstanceOf[Expr.Bind])
      if (preLocals.nonEmpty && exprs.length == preLocals.length) {
        Fail.opaque("object comprehension must have a field")
      } else
        exprs(preLocals.length) match {
          case Expr.Member.Field(
                offset,
                Expr.FieldName.Dyn(lhs),
                plus,
                args,
                Visibility.Normal,
                rhsBody
              ) =>
            val rhs = if (args == null) {
              rhsBody
            } else {
              Expr.Function(offset, args, rhsBody)
            }
            val postLocals = exprs
              .drop(preLocals.length + 1)
              .takeWhile(_.isInstanceOf[Expr.Bind])
              .map(_.asInstanceOf[Expr.Bind])

            /*
             * Prevent duplicate fields in list comprehension. See: https://github.com/databricks/sjsonnet/issues/99
             *
             * If comps._1 is a forspec with value greater than one lhs cannot be a Expr.Str
             * Otherwise the field value will be overriden by the multiple iterations of forspec
             */
            (lhs, comps) match {
              case (Val.Str(_, _), (Expr.ForSpec(_, _, Expr.Arr(_, values)), _))
                  if values.length > 1 =>
                Fail.opaque(s"""no duplicate field: "${lhs.asInstanceOf[Val.Str].value}" """)
              case (Val.Str(_, _), (Expr.ForSpec(_, _, arr: Val.Arr), _)) if arr.length > 1 =>
                Fail.opaque(s"""no duplicate field: "${lhs.asInstanceOf[Val.Str].value}" """)
              case _ =>
                Pass(
                  Expr.ObjBody.ObjComp(
                    pos,
                    preLocals.toArray,
                    lhs,
                    rhs,
                    plus,
                    postLocals.toArray,
                    comps._1,
                    comps._2.toList
                  )
                )
            }
          case _: Expr.Member.AssertStmt =>
            Fail.opaque("object comprehension cannot have asserts")
          case _ =>
            Fail.opaque(
              "object comprehension must have a single field with a dynamic field name"
            )
        }
  }

  def member[$: P]: P[Expr.Member] = P(objlocal | "assert" ~~ break ~ assertStmt | field)
  def field[$: P]: P[Expr.Member.Field] = P(
    (Pos ~~ fieldname ~/ "+".!.? ~ ("(" ~ params ~ ")").? ~ fieldKeySep ~/ expr).map {
      case (pos, name, plus, p, h2, e) =>
        Expr.Member.Field(pos, name, plus.nonEmpty, p.orNull, h2, e)
    }
  )
  def fieldKeySep[$: P]: P[Visibility] = P(StringIn(":::", "::", ":")).!.map {
    case ":"   => Visibility.Normal
    case "::"  => Visibility.Hidden
    case ":::" => Visibility.Unhide
  }
  def objlocal[$: P]: P[Expr.Bind] = P("local" ~~ break ~/ bind)
  def compspec[$: P]: P[Seq[Expr.CompSpec]] = P((forspec | ifspec).rep)
  def forspec[$: P]: P[Expr.ForSpec] =
    P(Pos ~~ "for" ~~ break ~/ id ~ "in" ~~ break ~ expr).map { case (pos, name, cond) =>
      Expr.ForSpec(pos, name, cond)
    }
  def ifspec[$: P]: P[Expr.IfSpec] = P(Pos ~~ "if" ~~ break ~/ expr).map { case (pos, cond) =>
    Expr.IfSpec(pos, cond)
  }
  def fieldname[$: P]: P[Expr.FieldName] = P(
    id.map(Expr.FieldName.Fixed.apply) |
    string.map(Expr.FieldName.Fixed.apply) |
    "[" ~ expr.map(Expr.FieldName.Dyn.apply) ~ "]"
  )
  def assertStmt[$: P]: P[Expr.Member.AssertStmt] =
    P(expr ~ (":" ~ expr).?.map(_.orNull)).map { case (value, msg) =>
      Expr.Member.AssertStmt(value, msg)
    }

  def bind[$: P]: P[Expr.Bind] =
    P(Pos ~~ id ~ ("(" ~/ params.? ~ ")").?.map(_.flatten).map(_.orNull) ~ "=" ~ expr)
      .map { case (pos, name, args, rhs) => Expr.Bind(pos, name, args, rhs) }

  def args[$: P]: P[(Array[Expr], Array[String])] =
    P(((id ~ "=" ~ !"=").? ~ expr).rep(sep = ",") ~ ",".?).flatMapX { x =>
      if (
        x.sliding(2).exists {
          case Seq(l, r) => l._1.isDefined && r._1.isEmpty
          case _         => false
        }
      ) {
        Fail.opaque("no positional params after named params")
      } else {
        val args = x.iterator.map(_._2).toArray
        val namedNames = x.iterator.dropWhile(_._1.isEmpty).map(_._1.get).toArray
        Pass((args, namedNames))
      }
    }

  def params[$: P]: P[Expr.Params] = P((id ~ ("=" ~ expr).?).rep(sep = ",") ~ ",".?).flatMapX { x =>
    val seen = collection.mutable.Set.empty[String]
    var overlap: String = null
    for ((k, _) <- x) {
      if (seen(k)) overlap = k
      else seen.add(k)
    }
    if (overlap == null) {
      val names = x.map(_._1).toArray[String]
      val exprs = x.map(_._2.orNull).toArray[Expr]
      Pass(Expr.Params(names, exprs))
    } else Fail.opaque("no duplicate parameter: " + overlap)

  }

  def binaryop[$: P]: P[String] = P(
    StringIn(
      "<<",
      ">>",
      "<=",
      ">=",
      "in",
      "==",
      "!=",
      "&&",
      "||",
      "*",
      "/",
      "%",
      "+",
      "-",
      "<",
      ">",
      "&",
      "^",
      "|"
    )
  ).!

  def document[$: P]: P[(Expr, FileScope)] = P(expr ~ Pass(fileScope) ~ End)
}

final class Position(val fileScope: FileScope, val offset: Int) {
  def currentFile: Path = fileScope.currentFile
  def noOffset: Position = fileScope.noOffsetPos
  override def equals(o: Any): Boolean = o match {
    case o: Position => currentFile == o.currentFile && offset == o.offset
    case _           => false
  }
  override def toString: String = {
    val name = if (fileScope == null) "null" else fileScope.currentFileLastPathElement
    s"Position($name, $offset)"
  }
}

/**
 * FileScope models the per-file context that is propagated throughout the evaluation of a single
 * Jsonnet file. Contains the current file path.
 */
class FileScope(val currentFile: Path) {
  lazy val currentFileLastPathElement: String = if (currentFile == null) null else currentFile.last
  val noOffsetPos: Position = new Position(this, -1)
}
