package sjsonnet

import scala.collection.mutable

import fastparse.Parsed

/** Resolve and read imported files */
abstract class Importer {
  def resolve(docBase: Path, importName: String): Option[Path]
  def read(path: Path): Option[String]

  def resolveAndRead(docBase: Path, importName: String): Option[(Path, String)] = for {
    path <- resolve(docBase, importName)
    txt <- read(path)
  } yield (path, txt)

  def resolveAndReadOrFail(value: String, pos: Position)(implicit ev: EvalErrorScope): (Path, String) =
    resolveAndRead(pos.fileScope.currentFile.parent(), value)
      .getOrElse(Error.fail("Couldn't import file: " + pprint.Util.literalize(value), pos))
}

object Importer {
  val empty: Importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] = None
    def read(path: Path): Option[String] = None
  }
}

class CachedImporter(parent: Importer) extends Importer {
  val cache = mutable.HashMap.empty[Path, String]

  def resolve(docBase: Path, importName: String): Option[Path] = parent.resolve(docBase, importName)

  def read(path: Path): Option[String] = cache.get(path) match {
    case s @ Some(x) =>
      if(x == null) None else s
    case None =>
      val x = parent.read(path)
      cache.put(path, x.getOrElse(null))
      x
  }
}

class CachedResolver(
  parentImporter: Importer,
  val parseCache: ParseCache,
  strictImportSyntax: Boolean) extends CachedImporter(parentImporter) {

  def parse(path: Path, txt: String)(implicit ev: EvalErrorScope): Either[Error, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate((path, txt), {
      val parsed = fastparse.parse(txt, new Parser(path, strictImportSyntax).document(_)) match {
        case f @ Parsed.Failure(_, _, _) =>
          val traced = f.trace()
          val pos = new Position(new FileScope(path), traced.index)
          Left(new ParseError(traced.msg).addFrame(pos))
        case Parsed.Success(r, _) => Right(r)
      }
      parsed.flatMap { case (e, fs) => process(e, fs) }
    })
  }

  def process(expr: Expr, fs: FileScope): Either[Error, (Expr, FileScope)] = Right((expr, fs))
}
