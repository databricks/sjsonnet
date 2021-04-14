package sjsonnet

import fastparse.Parsed

import scala.collection.mutable

abstract class Resolver {
  def resolve(p: Path, txt: String): Either[String, (Expr, FileScope)]
}

class CachedResolver(
  val parseCache: mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]] = new mutable.HashMap
) extends Resolver {

  def resolve(path: Path, txt: String): Either[String, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate((path, txt), {
      val parsed = fastparse.parse(txt, new Parser(path).document(_)) match {
        case f @ Parsed.Failure(l, i, e) => Left(f.trace().msg)
        case Parsed.Success(r, index) => Right(r)
      }
      parsed.flatMap { case (e, fs) => process(e, fs) }

    })
  }

  def process(expr: Expr, fs: FileScope): Either[String, (Expr, FileScope)] = Right((expr, fs))
}
