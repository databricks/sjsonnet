package sjsonnet

import scala.collection.Iterator

// Trait extended by JsonnetWorker (in universe) so that it can pass the cache based on Caffeine to main0 here
trait ParseCache {
  def getOrElseUpdate(
      key: (Path, String),
      defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)]
}

object ParseCache {

  /**
   * An empty parse cache that always returns the default value. This is useful for cases where no
   * caching is desired, such as in tests or when the cache is not needed.
   */
  val EmptyCache: ParseCache = new ParseCache {
    final override def getOrElseUpdate(
        key: (Path, String),
        defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
      defaultValue
    }
  }
}

// A default implementation backed by the platform cache store.
class DefaultParseCache extends ParseCache {
  private val cache = Platform.newParseCacheMap()

  // parseCache.getOrElseUpdate((path, txt), {...})
  override def getOrElseUpdate(
      key: (Path, String),
      defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
    cache.get(key) match {
      case Some(value) => value
      case None        =>
        val value = defaultValue
        cache.putIfAbsent(key, value).getOrElse(value)
    }
  }

  // parseCache.valuesIterator.map(_.getOrElse(???)).map(_._1).toSeq
  def valuesIterator: Iterator[Either[Error, (Expr, FileScope)]] = {
    cache.valuesIterator
  }

  // parseCache.keySet.toIndexedSeq
  def keySet: scala.collection.Set[(Path, String)] = {
    cache.keySet.toSet
  }
}
