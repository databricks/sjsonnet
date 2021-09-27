package sjsonnet

// Trait extended by JsonnetWorker (in universe) so that it can pass the cache based on Caffeine to main0 here
trait ParseCache {
  def getOrElseUpdate(key: (Path, String), defaultValue:  => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)]
}

// A default implementation based on a mutable HashMap. This implementation is not thread-safe.
class DefaultParseCache extends ParseCache {
  val cache = new collection.mutable.HashMap[(Path, String), Either[Error, (Expr, FileScope)]]()

  override def getOrElseUpdate(key: (Path, String), defaultValue:  => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
    cache.getOrElseUpdate(key, defaultValue)
  }
}