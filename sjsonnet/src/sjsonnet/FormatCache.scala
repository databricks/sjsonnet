package sjsonnet

/**
 * Cache for parsed format strings used by the `%` operator and `std.format`.
 *
 * Abstracting this as a trait (similar to [[ParseCache]]) allows users to plug in their own cache
 * implementation (e.g., Caffeine-based) for better control over eviction, concurrency, and memory.
 *
 * The cached values are opaque [[Format.CompiledFormat]] entries that represent pre-processed
 * format strings with array-indexed specifiers and metadata for fast-path checks.
 */
trait FormatCache {

  /**
   * Retrieve a cached compiled format entry, or compute and store it.
   *
   * @param key
   *   The raw format string (e.g., `"Hello %s, you are %d years old"`)
   * @param compute
   *   A by-name computation that parses the format string into a [[Format.CompiledFormat]]. Under
   *   concurrent access, `compute` may be evaluated more than once for the same key if multiple
   *   threads miss simultaneously; implementations should ensure `compute` is pure/idempotent.
   * @return
   *   The compiled format entry, either from cache or freshly computed.
   */
  def getOrElseUpdate(key: String, compute: => Format.CompiledFormat): Format.CompiledFormat
}

object FormatCache {

  /**
   * A shared default format cache instance. Uses an LRU eviction strategy with a maximum of 256
   * entries. This singleton preserves the process-wide caching behavior of the original static
   * cache, ensuring cross-interpreter format string reuse.
   */
  val SharedDefault: FormatCache = new DefaultFormatCache()

  /**
   * An empty format cache that always recomputes. Useful for testing or when caching is not
   * desired.
   */
  val EmptyCache: FormatCache = new FormatCache {
    override def getOrElseUpdate(
        key: String,
        compute: => Format.CompiledFormat): Format.CompiledFormat = compute
  }
}

/**
 * Default LRU format cache with a configurable maximum number of entries. Uses a synchronized
 * `LinkedHashMap` with access-order eviction.
 *
 * This implementation is thread-safe via synchronized blocks. For higher-concurrency scenarios,
 * users can implement [[FormatCache]] with a concurrent cache such as Caffeine.
 *
 * @param maxEntries
 *   The maximum number of entries before the oldest entry is evicted. Defaults to 256.
 */
class DefaultFormatCache(maxEntries: Int = 256) extends FormatCache {
  // Initial capacity set to avoid rehash before eviction kicks in.
  // With load factor 0.75, capacity = maxEntries / 0.75 + 2 ensures the table
  // can hold maxEntries without triggering a resize.
  private val cache =
    new java.util.LinkedHashMap[String, Format.CompiledFormat](
      (maxEntries / 0.75f + 2).toInt,
      0.75f,
      true
    ) {
      override def removeEldestEntry(
          eldest: java.util.Map.Entry[String, Format.CompiledFormat]): Boolean =
        size() > maxEntries
    }

  override def getOrElseUpdate(
      key: String,
      compute: => Format.CompiledFormat): Format.CompiledFormat = {
    // Double-checked locking: first read without full lock, then compute and store on miss
    val cached0 = cache.synchronized(cache.get(key))
    if (cached0 != null) cached0
    else {
      val value = compute
      cache.synchronized {
        val cached1 = cache.get(key)
        if (cached1 != null) cached1
        else {
          cache.put(key, value)
          value
        }
      }
    }
  }
}
