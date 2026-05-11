package sjsonnet

import fastparse.Parsed

import scala.collection.mutable

/**
 * Drives asynchronous (or otherwise externally-controlled) loading of imports by statically
 * discovering them ahead of evaluation.
 *
 * Jsonnet has no dynamic imports: every `import`, `importstr`, or `importbin` expression has a
 * literal string path. So given the parsed AST of an entry file we can enumerate its imports, load
 * them, parse the loaded code files for further imports, and repeat until the closure is known.
 * Once all files are in the cache, normal synchronous evaluation can run.
 *
 * Eager front-loading: every reachable import is loaded up front, including ones inside branches
 * the evaluator will never force (`if false then import 'x' else 1`). This trades laziness for the
 * ability to do all I/O before evaluation. Parse errors on discovered (non-entry) files are
 * tolerated for the same reason — they only surface at evaluation time if the branch is actually
 * forced. Loader failures (a rejected Promise, missing file, etc.) are real I/O problems and
 * propagate up to the caller.
 *
 * The parsed AST of each loaded code file is attached to its cache entry as a
 * [[PreParsedResolvedFile]] so the Interpreter does not re-run fastparse during evaluation; the
 * static optimizer still runs once on cache hit.
 *
 * Cache keying: entries are keyed by `(Path, binaryData)` so that the same path referenced as both
 * `importstr` (text) and `importbin` (bytes) keeps two distinct cache entries — matching the
 * `Importer.read(path, binaryData)` contract. Without this, an `importstr "x" + importbin "x"`
 * program would overwrite one entry and hand the wrong content to the evaluator.
 *
 * Usage (pseudo-code):
 * {{{
 *   val preloader = new Preloader(parentImporter)
 *   preloader.add(entryPath, StaticResolvedFile(entryText), ImportKind.Code)
 *   while (preloader.pendingImports.nonEmpty) {
 *     val batch = preloader.takePendingImports()
 *     for (p <- batch) {
 *       val content = await asyncLoad(p.path, p.binaryData)  // platform-specific async
 *       preloader.add(p.path, content, p.kind)
 *     }
 *   }
 *   val interpreter = new Interpreter(..., importer = preloader.importer, ...)
 *   interpreter.interpret(entryText, entryPath)
 * }}}
 *
 * @param parentImporter
 *   used only to resolve import names to [[Path]]s. Its `read` is never called.
 * @param settings
 *   parser settings (recursion depth, etc.).
 */
class Preloader(parentImporter: Importer, settings: Settings = Settings.default) {

  private val internedStrings = new mutable.HashMap[String, String]
  private val internedFieldSets =
    new mutable.HashMap[Val.StaticObjectFieldSet, java.util.LinkedHashMap[
      String,
      java.lang.Boolean
    ]]

  // Keyed by (path, binaryData) so importstr and importbin for the same path don't collide.
  private val cache = mutable.LinkedHashMap.empty[(Path, Boolean), ResolvedFile]

  // Tracks the strongest kind enqueued/loaded for each (path, binaryData). Used both to dedupe
  // loader calls and to upgrade a previously-Str pending entry to Code if a Code reference shows
  // up later.
  private val seen = mutable.HashMap.empty[(Path, Boolean), ImportKind]
  private val pending = mutable.ArrayBuffer.empty[Preloader.Pending]

  /** Resolve an import name relative to a base path, using the parent importer. */
  def resolve(docBase: Path, importName: String): Option[Path] =
    parentImporter.resolve(docBase, importName)

  /**
   * Register a loaded file in the cache and, if it's a Jsonnet code file, parse it to discover its
   * imports.
   *
   * The returned `Either` reports parse errors for code files; binary or string imports never fail
   * here.
   */
  def add(
      path: Path,
      content: ResolvedFile,
      kind: ImportKind = ImportKind.Code): Either[Error, Unit] = {
    putContent(path, kind.binaryData, content)
    if (kind.isCode) discover(path, content) else Right(())
  }

  /** All imports queued for loading. */
  def pendingImports: Seq[Preloader.Pending] = pending.toSeq

  /** Atomically take and clear the queue of pending imports. */
  def takePendingImports(): Seq[Preloader.Pending] = {
    val out = pending.toVector
    pending.clear()
    out
  }

  /** True when no more imports need to be loaded. */
  def isComplete: Boolean = pending.isEmpty

  /**
   * An [[Importer]] that resolves names through the parent importer but reads exclusively from this
   * preloader's cache. Pass to an [[Interpreter]] for synchronous evaluation after preload
   * completes.
   */
  def importer: Importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] =
      parentImporter.resolve(docBase, importName)
    def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
      cache.get((path, binaryData))
  }

  /** Snapshot of the loaded cache, exposed so callers can inspect or persist it. */
  def loaded: collection.Map[(Path, Boolean), ResolvedFile] = cache

  /**
   * Insert content into the cache without clobbering a richer (pre-parsed) entry. discover() puts a
   * [[PreParsedResolvedFile]]; a later add() of the same physical content (e.g. via a separate
   * `importstr` reference to the same path) must not downgrade it back to plain text.
   */
  private def putContent(path: Path, binaryData: Boolean, content: ResolvedFile): Unit = {
    val key = (path, binaryData)
    cache.get(key) match {
      case Some(existing) if existing.preParsedAst.isDefined && content.preParsedAst.isEmpty =>
      // keep the pre-parsed version
      case _ =>
        cache.put(key, content)
    }
  }

  private def discover(path: Path, content: ResolvedFile): Either[Error, Unit] = {
    CachedResolver.parseJsonImport(
      path,
      content,
      internedStrings,
      settings
    ) match {
      case Some((expr, fs)) =>
        cache.put((path, false), PreParsedResolvedFile(content, expr, fs))
        Right(())
      case None =>
        val parser = new Parser(path, internedStrings, internedFieldSets, settings)
        try {
          fastparse.parse(content.getParserInput(), parser.document(_)) match {
            case f: Parsed.Failure =>
              val traced = f.trace()
              Left(new ParseError(s"$path: ${traced.msg}", offset = traced.index))
            case Parsed.Success((expr, fs), _) =>
              // Stash the parsed AST on the cache entry so the Interpreter doesn't re-run fastparse.
              // The optimizer still runs once at evaluation time on cache hit.
              cache.put((path, false), PreParsedResolvedFile(content, expr, fs))
              // Match the synchronous evaluator's docBase: resolve relative to the importing file's
              // parent directory, not the file path itself. See Importer.resolveAndReadOrFail, which
              // calls resolve(pos.fileScope.currentFile.parent(), ...).
              val docBase = path.parent()
              ImportFinder.collect(expr).foreach { found =>
                parentImporter.resolve(docBase, found.value).foreach { resolved =>
                  record(resolved, found.kind)
                }
              }
              Right(())
          }
        } catch {
          case e: ParseError => Left(e)
        }
    }
  }

  /**
   * Record that `path` was referenced with `kind`. Enqueues the load if new, upgrades a pending
   * Str→Code if applicable, and lazily parses an already-loaded Str entry that needs Code analysis.
   */
  private def record(path: Path, kind: ImportKind): Unit = {
    val key = (path, kind.binaryData)
    cache.get(key) match {
      case Some(content) =>
        // Already loaded. If we now need Code analysis (e.g. an earlier `importstr` reference
        // loaded the file as plain text and we just hit an `import` of the same path), parse it
        // now and walk for sub-imports.
        if (kind.isCode && content.preParsedAst.isEmpty) {
          val _ = discover(path, content)
        }
      case None =>
        seen.get(key) match {
          case None =>
            seen(key) = kind
            pending += Preloader.Pending(path, kind)
          case Some(existing) if !existing.isCode && kind.isCode =>
            seen(key) = kind
            val idx =
              pending.indexWhere(p => p.path == path && p.kind.binaryData == kind.binaryData)
            if (idx >= 0) pending(idx) = Preloader.Pending(path, kind)
          case _ => // already pending with equal-or-stronger kind
        }
    }
  }
}

object Preloader {

  /** A path that needs to be loaded before evaluation can proceed. */
  final case class Pending(path: Path, kind: ImportKind) {
    def binaryData: Boolean = kind.binaryData
    def isCode: Boolean = kind.isCode
  }
}
