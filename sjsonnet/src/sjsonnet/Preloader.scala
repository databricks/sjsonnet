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
 * Usage (pseudo-code):
 * {{{
 *   val preloader = new Preloader(parentImporter)
 *   preloader.add(entryPath, StaticResolvedFile(entryText), Preloader.EntryKind)
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

  private val cache = mutable.LinkedHashMap.empty[Path, ResolvedFile]
  private val seen = mutable.HashSet.empty[(Path, ImportFinder.Kind)]
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
      kind: ImportFinder.Kind = ImportFinder.Kind.Code): Either[Error, Unit] = {
    cache.put(path, content)
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
    def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = cache.get(path)
  }

  /** Snapshot of the loaded cache, exposed so callers can inspect or persist it. */
  def loaded: collection.Map[Path, ResolvedFile] = cache

  private def discover(path: Path, content: ResolvedFile): Either[Error, Unit] = {
    val parser = new Parser(path, internedStrings, internedFieldSets, settings)
    try {
      fastparse.parse(content.getParserInput(), parser.document(_)) match {
        case f: Parsed.Failure =>
          val traced = f.trace()
          Left(new ParseError(s"$path: ${traced.msg}", offset = traced.index))
        case Parsed.Success((expr, _), _) =>
          // Match the synchronous evaluator's docBase: resolve relative to the importing file's
          // parent directory, not the file path itself. See Importer.resolveAndReadOrFail, which
          // calls resolve(pos.fileScope.currentFile.parent(), ...).
          val docBase = path.parent()
          ImportFinder.collect(expr).foreach { found =>
            parentImporter.resolve(docBase, found.value) match {
              case Some(resolved) =>
                if (seen.add((resolved, found.kind))) enqueue(resolved, found.kind)
              case None =>
              // resolution failure is deferred until evaluation, where it will surface
              // with a proper stack frame
            }
          }
          Right(())
      }
    } catch {
      case e: ParseError => Left(e)
    }
  }

  private def enqueue(path: Path, kind: ImportFinder.Kind): Unit = {
    if (cache.contains(path)) return
    pending += Preloader.Pending(path, kind)
  }
}

object Preloader {

  /** A path that needs to be loaded before evaluation can proceed. */
  final case class Pending(path: Path, kind: ImportFinder.Kind) {
    def binaryData: Boolean = kind.binaryData
    def isCode: Boolean = kind.isCode
  }
}
