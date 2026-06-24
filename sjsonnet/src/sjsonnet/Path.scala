package sjsonnet

import scala.collection.mutable

/**
 * [[Path]]s represent handles that Sjsonnet can use to resolve imports and load file contents.
 * Abstracts away the filesystem access so import resolution can be customized, e.g. using a virtual
 * filesystem when running in the browser.
 */
trait Path {
  def relativeToString(p: Path): String

  /**
   * Returns the path representation suitable for `std.thisFile`. By default, returns the relative
   * path to the working directory. Subclasses may override this to preserve the original path
   * string (e.g., when the file was invoked with an absolute path).
   */
  def thisFileRepr(wd: Path): String = relativeToString(wd)

  def parent(): Path
  def segmentCount(): Int
  def last: String
  def /(s: String): Path
  def renderOffsetStr(offset: Int, loadedFileContents: mutable.HashMap[Path, Array[Int]]): String
}
