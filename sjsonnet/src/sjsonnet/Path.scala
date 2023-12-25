package sjsonnet

import fastparse.IndexedParserInput

import scala.collection.mutable

/**
  * [[Path]]s represent handles that Sjsonnet can use to resolve imports and
  * load file contents. Abstracts away the filesystem access so import
  * resolution can be customized, e.g. using a virtual filesystem when running
  * in the browser.
  */
trait Path {
  def relativeToString(p: Path): String
  def parent(): Path
  def segmentCount(): Int
  def last: String
  def /(s: String): Path
  def renderOffsetStr(offset: Int, loadedFileContents: mutable.HashMap[Path, Array[Int]]): String

  /**
   * The size of the file in bytes.
   */
  def size: Long
}
