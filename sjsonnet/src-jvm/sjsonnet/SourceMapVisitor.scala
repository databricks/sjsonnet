package sjsonnet

import fastparse.IndexedParserInput
import upickle.core._

import scala.annotation.tailrec
import scala.collection.mutable

class SourceMapVisitor extends ujson.JsVisitor[SourceMap, SourceMap] { self =>
  private val m = new SourceMap
  private var path: List[String] = Nil
  private val loadedFileContents = mutable.Map.empty[Path, IndexedParserInput]

  def saveCurrentPos() = {
    val current = CurrentPos.currentPos.get()
    val fpath = current.currentFile.asInstanceOf[OsPath].p
    val parserInput = loadedFileContents
      .getOrElse(current.currentFile, new IndexedParserInput(os.read(fpath)))
    val prettyIdx = parserInput.prettyIndex(current.offset)
    m.positions.put(path.reverseIterator.mkString("."), (fpath, prettyIdx))
  }


  private def visitPrimitive(): SourceMap = { saveCurrentPos(); m }

  def visitArray(length: Int, index: Int): ArrVisitor[SourceMap, SourceMap] = new ArrVisitor[SourceMap, SourceMap] {
    saveCurrentPos()
    var idx = 1
    path = idx.toString :: path
    def subVisitor: Visitor[_, _] = self
    def visitValue(v: SourceMap, index: Int): Unit = {
      idx += 1
      path = idx.toString :: path.tail
    }
    def visitEnd(index: Int): SourceMap = {
      path = path.tail
      m
    }
  }

  def visitObject(length: Int, index: Int): ObjVisitor[SourceMap, SourceMap] = new ObjVisitor[SourceMap, SourceMap] {
    saveCurrentPos()
    path = "_" :: path
    def visitKey(index: Int): Visitor[_, _] = StringVisitor
    def visitKeyValue(v: Any): Unit = path = v.toString :: path.tail
    def subVisitor: Visitor[_, _] = self
    def visitValue(v: SourceMap, index: Int): Unit = ()
    def visitEnd(index: Int): SourceMap = {
      path = path.tail
      m
    }
  }

  def visitNull(index: Int): SourceMap = visitPrimitive()
  def visitFalse(index: Int): SourceMap = visitPrimitive()
  def visitTrue(index: Int): SourceMap = visitPrimitive()
  def visitString(s: CharSequence, index: Int) = visitPrimitive()
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = visitPrimitive()
}

class SourceMap {
  val positions = mutable.Map[String, (os.Path, String)]()

  @tailrec
  final def findNearest(p: JsonPath): (os.Path, String) = {
    positions.get(p.toString) match {
      case Some(pos) => pos
      case None => findNearest(p.parent.get)
    }
  }
}
