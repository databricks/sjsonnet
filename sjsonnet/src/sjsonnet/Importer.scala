package sjsonnet

import fastparse.{IndexedParserInput, Parsed, ParserInput}

import java.io.{BufferedInputStream, File, FileInputStream, RandomAccessFile}
import java.nio.charset.StandardCharsets
import scala.collection.mutable


/** Resolve and read imported files */
abstract class Importer {
  def resolve(docBase: Path, importName: String): Option[Path]
  def read(path: Path, binaryData: Boolean): Option[ResolvedFile]

  private def resolveAndRead(docBase: Path, importName: String, binaryData: Boolean): Option[(Path, ResolvedFile)] = for {
    path <- resolve(docBase, importName)
    txt <- read(path, binaryData)
  } yield (path, txt)

  def resolveAndReadOrFail(value: String, pos: Position, binaryData: Boolean)(implicit ev: EvalErrorScope): (Path, ResolvedFile) =
    resolveAndRead(pos.fileScope.currentFile.parent(), value, binaryData = binaryData)
      .getOrElse(Error.fail("Couldn't import file: " + pprint.Util.literalize(value), pos))
}

object Importer {
  val empty: Importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] = None
    def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = None
  }
}

final case class FileParserInput(file: File) extends ParserInput {

  private val bufferedFile = new BufferedRandomAccessFile(file.getAbsolutePath, 1024 * 8)

  private lazy val fileLength = file.length.toInt

  override def apply(index: Int): Char = {
    bufferedFile.readChar(index)
  }

  override def dropBuffer(index: Int): Unit = {}

  override def slice(from: Int, until: Int): String = {
    bufferedFile.readString(from, until)
  }

  override def length: Int = fileLength

  override def innerLength: Int = length

  override def isReachable(index: Int): Boolean = index < length

  override def checkTraceable(): Unit = {}

  private lazy val lineNumberLookup: Array[Int] = {
    val lines = mutable.ArrayBuffer[Int]()
    val bufferedStream = new BufferedInputStream(new FileInputStream(file))
    var byteRead: Int = 0
    var currentPosition = 0

    while ({ byteRead = bufferedStream.read(); byteRead != -1 }) {
      if (byteRead == '\n') {
        lines += currentPosition + 1
      }
      currentPosition += 1
    }

    bufferedStream.close()

    lines.toArray
  }

  def prettyIndex(index: Int): String = {
    val line = lineNumberLookup.indexWhere(_ > index) match {
      case -1 => lineNumberLookup.length - 1
      case n => math.max(0, n - 1)
    }
    val col = index - lineNumberLookup(line)
    s"${line + 1}:${col + 1}"
  }
}

class BufferedRandomAccessFile(fileName: String, bufferSize: Int) {

  // The file is opened in read-only mode
  private val file = new RandomAccessFile(fileName, "r")

  private val buffer = new Array[Byte](bufferSize)

  private var bufferStart: Long = -1

  private var bufferEnd: Long = -1

  private val fileLength: Long = file.length()

  private def fillBuffer(position: Long): Unit = {
    if (file.getFilePointer != position) {
      file.seek(position)
    }
    val bytesRead = file.read(buffer, 0, bufferSize)
    bufferStart = position
    bufferEnd = position + bytesRead
  }

  def readChar(index: Long): Char = {
    if (index >= fileLength) {
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for file of length $fileLength")
    }
    if (index < bufferStart || index >= bufferEnd) {
      fillBuffer(index)
    }
    buffer((index - bufferStart).toInt).toChar
  }

  def readString(from: Long, until: Long): String = {
    if (!(from < fileLength && until <= fileLength && from <= until)) {
      throw new IndexOutOfBoundsException(s"Invalid range: $from-$until for file of length $fileLength")
    }
    val length = (until - from).toInt

    if (from >= bufferStart && until <= bufferEnd) {
      // Range is within the buffer
      new String(buffer, (from - bufferStart).toInt, length, StandardCharsets.UTF_8)
    } else {
      // Range is outside the buffer
      val stringBytes = new Array[Byte](length)
      file.seek(from)
      file.readFully(stringBytes, 0, length)
      new String(stringBytes, StandardCharsets.UTF_8)
    }
  }

  def close(): Unit = {
    file.close()
  }
}

trait ResolvedFile {
  /**
   * Get an efficient parser input for this resolved file. Large files will be read from disk
   * (buffered reads), while small files will be served from memory.
   */
  def getParserInput(): ParserInput

  // Use this to read the file as a string. This is generally used for `importstr`
  def readString(): String

  // Get a content hash of the file suitable for detecting changes in a given file.
  def contentHash(): String

  // Used by importbin
  def readRawBytes(): Array[Byte]
}

final case class StaticResolvedFile(content: String) extends ResolvedFile {
  def getParserInput(): ParserInput = IndexedParserInput(content)

  def readString(): String = content

  // We just cheat, the content hash can be the content itself for static imports
  def contentHash(): String = content

  override def readRawBytes(): Array[Byte] = content.getBytes(StandardCharsets.UTF_8)
}

final case class StaticBinaryResolvedFile(content: Array[Byte]) extends ResolvedFile {
  def getParserInput(): ParserInput = ??? // Not used for binary imports

  def readString(): String = ??? // Not used for binary imports

  // We just cheat, the content hash can be the content itself for static imports
  def contentHash(): String = content.hashCode().toString

  override def readRawBytes(): Array[Byte] = content
}

class CachedImporter(parent: Importer) extends Importer {
  val cache: mutable.HashMap[Path,ResolvedFile] = mutable.HashMap.empty[Path, ResolvedFile]

  def resolve(docBase: Path, importName: String): Option[Path] = parent.resolve(docBase, importName)

  def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = cache.get(path) match {
    case s @ Some(x) =>
      if(x == null) None else s
    case None =>
      val x = parent.read(path, binaryData)
      cache.put(path, x.orNull)
      x
  }
}

class CachedResolver(
  parentImporter: Importer,
  val parseCache: ParseCache,
  strictImportSyntax: Boolean,
  internedStrings: mutable.HashMap[String, String],
  internedStaticFieldSets: mutable.HashMap[Val.StaticObjectFieldSet, java.util.LinkedHashMap[String, java.lang.Boolean]]) extends CachedImporter(parentImporter) {

  def parse(path: Path, content: ResolvedFile)(implicit ev: EvalErrorScope): Either[Error, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate((path, content.contentHash()), {
      val parsed = fastparse.parse(content.getParserInput(), new Parser(path, strictImportSyntax, internedStrings, internedStaticFieldSets).document(_)) match {
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
