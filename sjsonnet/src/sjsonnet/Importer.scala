package sjsonnet

import fastparse.{IndexedParserInput, Parsed, ParserInput}

import java.io.{BufferedInputStream, File, FileInputStream, RandomAccessFile}
import java.nio.charset.StandardCharsets
import java.util
import scala.collection.mutable

/** Resolve and read imported files */
abstract class Importer {
  def resolve(docBase: Path, importName: String): Option[Path]
  def read(path: Path, binaryData: Boolean): Option[ResolvedFile]

  private def resolveAndRead(
      docBase: Path,
      importName: String,
      binaryData: Boolean): Option[(Path, ResolvedFile)] = for {
    path <- resolve(docBase, importName)
    txt <- read(path, binaryData)
  } yield (path, txt)

  def resolveAndReadOrFail(value: String, pos: Position, binaryData: Boolean)(implicit
      ev: EvalErrorScope): (Path, ResolvedFile) =
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
    val lines = new mutable.ArrayBuilder.ofInt
    lines.sizeHint(100) // reasonable initial size hint
    lines.+=(0)
    val bufferedStream = new BufferedInputStream(new FileInputStream(file))
    var byteRead: Int = 0
    var currentPosition = 0

    while ({ byteRead = bufferedStream.read(); byteRead != -1 }) {
      if (byteRead == '\n') {
        lines.+=(currentPosition + 1)
      }
      currentPosition += 1
    }

    bufferedStream.close()

    lines.result()
  }

  def prettyIndex(index: Int): String =
    Util.prettyIndex(lineNumberLookup, index)
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
      throw new IndexOutOfBoundsException(
        s"Index $index is out of bounds for file of length $fileLength"
      )
    }
    if (index < bufferStart || index >= bufferEnd) {
      fillBuffer(index)
    }
    buffer((index - bufferStart).toInt).toChar
  }

  def readString(from: Long, until: Long): String = {
    if (!(from < fileLength && until <= fileLength && from <= until)) {
      throw new IndexOutOfBoundsException(
        s"Invalid range: $from-$until for file of length $fileLength"
      )
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

  /**
   * Optional pre-parsed AST. When defined, [[CachedResolver.parse]] uses this instead of running
   * fastparse again. Set by [[Preloader]] to avoid parsing each file twice (once during async
   * import discovery, once during evaluation).
   */
  def preParsedAst: Option[(Expr, FileScope)] = None
}

/** Wraps another [[ResolvedFile]] with an attached pre-parsed AST so the parser can be skipped. */
final case class PreParsedResolvedFile(underlying: ResolvedFile, expr: Expr, fileScope: FileScope)
    extends ResolvedFile {
  def getParserInput(): ParserInput = underlying.getParserInput()
  def readString(): String = underlying.readString()
  def contentHash(): String = underlying.contentHash()
  def readRawBytes(): Array[Byte] = underlying.readRawBytes()
  override val preParsedAst: Option[(Expr, FileScope)] = Some((expr, fileScope))
}

final case class StaticResolvedFile(content: String) extends ResolvedFile {
  def getParserInput(): ParserInput = IndexedParserInput(content)

  def readString(): String = content

  // We just cheat, the content hash can be the content itself for static imports
  def contentHash(): String = content

  override def readRawBytes(): Array[Byte] = content.getBytes(StandardCharsets.UTF_8)
}

final case class StaticBinaryResolvedFile(content: Array[Byte]) extends ResolvedFile {
  def getParserInput(): ParserInput = throw new NotImplementedError("Not used for binary imports")

  def readString(): String = throw new NotImplementedError("Not used for binary imports")

  // We just cheat, the content hash can be the content itself for static imports
  def contentHash(): String = content.hashCode().toString

  override def readRawBytes(): Array[Byte] = content
}

class CachedImporter(parent: Importer) extends Importer {
  val cache: mutable.HashMap[Path, ResolvedFile] = mutable.HashMap.empty[Path, ResolvedFile]

  def resolve(docBase: Path, importName: String): Option[Path] = parent.resolve(docBase, importName)

  def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = cache.get(path) match {
    case s @ Some(x) =>
      if (x == null) None else s
    case None =>
      val x = parent.read(path, binaryData)
      cache.put(path, x.orNull)
      x
  }
}

class CachedResolver(
    parentImporter: Importer,
    val parseCache: ParseCache,
    internedStrings: mutable.HashMap[String, String],
    internedStaticFieldSets: mutable.HashMap[
      Val.StaticObjectFieldSet,
      java.util.LinkedHashMap[String, java.lang.Boolean]
    ],
    settings: Settings = Settings.default)
    extends CachedImporter(parentImporter) {

  def parse(path: Path, content: ResolvedFile)(implicit
      ev: EvalErrorScope): Either[Error, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate(
      (path, content.contentHash()), {
        val parsed: Either[Error, (Expr, FileScope)] = content.preParsedAst match {
          case Some(pre) => Right(pre)
          case None      =>
            CachedResolver.parseJsonImport(
              path,
              content,
              internedStrings,
              settings
            ) match {
              case Some(parsedJson) => Right(parsedJson)
              case None             => parseJsonnet(path, content)
            }
        }
        parsed.flatMap { case (e, fs) => process(e, fs) }
      }
    )
  }

  private def parseJsonnet(path: Path, content: ResolvedFile)(implicit
      ev: EvalErrorScope): Either[Error, (Expr, FileScope)] = {
    try {
      fastparse.parse(
        content.getParserInput(),
        parser(path).document(_)
      ) match {
        case f @ Parsed.Failure(_, _, _) =>
          val traced = f.trace()
          val pos = new Position(new FileScope(path), traced.index)
          Left(new ParseError(traced.msg).addFrame(pos))
        case Parsed.Success(r, _) => Right(r)
      }
    } catch {
      case e: ParseError if e.offset >= 0 =>
        val pos = new Position(new FileScope(path), e.offset)
        Left(new ParseError(e.getMessage).addFrame(pos))
      case e: ParseError =>
        Left(e)
    }
  }

  def process(expr: Expr, fs: FileScope): Either[Error, (Expr, FileScope)] = Right((expr, fs))

  /**
   * Creates a parser instance for the given path. This method can be overridden to provide custom
   * parsing behavior.
   *
   * @param path
   *   The path to the file being parsed
   * @return
   *   A Parser instance that will be used to parse the file
   */
  protected def parser(path: Path): Parser = {
    new Parser(path, internedStrings, internedStaticFieldSets, settings)
  }
}

object CachedResolver {
  private final class DuplicateJsonKey extends RuntimeException(null, null, false, false)
  private final class InvalidJsonNumber extends RuntimeException(null, null, false, false)
  private final class JsonParseDepthExceeded extends RuntimeException(null, null, false, false)

  private[sjsonnet] def parseJsonImport(
      path: Path,
      content: ResolvedFile,
      internedStrings: mutable.HashMap[String, String],
      settings: Settings): Option[(Expr, FileScope)] = {
    if (!path.last.endsWith(".json")) return None
    val fileScope = new FileScope(path)
    try {
      val visitor =
        new JsonImportVisitor(fileScope, internedStrings, settings)
      Some((ujson.StringParser.transform(content.readString(), visitor), fileScope))
    } catch {
      case _: ujson.ParsingFailedException | _: DuplicateJsonKey | _: InvalidJsonNumber |
          _: JsonParseDepthExceeded | _: NumberFormatException =>
        None
    }
  }

  private final class JsonImportVisitor(
      fileScope: FileScope,
      internedStrings: mutable.HashMap[String, String],
      settings: Settings)
      extends ujson.JsVisitor[Val, Val] { self =>

    override def visitJsonableObject(length: Int, index: Int): upickle.core.ObjVisitor[Val, Val] =
      visitObject(length, index)

    def visitArray(length: Int, index: Int): upickle.core.ArrVisitor[Val, Val] = {
      enterContainer()
      val startPos = pos(index)
      new upickle.core.ArrVisitor[Val, Val] {
        private val values = new mutable.ArrayBuilder.ofRef[Eval]
        if (length >= 0) values.sizeHint(length)
        def subVisitor: upickle.core.Visitor[?, ?] = self
        def visitValue(v: Val, index: Int): Unit = values += v
        def visitEnd(index: Int): Val = {
          leaveContainer()
          Val.Arr(startPos, values.result())
        }
      }
    }

    def visitObject(length: Int, index: Int): upickle.core.ObjVisitor[Val, Val] = {
      enterContainer()
      val startPos = pos(index)
      new upickle.core.ObjVisitor[Val, Val] {
        private val seen = new util.HashSet[String]()
        private val keys = new mutable.ArrayBuilder.ofRef[String]
        private val members = new mutable.ArrayBuilder.ofRef[Val.Obj.Member]
        if (length >= 0) keys.sizeHint(length)
        if (length >= 0) members.sizeHint(length)
        private var key: String = _
        def subVisitor: upickle.core.Visitor[?, ?] = self
        def visitKey(index: Int): upickle.core.StringVisitor.type = upickle.core.StringVisitor
        def visitKeyValue(s: Any): Unit = key = intern(s.toString)
        def visitValue(v: Val, index: Int): Unit = {
          if (!seen.add(key)) throw new DuplicateJsonKey
          keys += key
          // Imported JSON literals can be shared through ParseCache/Preloader across evaluators.
          // Keep their inline object members immutable by disabling Val.Obj's lazy field cache.
          members += new Val.Obj.ConstMember(
            false,
            Expr.Member.Visibility.Normal,
            v,
            cached2 = false
          )
        }
        def visitEnd(index: Int): Val = {
          val keyArray = keys.result()
          val memberArray = members.result()
          leaveContainer()
          val obj = new Val.Obj(
            startPos,
            null,
            static = false,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            keyArray,
            memberArray
          )
          obj._skipFieldCache = true
          obj
        }
      }
    }

    def visitNull(index: Int): Val = Val.Null(pos(index))
    def visitFalse(index: Int): Val = Val.False(pos(index))
    def visitTrue(index: Int): Val = Val.True(pos(index))

    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Val =
      Val.Num(
        pos(index),
        parseNumber(s)
      )

    def visitString(s: CharSequence, index: Int): Val = {
      val str = s match {
        case str: String => str
        case _           => s.toString
      }
      val unique = intern(str)
      Val.Str(pos(index), unique)
    }

    private def pos(index: Int): Position =
      if (index >= 0) new Position(fileScope, index) else fileScope.noOffsetPos

    private def intern(s: String): String =
      if (s.length > 1024) s else internedStrings.getOrElseUpdate(s, s)

    private def parseNumber(s: CharSequence): Double = {
      val value = s.toString.toDouble
      if (!java.lang.Double.isFinite(value)) throw new InvalidJsonNumber
      value
    }

    private var containerDepth = 0

    private def enterContainer(): Unit = {
      containerDepth += 1
      if (containerDepth > settings.maxParserRecursionDepth) {
        throw new JsonParseDepthExceeded
      }
    }

    private def leaveContainer(): Unit =
      containerDepth -= 1
  }
}
