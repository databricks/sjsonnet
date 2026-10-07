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

final class StaticBinaryResolvedFile(content0: Array[Byte])
    extends ResolvedFile
    with Product
    with Serializable {
  private val bytes: Array[Byte] = content0.clone()

  def getParserInput(): ParserInput = throw new NotImplementedError("Not used for binary imports")

  def readString(): String = throw new NotImplementedError("Not used for binary imports")

  def content: Array[Byte] = bytes

  private lazy val contentHashValue: String = Platform.hashBytes(bytes)

  def contentHash(): String = contentHashValue

  override def readRawBytes(): Array[Byte] = bytes

  def copy(content: Array[Byte] = this.content): StaticBinaryResolvedFile =
    new StaticBinaryResolvedFile(content)

  def productArity: Int = 1

  def productElement(n: Int): Any =
    if (n == 0) content else throw new IndexOutOfBoundsException(n.toString)

  def canEqual(that: Any): Boolean = that.isInstanceOf[StaticBinaryResolvedFile]

  override def productPrefix: String = "StaticBinaryResolvedFile"

  override def equals(that: Any): Boolean = that match {
    case other: StaticBinaryResolvedFile =>
      (this eq other) || (other.canEqual(this) && java.util.Arrays.equals(bytes, other.bytes))
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

  override def toString: String = productIterator.mkString(productPrefix + "(", ",", ")")
}

object StaticBinaryResolvedFile
    extends scala.runtime.AbstractFunction1[Array[Byte], StaticBinaryResolvedFile]
    with Serializable {
  def apply(content: Array[Byte]): StaticBinaryResolvedFile =
    new StaticBinaryResolvedFile(content)

  def unapply(file: StaticBinaryResolvedFile): Option[Array[Byte]] =
    if (file == null) None else Some(file.content)
}

class CachedImporter(parent: Importer) extends Importer {
  val cache: Platform.ImporterFileCacheMap =
    Platform.newImporterFileCacheMap()

  // Memoize path resolution by (docBase, importName). resolve() runs on every visitImport — before
  // the evaluator's by-path Val cache is consulted — and each call stats candidate paths
  // (docBase + every jpath) via os.isFile. Resolution is deterministic within a run, so caching it
  // turns repeated imports (and re-evaluated import exprs) into a HashMap lookup, eliminating
  // redundant filesystem stats. Mirrors the existing read cache.
  private val resolveCache: Platform.ImporterResolveCacheMap =
    Platform.newImporterResolveCacheMap()

  def resolve(docBase: Path, importName: String): Option[Path] = {
    val key = (docBase, importName)
    resolveCache.get(key) match {
      case Some(v) => v
      case None    =>
        val v = parent.resolve(docBase, importName)
        resolveCache.putIfAbsent(key, v).getOrElse(v)
    }
  }

  def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
    val key = (path, binaryData)
    cache.get(key) match {
      case s @ Some(x) =>
        if (x eq CachedImporter.MissingResolvedFile) None else s
      case None =>
        val x = parent.read(path, binaryData)
        val cachedValue = x.getOrElse(CachedImporter.MissingResolvedFile)
        cache.putIfAbsent(key, cachedValue) match {
          case s @ Some(existing) =>
            if (existing eq CachedImporter.MissingResolvedFile) None else s
          case None => x
        }
    }
  }
}

object CachedImporter {
  private object MissingResolvedFile extends ResolvedFile {
    def getParserInput(): ParserInput =
      throw new IllegalStateException("missing resolved file cache sentinel")
    def readString(): String =
      throw new IllegalStateException("missing resolved file cache sentinel")
    def contentHash(): String =
      throw new IllegalStateException("missing resolved file cache sentinel")
    def readRawBytes(): Array[Byte] =
      throw new IllegalStateException("missing resolved file cache sentinel")
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
    try {
      parseCache.getOrElseUpdate(
        (path, content.contentHash()), {
          val parsed: Either[Error, (Expr, FileScope)] = content.preParsedAst match {
            case Some(pre) => Right(pre)
            case None      =>
              CachedResolver.parseJsonImportOrNull(
                path,
                content,
                internedStrings,
                settings
              ) match {
                case null       => parseJsonnet(path, content)
                case parsedJson => Right(parsedJson)
              }
          }
          parsed.flatMap { case (e, fs) => process(e, fs) }
        }
      )
    } catch {
      case e: CachedResolver.InvalidJsonUnicode =>
        val pos =
          if (e.offset >= 0) new Position(e.fileScope, e.offset)
          else e.fileScope.noOffsetPos
        // Named frames (e.g. <root>) hide unnamed position frames in Error.formatError,
        // so surface the position in the message like fastparse ParseErrors do.
        val where = ev.prettyIndex(pos) match {
          case Some((line, col)) => s" at line $line column $col"
          case None              => ""
        }
        Left(new ParseError(CachedResolver.InvalidJsonUnicodeMessage + where).addFrame(pos))
    }
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
  private[sjsonnet] final class InvalidJsonUnicode(val fileScope: FileScope, val offset: Int = -1)
      extends RuntimeException(null, null, false, false)

  private[sjsonnet] val InvalidJsonUnicodeMessage = "Invalid JSON: unpaired surrogate in string"

  /**
   * Parses strict `.json` imports through ujson's parser. Returns null when this fast path should
   * fall back to the Jsonnet parser; the nullable result avoids Some/None allocations in the import
   * hot path without adding a reusable OptionVal abstraction to this semantic fix.
   */
  private[sjsonnet] def parseJsonImportOrNull(
      path: Path,
      content: ResolvedFile,
      internedStrings: mutable.HashMap[String, String],
      settings: Settings): (Expr, FileScope) = {
    if (!path.last.endsWith(".json")) return null
    val fileScope = new FileScope(path)
    val bytes = content.readRawBytes()
    try {
      val visitor =
        new JsonImportVisitor(fileScope, internedStrings, settings)
      val expr = ujson.ByteArrayParser.transform(bytes, visitor)
      rejectUnpairedSurrogateEscapes(bytes, fileScope)
      (expr, fileScope)
    } catch {
      case e: InvalidJsonUnicode =>
        // Scanner and visitor report byte offsets; Position offsets are char indexes.
        if (e.offset >= 0) throw new InvalidJsonUnicode(fileScope, charOffset(bytes, e.offset))
        else throw e
      case _: ValVisitor.InvalidUnicodeString =>
        throw new InvalidJsonUnicode(fileScope)
      case e: Exception if isUnpairedSurrogateParserError(e) =>
        throw new InvalidJsonUnicode(fileScope)
      case _: ujson.ParsingFailedException | _: DuplicateJsonKey | _: InvalidJsonNumber |
          _: JsonParseDepthExceeded | _: NumberFormatException =>
        null
    }
  }

  private final val Backslash = '\\'.toByte
  private final val Quote = '"'.toByte
  private final val U = 'u'.toByte
  private final val UpperD = 'D'.toByte
  private final val LowerD = 'd'.toByte
  private final val Zero = '0'.toByte
  private final val Nine = '9'.toByte
  private final val UpperA = 'A'.toByte
  private final val UpperF = 'F'.toByte
  private final val LowerA = 'a'.toByte
  private final val LowerF = 'f'.toByte

  // Scans raw bytes for \uD800-\uDFFF escape sequences, catching unpaired surrogates even if
  // ujson normalizes them to U+FFFD before calling visitString (where the visitor check would miss).
  private def rejectUnpairedSurrogateEscapes(bytes: Array[Byte], fileScope: FileScope): Unit = {
    var i = 0
    var foundPotentialSurrogateEscape = false
    while (i + 5 < bytes.length && !foundPotentialSurrogateEscape) {
      foundPotentialSurrogateEscape =
        bytes(i) == Backslash && bytes(i + 1) == U && isD(bytes(i + 2))
      i += 1
    }
    if (!foundPotentialSurrogateEscape) return

    i = 0
    var inString = false
    var escaped = false
    while (i < bytes.length) {
      val b = bytes(i)
      if (!inString) {
        if (b == Quote) inString = true
        i += 1
      } else if (escaped) {
        if (b == U && i + 4 < bytes.length && isHex4(bytes, i + 1)) {
          val code = hex4(bytes, i + 1)
          if (Character.isHighSurrogate(code.toChar)) {
            val nextEscape = i + 5
            if (
              nextEscape + 6 > bytes.length || bytes(nextEscape) != Backslash ||
              bytes(nextEscape + 1) != U || !isHex4(bytes, nextEscape + 2) ||
              !Character.isLowSurrogate(hex4(bytes, nextEscape + 2).toChar)
            ) {
              throw new InvalidJsonUnicode(fileScope, i - 1)
            }
            i = nextEscape + 6
          } else if (Character.isLowSurrogate(code.toChar)) {
            throw new InvalidJsonUnicode(fileScope, i - 1)
          } else {
            i += 5
          }
        } else {
          i += 1
        }
        escaped = false
      } else if (b == Backslash) {
        escaped = true
        i += 1
      } else {
        if (b == Quote) inString = false
        i += 1
      }
    }
  }

  // Position offsets are interpreted as char indexes into the decoded file (see
  // EvalErrorScope.prettyIndex), so translate the scanner's byte index. Error path only.
  private def charOffset(bytes: Array[Byte], byteOffset: Int): Int =
    new String(bytes, 0, byteOffset, java.nio.charset.StandardCharsets.UTF_8).length

  private def isD(b: Byte): Boolean = b == UpperD || b == LowerD

  private def isHex4(bytes: Array[Byte], offset: Int): Boolean =
    isHex(bytes(offset)) && isHex(bytes(offset + 1)) && isHex(bytes(offset + 2)) &&
    isHex(bytes(offset + 3))

  private def isHex(b: Byte): Boolean =
    (b >= Zero && b <= Nine) || (b >= UpperA && b <= UpperF) ||
    (b >= LowerA && b <= LowerF)

  private def hex4(bytes: Array[Byte], offset: Int): Int =
    (hex(bytes(offset)) << 12) | (hex(bytes(offset + 1)) << 8) |
    (hex(bytes(offset + 2)) << 4) | hex(bytes(offset + 3))

  private def hex(b: Byte): Int =
    if (b <= Nine) b - Zero
    else if (b <= UpperF) b - UpperA + 10
    else b - LowerA + 10

  // Last-resort catch for ujson's own surrogate validation (upickle#722).
  // Fragile: depends on ujson throwing plain Exception with "Un-paired ... surrogate ..." message.
  // Primary defenses are ValVisitor.rejectUnpairedSurrogates + rejectUnpairedSurrogateEscapes above.
  private[sjsonnet] def isUnpairedSurrogateParserError(e: Exception): Boolean =
    e.getClass == classOf[Exception] &&
    e.getMessage != null &&
    e.getMessage.startsWith("Un-paired ") &&
    e.getMessage.contains(" surrogate ")

  private final class JsonImportVisitor(
      fileScope: FileScope,
      internedStrings: mutable.HashMap[String, String],
      settings: Settings)
      extends ujson.JsVisitor[Val, Val] { self =>
    private val jsonPos = fileScope.noOffsetPos

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
        private var keyIndex: Int = -1
        def subVisitor: upickle.core.Visitor[?, ?] = self
        def visitKey(index: Int): upickle.core.StringVisitor.type = {
          keyIndex = index
          upickle.core.StringVisitor
        }
        def visitKeyValue(s: Any): Unit = {
          val str = s.toString
          try ValVisitor.rejectUnpairedSurrogates(str)
          catch {
            case _: ValVisitor.InvalidUnicodeString =>
              throw new InvalidJsonUnicode(fileScope, keyIndex)
          }
          key = intern(str)
        }
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
          if (keyArray.length > 1)
            obj._sortedInlineOrder = Materializer.computeSortedInlineOrder(keyArray, memberArray)
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
      try ValVisitor.rejectUnpairedSurrogates(str)
      catch {
        case _: ValVisitor.InvalidUnicodeString =>
          throw new InvalidJsonUnicode(fileScope, index)
      }
      val unique = intern(str)
      Val.Str(pos(index), unique)
    }

    private def pos(index: Int): Position = jsonPos

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
