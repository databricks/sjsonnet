package sjsonnet

import java.io.{BufferedInputStream, ByteArrayInputStream, File, FileInputStream, InputStream, RandomAccessFile}
import java.nio.file.Files
import java.security.MessageDigest
import scala.collection.mutable
import scala.util.Try
import fastparse.{IndexedParserInput, Parsed, ParserInput}

import java.nio.charset.StandardCharsets


/** Resolve and read imported files */
abstract class Importer {
  def resolve(docBase: Path, importName: String): Option[Path]

  def read(path: Path): Option[ResolvedFile]

  def resolveAndRead(docBase: Path, importName: String): Option[(Path, ResolvedFile)] = for {
    path <- resolve(docBase, importName)
    txt <- read(path)
  } yield (path, txt)

  def resolveAndReadOrFail(value: String, pos: Position)(implicit ev: EvalErrorScope): (Path, ResolvedFile) =
    resolveAndRead(pos.fileScope.currentFile.parent(), value)
      .getOrElse(Error.fail("Couldn't import file: " + pprint.Util.literalize(value), pos))
}

object Importer {
  val empty: Importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] = None
    def read(path: Path): Option[ResolvedFile] = None
  }
}

case class FileParserInput(file: RandomAccessFile) extends ParserInput {
  private lazy val fileLength = file.length.toInt

  override def apply(index: Int): Char = {
    file.seek(index)
    file.read().toChar
  }

  override def dropBuffer(index: Int): Unit = {}

  override def slice(from: Int, until: Int): String = {
    file.seek(from)
    val length = until - from
    val bytes = new Array[Byte](length)
    file.readFully(bytes)
    new String(bytes)
  }

  override def length: Int = fileLength

  override def innerLength: Int = length

  override def isReachable(index: Int): Boolean = index < length

  override def checkTraceable(): Unit = {}

  private[this] lazy val lineNumberLookup: Array[Int] = {
    val lines = mutable.ArrayBuffer[Int]()
    Try {
      val bufferedStream = new BufferedInputStream(new RandomAccessFileInputStream(file))
      var byteRead: Int = 0
      var currentPosition = 0

      while ({ byteRead = bufferedStream.read(); byteRead != -1 }) {
        if (byteRead == '\n') {
          lines += currentPosition + 1
        }
        currentPosition += 1
      }

      bufferedStream.close()
    }

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

  // Helper class to convert RandomAccessFile to InputStream
  private class RandomAccessFileInputStream(raf: RandomAccessFile) extends java.io.InputStream {
    override def read(): Int = raf.read()
  }
}

trait ResolvedFile {
  def getParserInput(): ParserInput

  def readString(): String

  def contentHash(): String
}

case class StaticResolvedFile(content: String) extends ResolvedFile {
  // This is probably stupid, but it's the easiest way to get an InputStream from a String
  def getParserInput(): ParserInput = IndexedParserInput(content)

  def readString(): String = content

  // We just cheat, the content hash can be the content itself for static imports
  override def contentHash(): String = content
}

/**
 * A class that encapsulates a resolved import. This is used to cache the result of
 * resolving an import. If the import is deemed too large (IE it's a large file), then we will avoid keeping it in
 * memory and instead will re-read it from disk.
 */
class CachedResolvedFile(val resolvedImportPath: OsPath) extends ResolvedFile {

  private val jFile: File = resolvedImportPath.p.toIO

  assert(jFile.exists(), s"Resolved import path ${resolvedImportPath} does not exist")

  private[this] val resolvedImportContent: StaticResolvedFile = {
    if (jFile.length() > 1024 * 1024) {
      // If the file is too large, then we will just read it from disk
      null
    } else {
      StaticResolvedFile(Files.readString(jFile.toPath))
    }
  }

  /**
   * A method that will return a reader for the resolved import. If the import is too large, then this will return
   * a reader that will read the file from disk. Otherwise, it will return a reader that reads from memory.
   */
  def getParserInput(): ParserInput = {
    if (resolvedImportContent == null) {
      FileParserInput(new RandomAccessFile(jFile, "r"))
    } else {
      resolvedImportContent.getParserInput()
    }
  }

  override def readString(): String = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      Files.readString(jFile.toPath)
    } else {
      // Otherwise, we will read it from memory
      resolvedImportContent.readString()
    }
  }

  private def md5HashFile(filePath: String): String = {
    val buffer = new Array[Byte](8192)
    val md5 = MessageDigest.getInstance("MD5")

    val fis = new FileInputStream(new File(filePath))
    val bis = new BufferedInputStream(fis)

    try {
      var bytesRead = bis.read(buffer)
      while (bytesRead != -1) {
        md5.update(buffer, 0, bytesRead)
        bytesRead = bis.read(buffer)
      }
    } finally {
      bis.close()
      fis.close()
    }

    md5.digest().map("%02x".format(_)).mkString
  }

  // SHA1 hash of the resolved import content
  override def contentHash(): String = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      md5HashFile(jFile.getAbsolutePath)
    } else {
      resolvedImportContent.contentHash()
    }
  }
}

class CachedImporter(parent: Importer) extends Importer {
  val cache = mutable.HashMap.empty[Path, ResolvedFile]

  def resolve(docBase: Path, importName: String): Option[Path] = parent.resolve(docBase, importName)

  def read(path: Path): Option[ResolvedFile] = cache.get(path) match {
    case s @ Some(x) =>
      if(x == null) None else s
    case None =>
      val x = parent.read(path)
      cache.put(path, x.getOrElse(null))
      x
  }
}

class CachedResolver(
  parentImporter: Importer,
  val parseCache: ParseCache,
  strictImportSyntax: Boolean) extends CachedImporter(parentImporter) {

  def parse(path: Path, content: ResolvedFile)(implicit ev: EvalErrorScope): Either[Error, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate((path, content.contentHash()), {
      val parsed = fastparse.parse(content.getParserInput(), new Parser(path, strictImportSyntax).document(_)) match {
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
