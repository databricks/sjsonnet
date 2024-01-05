package sjsonnet

import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.CRC32
import fastparse.ParserInput

/**
 * A class that encapsulates a resolved import. This is used to cache the result of
 * resolving an import. If the import is deemed too large (IE it's a large file), then we will avoid keeping it in
 * memory and instead will re-read it from disk.
 *
 * @param resolvedImportPath The path of the file on disk that was resolved
 * @param memoryLimitBytes The maximum size of a file that we will resolve. This is not the size of
 * the buffer, but a mechanism to fail when being asked to resolve (and downstream parse) a file
 * that is beyond this limit.
 */
class CachedResolvedFile(val resolvedImportPath: OsPath, memoryLimitBytes: Long) extends ResolvedFile {

  private val jFile: File = resolvedImportPath.p.toIO

  assert(jFile.exists(), s"Resolved import path ${resolvedImportPath} does not exist")
  // Assert that the file is less than limit
  assert(jFile.length() <= memoryLimitBytes, s"Resolved import path ${resolvedImportPath} is too large: ${jFile.length()} bytes > ${memoryLimitBytes} bytes")

  private[this] val resolvedImportContent: StaticResolvedFile = {
    if (jFile.length() > 1024 * 1024) {
      // If the file is too large, then we will just read it from disk
      null
    } else {
      StaticResolvedFile(readString(jFile))
    }
  }

  private[this] def readString(jFile: File): String = {
    new String(Files.readAllBytes(jFile.toPath), StandardCharsets.UTF_8);
  }

  /**
   * A method that will return a reader for the resolved import. If the import is too large, then this will return
   * a reader that will read the file from disk. Otherwise, it will return a reader that reads from memory.
   */
  def getParserInput(): ParserInput = {
    if (resolvedImportContent == null) {
      FileParserInput(jFile)
    } else {
      resolvedImportContent.getParserInput()
    }
  }

  override def readString(): String = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      readString(jFile)
    } else {
      // Otherwise, we will read it from memory
      resolvedImportContent.readString()
    }
  }

  private def crcHashFile(file: File): Long = {
    val buffer = new Array[Byte](8192)
    val crc = new CRC32()

    val fis = new FileInputStream(file)
    val bis = new BufferedInputStream(fis)

    try {
      var bytesRead = bis.read(buffer)
      while (bytesRead != -1) {
        crc.update(buffer, 0, bytesRead)
        bytesRead = bis.read(buffer)
      }
    } finally {
      bis.close()
      fis.close()
    }

    crc.getValue()
  }

  override lazy val contentHash: String = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      crcHashFile(jFile).toString
    } else {
      resolvedImportContent.contentHash
    }
  }
}
