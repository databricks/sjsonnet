package sjsonnet

import fastparse.ParserInput

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/**
 * A class that encapsulates a resolved import. This is used to cache the result of
 * resolving an import. If the import is deemed too large (IE it's a large file), then we will avoid keeping it in
 * memory and instead will re-read it from disk.
 *
 * @param resolvedImportPath The path of the file on disk that was resolved
 * @param memoryLimitBytes The maximum size of a file that we will resolve. This is not the size of
 * the buffer, but a mechanism to fail when being asked to resolve (and downstream parse) a file
 * that is beyond this limit.
 * @param cacheThresholdBytes The maximum size of a file that we will cache in memory. If the file
 * is larger than this, then we will serve it from disk
 */
class CachedResolvedFile(val resolvedImportPath: OsPath, memoryLimitBytes: Long, cacheThresholdBytes: Long = 1024 * 1024, binaryData: Boolean = false) extends ResolvedFile {

  private val jFile: File = resolvedImportPath.p.toIO

  assert(jFile.exists(), s"Resolved import path $resolvedImportPath does not exist")
  // Assert that the file is less than limit
  assert(jFile.length() <= memoryLimitBytes, s"Resolved import path $resolvedImportPath is too large: ${jFile.length()} bytes > ${memoryLimitBytes} bytes")

  private val resolvedImportContent: ResolvedFile = {
    // TODO: Support caching binary data
    if (jFile.length() > cacheThresholdBytes) {
      // If the file is too large, then we will just read it from disk
      null
    } else if (binaryData) {
      StaticBinaryResolvedFile(readRawBytes(jFile))
    } else {
      StaticResolvedFile(readString(jFile))
    }
  }

  private def readString(jFile: File): String = {
    new String(Files.readAllBytes(jFile.toPath), StandardCharsets.UTF_8);
  }

  private def readRawBytes(jFile: File): Array[Byte] = Files.readAllBytes(jFile.toPath)

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


  override def contentHash(): String = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      Platform.hashFile(jFile)
    } else {
      resolvedImportContent.contentHash()
    }
  }

  override def readRawBytes(): Array[Byte] = {
    if (resolvedImportContent == null) {
      // If the file is too large, then we will just read it from disk
      readRawBytes(jFile)
    } else {
      // Otherwise, we will read it from memory
      resolvedImportContent.readRawBytes()
    }
  }
}

