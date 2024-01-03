package sjsonnet

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, BufferedInputStream, BufferedOutputStream}
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Paths, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.GZIPOutputStream
import java.util.{Arrays, Base64, EnumSet}

import org.json.JSONObject
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import com.github.blemale.scaffeine.{Cache, Scaffeine}

object Platform {
  def gzipBytes(b: Array[Byte]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    val gzip: GZIPOutputStream = new GZIPOutputStream(outputStream)
    gzip.write(b)
    gzip.close()
    val gzippedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    gzippedBase64
  }
  def gzipString(s: String): String = {
    gzipBytes(s.getBytes())
  }

  private def withTime[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    (result, end - start)
  }

  private case class CacheKey(b: Array[Byte], compressionLevel: Option[Int]) {
    override lazy val hashCode: Int = {
      Arrays.hashCode(b) + compressionLevel.hashCode()
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case CacheKey(b2, compressionLevel2) =>
          compressionLevel == compressionLevel2 && Arrays.equals(b, b2)
        case _ =>
          false
      }
    }
  }

  private lazy val xzCache: Cache[CacheKey, String] = {
    Scaffeine()
      .recordStats()
      // Use weakValues() to allow the cache to be garbage collected when a value is no longer in
      // use
      .weakValues()
      .weigher(cacheWeigher)
      // Max cache size is 100 MB
      .maximumWeight(100 * 1024 * 1024)
      .build()
  }

  private def cacheWeigher(k: CacheKey, v: String): Int = {
    // The size of the cache is the sum of the sizes of the keys and values
    k.b.length + v.length
  }

  /**
   *  Valid compression levels are 0 (no compression) to 9 (maximum compression).
   */
  def xzBytes(b: Array[Byte], compressionLevel: Option[Int]): String = {
    // For a given byte array and compression level, check the cache to see if we've already
    // compressed it
    val (compressed, duration) = withTime {
      xzCache.get(CacheKey(b, compressionLevel), { _ =>
        xzBytesUncached(b, compressionLevel)
      })
    }
    // Print the time it took to compress with XZ
    System.err.println(s"XZ: $duration ms")
    compressed
  }

  private def xzBytesUncached(b: Array[Byte], compressionLevel: Option[Int]): String = {
    // val (xzedBase64Length: Int, time1) = withTime {
    //   val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    //   // Set compression to specified level
    //   val level = compressionLevel.getOrElse(LZMA2Options.PRESET_DEFAULT)
    //   val xz: XZOutputStream = new XZOutputStream(outputStream, new LZMA2Options(level))
    //   xz.write(b)
    //   xz.close()
    //   val xzedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    //   outputStream.close()
    //   xzedBase64.length
    // }
    val (zcompressed: String, time2) = withTime {
      val compressed = compressWith7z(b, compressionLevel)
      val xzedBase64: String = Base64.getEncoder.encodeToString(compressed)
      xzedBase64
    }
    // Print the time it took to compress with XZ and 7z
    // Print the ratio of the two compressed sizes
    // System.err.println(s"XZ: $time1 ms, 7z: $time2 ms")
    // System.err.println(s"XZ: $xzedBase64Length, 7z: ${zcompressed.length}")
    // System.err.println(s"Ratio: ${xzedBase64Length.toDouble / zcompressed.length}")
    System.err.println(s"7zip: $time2 ms ${b.length} -> ${zcompressed.length}")
    zcompressed
  }

  def xzString(s: String, compressionLevel: Option[Int]): String = {
    xzBytes(s.getBytes(), compressionLevel)
  }

  def compressWith7z(inputBytes: Array[Byte], compressionLevel: Option[Int]): Array[Byte] = {
    // Temporary files for input and output
    val inputFile = Files.createTempFile("input", ".tmp")
    val outputPath = inputFile.toString + ".xz"

    try {
      // Write the input bytes to the input file
      Files.write(inputFile, inputBytes)

      // Prepare the 7z command
      val compressionLevelOpt = compressionLevel.map(l => s"-mx=$l")
      val command = List(sevenZipExecutablePath.toString, "a", "-txz") ++ compressionLevelOpt ++ List(outputPath, inputFile.toString)
      val pb = new ProcessBuilder(command: _*)
      val process = pb.start()
      process.waitFor()
      // If the process failed, throw an exception with the stdout and stderr
      if (process.exitValue() != 0) {
        val stdout = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val stderr = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        throw new Exception(s"7z failed with exit code ${process.exitValue()}\nstdout:\n$stdout\nstderr:\n$stderr")
      }

      // Read the compressed output
      Files.readAllBytes(Paths.get(outputPath))
    } finally {
      // Clean up temporary files
      Files.deleteIfExists(inputFile)
      Files.deleteIfExists(Paths.get(outputPath))
    }
  }

  def yamlToJson(yamlString: String): String = {
    val yaml: java.util.LinkedHashMap[String, Object] = new Yaml(new Constructor(classOf[java.util.LinkedHashMap[String, Object]])).load(yamlString)
    new JSONObject(yaml).toString()
  }
  def md5(s: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map{ b => String.format("%02x", new java.lang.Integer(b & 0xff))}
      .mkString
  }

  private lazy val sevenZipExecutablePath: Path = {
    val osName = System.getProperty("os.name").toLowerCase
    val osArch = System.getProperty("os.arch")

    val executableName = (osName, osArch) match {
      case (name, arch) if name.contains("win") => "/7z/windows/7z.exe"
      case (name, arch) if name.contains("mac") => "/7z/osx/7z"
      case (name, arch) if arch.contains("amd64") || arch.contains("x86_64") => "/7z/linux/7z-x86"
      case _ => "/7z/linux/7z-arm" // Assuming ARM for all other cases
    }

    val resourceStream = getClass.getResourceAsStream(executableName)
    val tempFile = Files.createTempFile("7z", if (osName.contains("win")) ".exe" else "")
    Files.copy(resourceStream, tempFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING)

    if (!osName.contains("win")) {
      val perms = EnumSet.of(
        PosixFilePermission.OWNER_READ,
        PosixFilePermission.OWNER_WRITE,
        PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.GROUP_READ,
        PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.OTHERS_READ,
        PosixFilePermission.OTHERS_EXECUTE
      )
      Files.setPosixFilePermissions(tempFile, perms)
    }

    tempFile
  }
}
