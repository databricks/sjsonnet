package sjsonnet

import java.nio.file.{Files, Path => JavaPath}

import scala.util.Random

import net.jpountz.xxhash.{StreamingXXHash64, XXHashFactory, XXHash64}

import utest._
import TestUtils.eval

object XxHash64Tests extends TestSuite {
  val tests = Tests {

    test("xxhash") {
      for (sizeInKb <- List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)) {
        val (randomContent, tempFilePath) = generateRandomContentAndSaveToFile(sizeInKb)
        val xxHash64 = XXHashFactory.fastestInstance().hash64()
        // Use the non-streaming version of xxHash64 to hash the whole byte array
        val xxHash64Result = xxHash64.hash(randomContent, 0, randomContent.length, 0).toString
        // Then use the streaming version of xxHash64 to hash the file in chunks
        val cachedFile = new CachedResolvedFile(
          OsPath(os.Path(tempFilePath)),
          memoryLimitBytes = Int.MaxValue,
          cacheThresholdBytes = 0)
        // They should agree
        val hash = cachedFile.contentHash
        assert(xxHash64Result == hash)
      }
    }
  }

  private def generateRandomContentAndSaveToFile(sizeInKb: Int): (Array[Byte], JavaPath) = {
    val random = new Random()
    val byteArraySize = 1024 * sizeInKb
    val randomContent = new Array[Byte](byteArraySize)
    random.nextBytes(randomContent)

    val tempFilePath = Files.createTempFile("randomContent", ".tmp")
    Files.write(tempFilePath, randomContent)

    (randomContent, tempFilePath)
  }
}

