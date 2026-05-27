package sjsonnet

import java.io.OutputStream
import scala.scalanative.libc.stdio
import scala.scalanative.libc.stdio.FILE
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

/**
 * Direct fwrite-based OutputStream for Scala Native, bypassing the JVM compat chain:
 * PrintStream.write (synchronized) → FileOutputStream → FileChannelImpl → unistd.write
 *
 * Uses stdio.fwrite which has internal C library buffering, avoiding per-call syscall overhead.
 */
class NativeOutputStream(file: Ptr[FILE]) extends OutputStream {
  stdio.setvbuf(file, null, stdio._IOFBF, NativeOutputStream.BufferSize.toUSize)

  override def write(b: Int): Unit =
    stdio.fputc(b, file)

  override def write(buf: Array[Byte], off: Int, len: Int): Unit =
    if (len > 0) {
      stdio.fwrite(buf.at(off), 1.toUSize, len.toUSize, file)
    }

  override def flush(): Unit =
    stdio.fflush(file)

  override def close(): Unit =
    flush()
}

object NativeOutputStream {
  private final val BufferSize = 256 * 1024
}
