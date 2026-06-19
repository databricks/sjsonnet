package sjsonnet

import java.io._
import java.net.StandardProtocolFamily
import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters._
import sjsonnet.client.{Lock, Locks, ProxyOutputStream, Util => ClientUtil}
import sun.misc.{Signal, SignalHandler}

import scala.util.control.NonFatal

trait SjsonnetServerMain[T] {
  var stateCache = Option.empty[T]
  def main0(
      args: Array[String],
      stateCache: Option[T],
      mainInteractive: Boolean,
      stdin: InputStream,
      stdout: PrintStream,
      stderr: PrintStream,
      env: Map[String, String],
      setIdle: Boolean => Unit,
      wd: os.Path): (Boolean, Option[T])
}

object SjsonnetServerRealMain {
  def main(args0: Array[String]): Unit = {
    SjsonnetServerMain.internalMain(args0)
  }
}

object SjsonnetServerMain extends SjsonnetServerMain[DefaultParseCache] {
  def internalMain(args0: Array[String]): Unit = {
    // Disable SIGINT interrupt signal in the Mill server.
    //
    // This gets passed through from the client to server whenever the user
    // hits `Ctrl-C`, which by default kills the server, which defeats the purpose
    // of running a background server. Furthermore, the background server already
    // can detect when the Mill client goes away, which is necessary to handle
    // the case when a Mill client that did *not* spawn the server gets `CTRL-C`ed
    Signal.handle(
      new Signal("INT"),
      new SignalHandler {
        def handle(sig: Signal): Unit = {} // do nothing
      }
    )
    new Server(
      args0(0),
      this,
      () => System.exit(0),
      300000,
      sjsonnet.client.Locks.files(args0(0))
    ).run()
  }
  def main0(
      args: Array[String],
      stateCache: Option[DefaultParseCache],
      mainInteractive: Boolean,
      stdin: InputStream,
      stdout: PrintStream,
      stderr: PrintStream,
      env: Map[String, String],
      setIdle: Boolean => Unit,
      wd: os.Path): (Boolean, Option[DefaultParseCache]) = {

    val stateCache2 = stateCache.getOrElse {
      val p = new DefaultParseCache
      this.stateCache = Some(p)
      p
    }
    val out = System.out
    val in = System.in
    val err = System.err
    (
      try {
        System.setIn(stdin)
        System.setErr(stderr)
        System.setOut(stdout)
        scala.Console.withIn(stdin) {
          scala.Console.withOut(stdout) {
            scala.Console.withErr(stderr) {
              sjsonnet.SjsonnetMainBase.main0(args, stateCache2, stdin, stdout, stderr, wd) == 0
            }
          }
        }
      } finally {
        System.setErr(err)
        System.setOut(out)
        System.setIn(in)
      },
      Some(stateCache2)
    )
  }
}

class Server[T](
    lockBase: String,
    sm: SjsonnetServerMain[T],
    interruptServer: () => Unit,
    acceptTimeout: Int,
    locks: Locks) {

  val originalStdout: PrintStream = System.out
  def run(): Unit = {
    Server
      .tryLockBlock(locks.processLock) {
        var running = true
        while (running) {
          Server.lockBlock(locks.serverLock) {
            val socketPath = Paths.get(lockBase, "io")
            Files.deleteIfExists(socketPath)
            val addr = UnixDomainSocketAddress.of(socketPath)
            val serverSocket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
            try {
              serverSocket.bind(addr)

              val socketClose = () => {
                val ch = SocketChannel.open(StandardProtocolFamily.UNIX)
                try ch.connect(addr) finally ch.close()
              }

              val sockOpt = Server.interruptWith(
                "SjsonnetSocketTimeoutInterruptThread",
                acceptTimeout,
                socketClose(),
                serverSocket.accept()
              )

              sockOpt match {
                case None       => running = false
                case Some(sock) =>
                  try {
                    handleRun(sock)
                  } catch { case NonFatal(e) => e.printStackTrace(originalStdout) }
              }
            } finally {
              serverSocket.close()
            }
          }
          // Make sure you give an opportunity for the client to probe the lock
          // and realize the server has released it to signal completion
          Thread.sleep(10)
        }
      }
      .getOrElse(throw new Exception("PID already present"))
  }

  def handleRun(clientSocket: SocketChannel): Unit = {

    val currentOutErr = Channels.newOutputStream(clientSocket)
    val stdout = new PrintStream(new ProxyOutputStream(currentOutErr, 1), true)
    val stderr = new PrintStream(new ProxyOutputStream(currentOutErr, -1), true)
    val socketIn = Channels.newInputStream(clientSocket)
    val (interactive, clientSjsonnetVersion, wd, args, env) = {
      val argStream = new FileInputStream(lockBase + "/run")
      try {
        val i = argStream.read() != 0
        val cv = ClientUtil.readString(argStream)
        val w = ClientUtil.readString(argStream)
        val a = ClientUtil.parseArgs(argStream)
        val e = ClientUtil.parseMap(argStream)
        (i, cv, w, a, e)
      } finally argStream.close()
    }
    val serverSjsonnetVersion = sys.props("SJSONNET_VERSION")
    if (clientSjsonnetVersion != serverSjsonnetVersion) {
      stdout.println(
        s"Sjsonnet version changed ($serverSjsonnetVersion -> $clientSjsonnetVersion), re-starting server"
      )
      System.exit(0)
    }

    @volatile var done = false
    @volatile var idle = false
    val t = new Thread(
      new Runnable {
        override def run(): Unit = {
          try {
            val (result, newStateCache) = sm.main0(
              args,
              sm.stateCache,
              interactive,
              socketIn,
              stdout,
              stderr,
              env.asScala.toMap,
              idle = _,
              os.Path(wd)
            )

            sm.stateCache = newStateCache
            Files.write(
              Paths.get(lockBase, "exitCode"),
              (if (result) 0 else 1).toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
            )
          } finally {
            done = true
            idle = true
          }
        }
      },
      "SjsonnetServerActionRunner"
    )
    t.start()
    // We cannot simply use Lock#await here, because the filesystem doesn't
    // realize the clientLock/serverLock are held by different threads in the
    // two processes and gives a spurious deadlock error
    while (!done && !locks.clientLock.probe()) Thread.sleep(3)

    if (!idle) interruptServer()

    t.interrupt()
    // Thread.stop() is removed in Java 25+; try it for older JVMs but ignore failures
    try t.stop()
    catch { case NonFatal(_) => }

    if (ClientUtil.isWindows) {
      // Closing the socket on Windows can sometimes take a few seconds.
      // It seems OK to exit the client early and subsequently
      // start up sjsonnet client again.
      val t = new Thread(() => clientSocket.close())
      t.setDaemon(true)
      t.start()
    } else clientSocket.close()
  }
}
object Server {
  def lockBlock[T](lock: Lock)(t: => T): T = {
    val l = lock.lock()
    try t
    finally l.release()
  }
  def tryLockBlock[T](lock: Lock)(t: => T): Option[T] = {
    lock.tryLock() match {
      case null => None
      case l    =>
        try Some(t)
        finally l.release()
    }

  }
  def interruptWith[T](threadName: String, millis: Int, close: => Unit, t: => T): Option[T] = {
    @volatile var interrupt = true
    @volatile var interrupted = false
    val thread = new Thread(
      () => {
        try Thread.sleep(millis)
        catch {
          case t: InterruptedException => /* Do Nothing */
        }
        if (interrupt) {
          interrupted = true
          close
        }
      },
      threadName
    )

    thread.start()
    try {
      val res =
        try Some(t)
        catch { case NonFatal(e) => None }

      if (interrupted) None
      else res

    } finally {
      thread.interrupt()
      interrupt = false
    }
  }
}
