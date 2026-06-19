package sjsonnet.client;

import java.io.*;
import java.net.StandardProtocolFamily;
import java.net.UnixDomainSocketAddress;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SjsonnetClientMain {
    static void initServer(String lockBase) throws IOException {
        List<String> l = new ArrayList<>();
        l.add("java");
        l.add("-DSJSONNET_VERSION=" + System.getProperty("SJSONNET_VERSION"));
        l.add("-cp");
        l.add(System.getProperty("SJSONNET_EXECUTABLE"));
        l.add("sjsonnet.SjsonnetServerRealMain");
        l.add(lockBase);

        new ProcessBuilder()
                .command(l)
                .redirectOutput(Path.of(lockBase, "logs").toFile())
                .redirectError(Path.of(lockBase, "logs").toFile())
                .start();
    }

    public static void main(String[] args) throws Exception {
        System.exit(main0(args));
    }

    public static int main0(String[] args) throws Exception {
        Map<String, String> env = System.getenv();
        int index = 0;
        while (index < 5) {
            index += 1;
            String lockBase = System.getProperty("user.home") + "/.sjsonnet/out/mill-worker-" + index;
            Files.createDirectories(Path.of(lockBase));

            try (RandomAccessFile lockFile = new RandomAccessFile(lockBase + "/clientLock", "rw");
                 FileChannel channel = lockFile.getChannel();
                 java.nio.channels.FileLock tryLock = channel.tryLock();
                 Locks locks = Locks.files(lockBase)) {
                if (tryLock != null) {
                    return SjsonnetClientMain.run(
                            lockBase,
                            () -> {
                                try {
                                    initServer(lockBase);
                                } catch (Exception e) {
                                    throw new RuntimeException(e);
                                }
                            },
                            locks,
                            System.in,
                            System.out,
                            System.err,
                            args,
                            env
                    );
                }
            }
        }
        throw new Exception("Reached max sjsonnet process limit: " + 5);
    }

    public static int run(String lockBase,
                          Runnable initServer,
                          Locks locks,
                          InputStream stdin,
                          OutputStream stdout,
                          OutputStream stderr,
                          String[] args,
                          Map<String, String> env) throws Exception {

        Path runFile = Path.of(lockBase, "run");
        try (FileOutputStream f = new FileOutputStream(runFile.toFile())) {
            f.write(System.console() != null ? 1 : 0);
            Util.writeString(f, System.getProperty("SJSONNET_VERSION"));
            Util.writeString(f, Path.of("").toAbsolutePath().toString());
            Util.writeArgs(args, f);
            Util.writeMap(env, f);
        }

        boolean serverInit = false;
        if (locks.processLock.probe()) {
            serverInit = true;
            initServer.run();
        }
        while (locks.processLock.probe()) Thread.sleep(3);

        // Need to give some time for the UDS server socket to be ready
        if (serverInit && Util.isWindows) Thread.sleep(1000);

        UnixDomainSocketAddress addr = UnixDomainSocketAddress.of(lockBase + "/io");
        SocketChannel ioSocket = null;

        long retryStart = System.currentTimeMillis();

        while (ioSocket == null && System.currentTimeMillis() - retryStart < 1000) {
            try {
                ioSocket = SocketChannel.open(StandardProtocolFamily.UNIX);
                ioSocket.connect(addr);
            } catch (Throwable e) {
                if (ioSocket != null) {
                    try { ioSocket.close(); } catch (IOException ignored) {}
                    ioSocket = null;
                }
                Thread.sleep(1);
            }
        }
        if (ioSocket == null) {
            throw new Exception("Failed to connect to server");
        }

        InputStream outErr = Channels.newInputStream(ioSocket);
        OutputStream in = Channels.newOutputStream(ioSocket);
        ProxyStreamPumper outPump = new ProxyStreamPumper(outErr, stdout, stderr);
        InputPumper inPump = new InputPumper(stdin, in, true);
        Thread outThread = new Thread(outPump);
        outThread.setDaemon(true);
        Thread inThread = new Thread(inPump);
        inThread.setDaemon(true);
        outThread.start();
        inThread.start();

        locks.serverLock.await();

        try {
            String content = Files.readString(Path.of(lockBase, "exitCode"), StandardCharsets.UTF_8);
            return Integer.parseInt(content.trim());
        } catch (Throwable e) {
            return 1;
        } finally {
            ioSocket.close();
        }
    }
}
