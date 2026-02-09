package sjsonnet.client;

import org.scalasbt.ipcsocket.UnixDomainSocket;
import org.scalasbt.ipcsocket.Win32NamedPipeSocket;

import java.io.*;
import java.net.Socket;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SjsonnetClientMain {
    static void initServer(String lockBase, boolean setJnaNoSys) throws IOException {
        List<String> l = new ArrayList<>();
        List<String> vmOptions = new ArrayList<>();
        l.add("java");
        l.add("-DSJSONNET_VERSION=" + System.getProperty("SJSONNET_VERSION"));
        l.add("-cp");
        l.add(System.getProperty("SJSONNET_EXECUTABLE"));
        if (setJnaNoSys) {
            vmOptions.add("-Djna.nosys=true");
        }
        if(!Util.isWindows){
            l.addAll(vmOptions);
        } else {
            final File vmOptionsFile = new File(lockBase, "vmoptions");
            try (PrintWriter out = new PrintWriter(vmOptionsFile)) {
                for(String opt: vmOptions) {
                    out.println(opt);
                }
            }
            l.add("-XX:VMOptionsFile=" + vmOptionsFile.getCanonicalPath());
        }

        l.add("sjsonnet.SjsonnetServerMain");
        l.add(lockBase);

        new ProcessBuilder()
                .command(l)
                .redirectOutput(new java.io.File(lockBase + "/logs"))
                .redirectError(new java.io.File(lockBase + "/logs"))
                .start();
    }
    public static void main(String[] args) throws Exception{
        System.exit(main0(args));
    }
    public static int main0(String[] args) throws Exception{
        boolean setJnaNoSys = System.getProperty("jna.nosys") == null;
        Map<String, String> env = System.getenv();
        if (setJnaNoSys) {
            System.setProperty("jna.nosys", "true");
        }
        int index = 0;
        while (index < 5) {
            index += 1;
            String lockBase = System.getProperty("user.home") + "/.sjsonnet/out/mill-worker-" + index;
            new java.io.File(lockBase).mkdirs();

            try(RandomAccessFile lockFile = new RandomAccessFile(lockBase + "/clientLock", "rw");
                FileChannel channel = lockFile.getChannel();
                java.nio.channels.FileLock tryLock = channel.tryLock();
                Locks locks = Locks.files(lockBase)){
                if (tryLock != null) {
                    return SjsonnetClientMain.run(
                            lockBase,
                        () -> {
                            try {
                                initServer(lockBase, setJnaNoSys);
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
                          Map<String, String> env) throws Exception{

        try(FileOutputStream f = new FileOutputStream(lockBase + "/run")){
            f.write(System.console() != null ? 1 : 0);
            Util.writeString(f, System.getProperty("SJSONNET_VERSION"));
            Util.writeString(f, java.nio.file.Paths.get("").toAbsolutePath().toString());
            Util.writeArgs(args, f);
            Util.writeMap(env, f);
        }

        boolean serverInit = false;
        if (locks.processLock.probe()) {
            serverInit = true;
            initServer.run();
        }
        while(locks.processLock.probe()) Thread.sleep(3);

        // Need to give sometime for Win32NamedPipeSocket to work
        // if the server is just initialized
        if (serverInit && Util.isWindows) Thread.sleep(1000);

        Socket ioSocket = null;

        long retryStart = System.currentTimeMillis();

        while(ioSocket == null && System.currentTimeMillis() - retryStart < 1000){
            try{
                ioSocket = Util.isWindows?
                        new Win32NamedPipeSocket(Util.WIN32_PIPE_PREFIX + new File(lockBase).getName())
                        : new UnixDomainSocket(lockBase + "/io");
            }catch(Throwable e){
                Thread.sleep(1);
            }
        }
        if (ioSocket == null){
            throw new Exception("Failed to connect to server");
        }

        InputStream outErr = ioSocket.getInputStream();
        OutputStream in = ioSocket.getOutputStream();
        ProxyStreamPumper outPump = new ProxyStreamPumper(outErr, stdout, stderr);
        InputPumper inPump = new InputPumper(stdin, in, true);
        Thread outThread = new Thread(outPump);
        outThread.setDaemon(true);
        Thread inThread = new Thread(inPump);
        inThread.setDaemon(true);
        outThread.start();
        inThread.start();

        locks.serverLock.await();

        try(FileInputStream fos = new FileInputStream(lockBase + "/exitCode")){
            return Integer.parseInt(new BufferedReader(new InputStreamReader(fos)).readLine());
        } catch(Throwable e){
            return 1;
        } finally{
            ioSocket.close();
        }
    }
}
