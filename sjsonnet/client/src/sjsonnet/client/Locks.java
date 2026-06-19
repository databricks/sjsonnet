package sjsonnet.client;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;

public class Locks implements AutoCloseable {
    public Lock processLock;
    public Lock serverLock;
    public Lock clientLock;

    public Locks(Lock processLock, Lock serverLock, Lock clientLock) {
        this.processLock = processLock;
        this.serverLock = serverLock;
        this.clientLock = clientLock;
    }

    public static Locks files(String lockBase) throws Exception {
        FileLock process = new FileLock(lockBase + "/pid");
        FileLock server = null;
        FileLock client = null;
        try {
            server = new FileLock(lockBase + "/serverLock");
            client = new FileLock(lockBase + "/clientLock");
            return new Locks(process, server, client);
        } catch (Exception e) {
            if (client != null) try { client.close(); } catch (Exception ignored) {}
            if (server != null) try { server.close(); } catch (Exception ignored) {}
            try { process.close(); } catch (Exception ignored) {}
            throw e;
        }
    }

    @Override
    public void close() throws Exception {
        Throwable first = null;
        try {
            processLock.close();
        } catch (Throwable t) {
            first = t;
        }
        try {
            serverLock.close();
        } catch (Throwable t) {
            if (first == null) first = t;
            else first.addSuppressed(t);
        }
        try {
            clientLock.close();
        } catch (Throwable t) {
            if (first == null) first = t;
            else first.addSuppressed(t);
        }
        if (first != null) {
            if (first instanceof Exception) throw (Exception) first;
            throw new RuntimeException(first);
        }
    }
}

final class FileLocked implements Locked {
    private final java.nio.channels.FileLock lock;

    FileLocked(java.nio.channels.FileLock lock) {
        this.lock = lock;
    }

    public void release() throws Exception {
        this.lock.release();
    }
}

final class FileLock extends Lock {
    private final RandomAccessFile raf;
    private final FileChannel chan;

    FileLock(String path) throws Exception {
        this.raf = new RandomAccessFile(path, "rw");
        this.chan = raf.getChannel();
    }

    public Locked lock() throws Exception {
        return new FileLocked(chan.lock());
    }

    public Locked tryLock() throws Exception {
        java.nio.channels.FileLock l = chan.tryLock();
        if (l == null) return null;
        return new FileLocked(l);
    }

    public boolean probe() throws Exception {
        java.nio.channels.FileLock l = chan.tryLock();
        if (l == null) return false;
        l.release();
        return true;
    }

    @Override
    public void close() throws IOException {
        try {
            raf.close();
        } finally {
            chan.close();
        }
    }
}
