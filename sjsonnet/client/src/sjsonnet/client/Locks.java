package sjsonnet.client;

import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.util.concurrent.locks.ReentrantLock;

public class Locks implements AutoCloseable{
    public Lock processLock;
    public Lock serverLock;
    public Lock clientLock;
    public static Locks files(String lockBase) throws Exception{
        return new Locks(){{
            processLock = new FileLock(lockBase + "/pid");

            serverLock = new FileLock(lockBase + "/serverLock");

            clientLock = new FileLock(lockBase + "/clientLock");
        }};
    }

    @Override
    public void close() throws Exception {
        processLock.close();
        serverLock.close();
        clientLock.close();
    }
}
class FileLocked implements Locked{
    private java.nio.channels.FileLock lock;
    public FileLocked(java.nio.channels.FileLock lock){
        this.lock = lock;
    }
    public void release() throws Exception{
        this.lock.release();
    }
}

class FileLock extends Lock{
    String path;
    RandomAccessFile raf;
    FileChannel chan;
    public FileLock(String path) throws Exception{
        this.path = path;
        raf = new RandomAccessFile(path, "rw");
        chan = raf.getChannel();
    }

    public Locked lock() throws Exception{
        return new FileLocked(chan.lock());
    }
    public Locked tryLock() throws Exception{
        java.nio.channels.FileLock l = chan.tryLock();
        if (l == null) return null;
        else return new FileLocked(l);
    }
    public boolean probe()throws Exception{
        java.nio.channels.FileLock l = chan.tryLock();
        if (l == null) return false;
        else {
            l.release();
            return true;
        }
    }

    @Override
    public void close() throws Exception {
        raf.close();
        chan.close();
    }
}

