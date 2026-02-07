package sjsonnet.client;

import java.util.concurrent.locks.ReentrantLock;

class MemoryLocked implements Locked{
    final java.util.concurrent.locks.Lock l;
    public MemoryLocked(java.util.concurrent.locks.Lock l){
        this.l = l;
    }
    public void release() throws Exception{
        l.unlock();
    }
}

class MemoryLock extends Lock{
    public static Locks memory(){
        return new Locks(){{
            this.processLock = new MemoryLock();
            this.serverLock = new MemoryLock();
            this.clientLock = new MemoryLock();
        }};
    }

    final ReentrantLock innerLock = new ReentrantLock(true);

    public boolean probe(){
          return !innerLock.isLocked();
    }
    public Locked lock() {
        innerLock.lock();
        return new MemoryLocked(innerLock);
    }
    public Locked tryLock() {
        if (innerLock.tryLock()) return new MemoryLocked(innerLock);
        else return null;
    }

    @Override
    public void close() throws Exception {
        innerLock.unlock();
    }
}
