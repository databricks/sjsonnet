package sjsonnet.client;

import java.util.concurrent.locks.ReentrantLock;

final class MemoryLocked implements Locked {
    private final java.util.concurrent.locks.Lock l;

    MemoryLocked(java.util.concurrent.locks.Lock l) {
        this.l = l;
    }

    public void release() throws Exception {
        l.unlock();
    }
}

final class MemoryLock extends Lock {
    public static Locks memory() {
        return new Locks(
                new MemoryLock(),
                new MemoryLock(),
                new MemoryLock()
        );
    }

    private final ReentrantLock innerLock = new ReentrantLock(true);

    public boolean probe() {
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
