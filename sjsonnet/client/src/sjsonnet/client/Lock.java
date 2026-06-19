package sjsonnet.client;
public abstract class Lock implements AutoCloseable {
    abstract public Locked lock() throws Exception;
    abstract public Locked tryLock() throws Exception;

    public void await() throws Exception {
        try (Locked l = lock()) {
            // wait for release
        }
    }

    /**
     * Returns `true` if the lock is *available for taking*
     */
    abstract public boolean probe() throws Exception;
}