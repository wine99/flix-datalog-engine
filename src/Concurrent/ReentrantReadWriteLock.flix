mod Concurrent2 {
    @Internal
    pub enum ReentrantReadWriteLock[r: Region](Region[r], ##java.util.concurrent.locks.ReentrantReadWriteLock)

    mod ReentrantReadWriteLock {

        @Internal
        pub def newLock(rc: Region[r], fair: Bool): ReentrantReadWriteLock[r] \ r =
            import java_new java.util.concurrent.locks.ReentrantReadWriteLock(Bool): ##java.util.concurrent.locks.ReentrantReadWriteLock \ r as reentrantReadWriteLock;
            ReentrantReadWriteLock(rc, reentrantReadWriteLock(fair))

        @Internal
        pub def isFair(lock: ReentrantReadWriteLock[r]): Bool =
            import java.util.concurrent.locks.ReentrantReadWriteLock.isFair(): Bool \ {};
            let ReentrantReadWriteLock(_, l) = lock;
            isFair(l)

        @Internal
        pub def lockWrite(lock: ReentrantReadWriteLock[r]): Unit \ r =
            import java.util.concurrent.locks.ReentrantReadWriteLock.writeLock(): ##java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock \ {} as writeLock;
            import java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock.lock(): Unit \ r as lockIt;
            let ReentrantReadWriteLock(_, l) = lock;
            let writeL = writeLock(l);
            lockIt(writeL)

        @Internal
        pub def unlockWrite(lock: ReentrantReadWriteLock[r]): Unit \ r =
            import java.util.concurrent.locks.ReentrantReadWriteLock.writeLock(): ##java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock \ {} as writeLock;
            import java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock.unlock(): Unit \ r as unlockIt;
            let ReentrantReadWriteLock(_, l) = lock;
            let writeL = writeLock(l);
            unlockIt(writeL)

        @Internal
        pub def lockRead(lock: ReentrantReadWriteLock[r]): Unit \ r =
            import java.util.concurrent.locks.ReentrantReadWriteLock.readLock(): ##java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock \ {} as readLock;
            import java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock.lock(): Unit \ r as lockIt;
            let ReentrantReadWriteLock(_, l) = lock;
            let readL = readLock(l);
            lockIt(readL)

        @Internal
        pub def unlockRead(lock: ReentrantReadWriteLock[r]): Unit \ r =
            import java.util.concurrent.locks.ReentrantReadWriteLock.readLock(): ##java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock \ {} as readLock;
            import java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock.unlock(): Unit \ r as unlockIt;
            let ReentrantReadWriteLock(_, l) = lock;
            let readL = readLock(l);
            unlockIt(readL)
    }
}
