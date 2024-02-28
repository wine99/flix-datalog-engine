mod Concurrent2 {
    ///
    /// A wrapper around a Java ReentrantLock.
    ///
    @Internal
    pub enum ReentrantLock[r: Region](Region[r], ##java.util.concurrent.locks.ReentrantLock)

    mod ReentrantLock {

        use Concurrent2.Condition
        use Concurrent2.Condition.Condition

        ///
        /// Creates an instance of ReentrantLock with the given fairness policy.
        /// `fair = False` is the default for Java.
        ///
        @Internal
        pub def newLock(rc: Region[r], fair: Bool): ReentrantLock[r] \ r =
            import java_new java.util.concurrent.locks.ReentrantLock(Bool): ##java.util.concurrent.locks.ReentrantLock \ r as reentrantLock;
            ReentrantLock(rc, reentrantLock(fair))

        ///
        /// Returns true if this lock has fairness set true.
        ///
        @Internal
        pub def isFair(lock: ReentrantLock[r]): Bool \ {} =
            import java.util.concurrent.locks.ReentrantLock.isFair(): Bool \ {};
            let ReentrantLock(_rc, l) = lock;
            isFair(l)

        ///
        /// Acquires the lock.
        ///
        @Internal
        pub def lock(lock: ReentrantLock[r]): Unit \ r =
            import java.util.concurrent.locks.ReentrantLock.lock(): Unit \ r as lockIt;
            let ReentrantLock(_rc, l) = lock;
            lockIt(l)

        ///
        /// Acquires the lock unless the current thread is interrupted.
        ///
        @Internal
        pub def lockInterruptibly(lock: ReentrantLock[r]): Result[##java.lang.InterruptedException, Unit] \ r =
            import java.util.concurrent.locks.ReentrantLock.lockInterruptibly(): Unit \ r;
            let ReentrantLock(_rc, l) = lock;
            try {
                Ok(lockInterruptibly(l))
            } catch {
                case e: ##java.lang.InterruptedException => Err(e)
            }

        ///
        /// Returns a Condition instance for use with this Lock instance.
        ///
        @Internal
        pub def newCondition(lock: ReentrantLock[r]): Condition[r] \ r =
            import java.util.concurrent.locks.ReentrantLock.newCondition(): ##java.util.concurrent.locks.Condition \ r;
            let ReentrantLock(rc, l) = lock;
            Condition(rc, newCondition(l))

        ///
        /// Acquires the lock only if it is not held by another thread at the time of invocation.
        ///
        @Internal
        pub def tryLock(lock: ReentrantLock[r]): Bool \ r =
            import java.util.concurrent.locks.ReentrantLock.tryLock(): Bool \ r;
            let ReentrantLock(_rc, l) = lock;
            tryLock(l)

        ///
        /// Acquires the lock if it is not held by another thread within the given waiting time (nanos) and the current thread has not been interrupted.
        ///
        @Internal
        pub def tryLockNanos(lock: ReentrantLock[r], nanosTimeout: Int64): Result[##java.lang.InterruptedException, Bool] \ r =
            import java.util.concurrent.locks.ReentrantLock.tryLock(Int64, ##java.util.concurrent.TimeUnit): Bool \ r;
            import static java_get_field java.util.concurrent.TimeUnit.NANOSECONDS: ##java.util.concurrent.TimeUnit \ r as nano;
            let ReentrantLock(_rc, l) = lock;
            try {
                Ok(tryLock(l, nanosTimeout, nano()))
            } catch {
                case e: ##java.lang.InterruptedException => Err(e)
            }

        ///
        /// Attempts to release this lock.
        ///
        @Internal
        pub def unlock(lock: ReentrantLock[r]): Result[##java.lang.IllegalMonitorStateException, Unit] \ r =
            import java.util.concurrent.locks.ReentrantLock.unlock(): Unit \ r;
            let ReentrantLock(_rc, l) = lock;
            try {
                Ok(unlock(l))
            } catch {
                case e: ##java.lang.IllegalMonitorStateException => Err(e)
            }

    }
}
