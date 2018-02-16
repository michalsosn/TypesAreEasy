module Control.Monad.Trans.Step.Concurrency (
      Lock, released
    , lock, unlock, withLock
    , tryLock, tryLockTimeout, withLockTimeout
    ) where

import Control.Monad.State

import Control.Monad.Trans.Step

data Lock = Released | Acquired

released :: Lock
released = Released

lock :: (MonadStep m, MonadState s m) => (s -> Lock) -> (Lock -> s -> s) -> m () -> m ()
lock get set wait = gets get >>= aux
    where
        aux Released = modify (set Acquired)
        aux Acquired = wait >> lock get set wait

unlock :: (MonadStep m, MonadState s m) => (Lock -> s -> s) -> m ()
unlock set = modify (set Released)

withLock :: (MonadStep m, MonadState s m) => (s -> Lock) -> (Lock -> s -> s) -> m () -> m () -> m ()
withLock get set wait m = lock get set wait >> m >> unlock set

tryLock :: (MonadStep m, MonadState s m) => (s -> Lock) -> (Lock -> s -> s) -> m Bool
tryLock get set = gets get >>= aux
    where
        aux Released = modify (set Acquired) >> return True
        aux Acquired = return False

tryLockTimeout :: (MonadStep m, MonadState s m) => (s -> Lock) -> (Lock -> s -> s) -> m () -> Int -> m Bool
tryLockTimeout get set _wait 0 = tryLock get set
tryLockTimeout get set wait n = tryLock get set >>= aux
    where
        aux True  = return True
        aux False = wait >> tryLockTimeout get set wait (n - 1)

withLockTimeout :: (MonadStep m, MonadState s m) => (s -> Lock) -> (Lock -> s -> s) -> m () -> Int -> m () -> m Bool
withLockTimeout get set wait n m = tryLockTimeout get set wait n >>= aux
    where
        aux True  = m >> unlock set >> return True
        aux False = return False

--data Queue a = Queue [a] [a]

--data Latch = Open | Closed Int

--data Semaphore = Semaphore Int
