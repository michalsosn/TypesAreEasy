module Control.Monad.Trans.Step.Class (
      MonadStep(..)
    , stepFor
    , idle, idleFor
    , runFor
    , alternate
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Either
import Data.Monoid


class (Monad m) => MonadStep m where
    now  :: a -> m a
    now = run . step
    step :: a -> m a
    run  :: m a -> m a
    finish :: m a -> m a
    finish m = do
        ok <- peekEnd m
        if ok then m else finish (run m)
    peekEnd :: m a -> m Bool

stepFor :: (MonadStep m) => Int -> a -> m a
stepFor 0 x = now x
stepFor n x = step x >>= stepFor (n - 1)

idle :: (MonadStep m) => m ()
idle = step ()

idleFor :: (MonadStep m) => Int -> m ()
idleFor = flip stepFor ()

runFor :: (MonadStep m) => Int -> m a -> m a
runFor 0 = id
runFor n = runFor (n - 1) . run

alternate :: (MonadStep m) => [m a] -> [Int] -> [m a]
alternate [] _ = []
alternate as [] = as
alternate (a:as) (n:ns) = alternate (as ++ [runFor n a]) ns


instance MonadStep m => MonadStep (ReaderT s m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd

instance MonadStep m => MonadStep (Lazy.StateT s m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd

instance MonadStep m => MonadStep (Strict.StateT s m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd

instance (Monoid w, MonadStep m) => MonadStep (Lazy.WriterT w m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd

instance (Monoid w, MonadStep m) => MonadStep (Strict.WriterT w m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd
