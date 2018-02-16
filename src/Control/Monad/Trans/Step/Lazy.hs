{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.Monad.Trans.Step.Lazy (
      StepT(..), Step
    , current, next, finished
    , runStep
    , alternateFinish
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Trans.Step.Class


newtype StepT m a = StepT { runStepT :: m (Either (StepT m a) a) }

instance (Functor m) => Functor (StepT m) where
    f `fmap` x = StepT $ either (Left . fmap f) (Right . f) `fmap` runStepT x

instance (Applicative m) => Applicative (StepT m) where
    pure = StepT . pure . Right
    x <*> y = StepT $ liftA2 k (runStepT x) (runStepT y)
        where
            k :: (Applicative m) => (Either (StepT m (a -> b)) (a -> b)) -> (Either (StepT m a) a) -> (Either (StepT m b) b)
            k (Left sf) (Left sv) = Left (sf <*> sv)
            k (Left sf) (Right v) = Left (sf <*> pure v)
            k (Right f) (Left sv) = Left (pure f <*> sv)
            k (Right f) (Right v) = Right (f v)

instance (Monad m) => Monad (StepT m) where
    return = StepT . return . Right
    m >>= f = StepT $ runStepT m >>= either (return . Left . (>>= f)) (runStepT . f)

instance MonadTrans StepT where
    lift m = StepT (Right `liftM` m)

instance (Monad m) => MonadStep (StepT m) where
    now = return
    step = StepT . return . Left . now
    run m = StepT $ runStepT m >>= either runStepT (return . return)
    finish m = StepT $ runStepT m >>= either (runStepT . finish) (return . return)
    peekEnd m = StepT $ either (bail False) (bail True) `liftM` runStepT m
        where
            bail = const . return

instance (MonadIO m) => MonadIO (StepT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (StepT m) where
    get = lift get
    put k = lift (put k)

instance (MonadWriter w m) => MonadWriter w (StepT m) where
    writer = lift . writer
    tell   = lift . tell
    -- listen :: m a -> m (a, w)
    listen = liftListen listen
    -- pass :: m (a, w -> w) -> m a
    pass = liftPass pass

mapStepT :: (m (Either (StepT m a) a) -> n (Either (StepT n b) b)) -> StepT m a -> StepT n b
mapStepT f m = StepT $ f (runStepT m)

liftListen :: Monad m => (m (Either (StepT m a) a) -> m (Either (StepT m a) a, w)) -> StepT m a -> StepT m (a, w)
liftListen listen = mapStepT $ \m -> do
    (a, w) <- listen m
    let add r = (r, w)
    return $ either (Left . liftM add) (Right . add) a

liftPass :: Monad m => (m (Either (StepT m a) a, w -> w) -> m (Either (StepT m a) a)) -> StepT m (a, w -> w) -> StepT m a
liftPass pass = mapStepT $ \m -> pass $ do
    a <- m
    return $ case a of
        Left l -> (Left (liftPass pass l), id)
        Right (r, f) -> (Right r, f)


current :: (Monad m) => StepT m a -> m (StepT m a)
current = liftM (either (StepT . return . Left) now) . runStepT

next :: (Monad m) => StepT m a -> m (StepT m a)
next = liftM (either id now) . runStepT

finished :: (Monad m) => StepT m a -> m Bool
finished = liftM (either (const False) (const True)) . runStepT


type Step = StepT Identity

runStep :: Step a -> a
runStep m = let Right x = runIdentity . runStepT . finish $ m in x

alternateFinish :: [Step a] -> [Int] -> [a]
alternateFinish as ns = aux as ns []
    where
        aux [] _  acc = acc
        aux as [] acc = fmap runStep as ++ acc
        aux (a:as) (n:ns) acc
            | runStep . peekEnd $ a = aux as (n:ns) (runStep a:acc)
            | otherwise             = aux (as ++ [runFor n a]) ns acc

