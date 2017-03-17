import Control.Applicative

newtype State s a = State
  { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f mx = State $ \s ->
    let (a, s') = runState mx $ s in
    (f a, s')

instance Applicative (State s) where
  pure x = State $ (,) x
  mf <*> mx = State $ \s ->
    let
      (a, s') = runState mx $ s
      (func, s'') = runState mf $ s'
    in
      (func a, s'')

instance Monad (State s) where
  mx >>= f = State $ \s ->
    let (a, s') = runState mx $ s in
    (runState $ f a) $ s'

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap f mx = StateT $ \s -> do
    (a, s') <- runStateT mx $ s
    return $ (f a, s')

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> return $ (x, s)
  mf <*> mx = StateT $ \s -> do
    (a, s') <- runStateT mx $ s
    (f', s'') <- runStateT mf $ s'
    return (f' a, s'')

instance Monad m => Monad (StateT s m) where
  mx >>= f = StateT $ \s -> do
    (a, s') <- runStateT mx $ s
    (runStateT $ f a) $ s'
