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

newtype StateIO s a = StateIO
  { runStateIO :: s -> IO (a, s) }

instance Functor (StateIO s) where
  fmap f mx = StateIO $ \s -> do
    (a, s') <- runStateIO mx $ s
    return $ (f a, s')

instance Applicative (StateIO s) where
  pure x = StateIO $ \s -> return $ (x, s)
  mf <*> mx = StateIO $ \s -> do
    (a, s') <- runStateIO mx $ s
    (f', s'') <- runStateIO mf $ s'
    return (f' a, s'')

instance Monad (StateIO s) where
  mx >>= f = StateIO $ \s -> do
    (a, s') <- runStateIO mx $ s
    (runStateIO $ f a) $ s'
