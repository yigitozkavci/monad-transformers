import Control.Applicative

newtype State s a = State
  { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f x = State $ \s ->
    let (a, s) = runState x $ s in
    (f a, s)

instance Applicative (State s) where
  pure x = State $ (,) x
  f <*> mx = State $ \s ->
    let
      (a, s') = runState mx $ s
      (func, s'') = runState f $ s'
    in
      (func a, s'')

instance Monad (State s) where
  mx >>= f = State $ \s ->
    let (a, s') = runState mx $ s in
    (runState $ f a) $ s'
