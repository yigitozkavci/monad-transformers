import Control.Applicative

newtype Reader a b = Reader
  { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f x = Reader $ \r -> f $ runReader x r

instance Applicative (Reader a) where
  pure = Reader . const
  f <*> x = Reader $ \r -> runReader f r (runReader x r)

instance Monad (Reader a) where
  return x = Reader (const x)
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r
