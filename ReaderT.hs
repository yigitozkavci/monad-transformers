import Control.Applicative

newtype Reader a b = Reader
  { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f (Reader x) = Reader $ \r -> f $ x r

instance Applicative (Reader a) where
  pure = Reader . const
  (Reader f) <*> (Reader x) = Reader $ \r -> (f r) (x r)

instance Monad (Reader a) where
  return x = Reader (const x)
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

newtype ReaderT a m b = ReaderT
  { runReaderT :: a -> m b }

instance Monad m => Functor (ReaderT a m) where
  fmap f x = ReaderT $ \a -> do
    b <- runReaderT x $ a
    return $ f b

instance Monad m => Applicative (ReaderT a m) where
  pure x  = ReaderT $ const $ return x
  f <*> x = ReaderT $ liftA2 (<*>) (runReaderT f) (runReaderT x)

instance Monad m => Monad (ReaderT a m) where
  ma >>= f = ReaderT $ \a -> do
    b <- runReaderT ma $ a
    (runReaderT $ f b) $ a
