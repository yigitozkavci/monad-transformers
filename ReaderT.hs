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

newtype ReaderIO a b = ReaderIO
  { runReaderIO :: a -> IO b }

instance Functor (ReaderIO a) where
  fmap f x = ReaderIO $ \a -> do
    b <- runReaderIO x $ a
    return $ f b

instance Applicative (ReaderIO a) where
  pure x  = ReaderIO $ const $ return x
  f <*> x = ReaderIO $ liftA2 (<*>) (runReaderIO f) (runReaderIO x)

instance Monad (ReaderIO a) where
  ma >>= f = ReaderIO $ \a -> do
    b <- runReaderIO ma $ a
    (runReaderIO $ f b) $ a
