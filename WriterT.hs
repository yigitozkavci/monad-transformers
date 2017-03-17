import Control.Applicative
import Data.Monoid ((<>))

newtype Writer w a = Writer
  { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f x = Writer $ (f val, log) where
    (val, log) = runWriter x

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  f <*> x = Writer $
    ( (fst . runWriter $ f) $ (fst . runWriter $ x)
    , (snd . runWriter $ f) <> (snd . runWriter $ x) )

instance Monoid w => Monad (Writer w) where
  mx >>= f = Writer $ (val2, log1 <> log2) where
    (val1, log1) = runWriter mx
    (val2, log2) = runWriter . f $ val1

newtype WriterT w m a = WriterT
  { runWriterT :: m (Writer w a) }

instance Monad m => Functor (WriterT w m) where
  fmap f ma = WriterT $ runWriterT ma >>= \writer ->
    let (val, log) = runWriter writer in
      return . Writer $ (f val, log)

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure x = WriterT $ return $ (Writer (x, mempty))
  f <*> ma = WriterT $ do
    writer <- runWriterT ma
    writer2 <- runWriterT f
    let (val, log) = runWriter writer
    let (func, log2) = runWriter writer2
    return $ Writer (func val, log <> log2)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  ma >>= f = WriterT $ do
    writer <- runWriterT ma
    let (val, log) = runWriter writer
    writer2 <- runWriterT $ f val
    let (val2, log2) = runWriter writer2
    return $ Writer (val2, log <> log2)
