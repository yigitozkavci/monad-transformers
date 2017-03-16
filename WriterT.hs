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

newtype WriterIO w a = WriterIO
  { runWriterIO :: IO (Writer w a) }

instance Functor (WriterIO w) where
  fmap f ma = WriterIO $ runWriterIO ma >>= \writer ->
    let (val, log) = runWriter writer in
      return . Writer $ (f val, log)

instance Monoid w => Applicative (WriterIO w) where
  pure x = WriterIO $ return $ (Writer (x, mempty))
  f <*> ma = WriterIO $ do
    writer <- runWriterIO ma
    writer2 <- runWriterIO f
    let (val, log) = runWriter writer
    let (func, log2) = runWriter writer2
    return $ Writer (func val, log <> log2)

instance Monoid w => Monad (WriterIO w) where
  ma >>= f = WriterIO $ do
    writer <- runWriterIO ma
    let (val, log) = runWriter writer
    writer2 <- runWriterIO $ f val
    let (val2, log2) = runWriter writer2
    return $ Writer (val2, log <> log2)
