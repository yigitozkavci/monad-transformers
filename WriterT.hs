import Control.Applicative
import Data.Monoid ((<>))

newtype Writer w a = Writer
  { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f x = Writer $ (f val, log) where
    (val, log) = runWriter x

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty) -- What is the empty value of a monoid?
  f <*> x = Writer $
    ( (fst . runWriter $ f) $ (fst . runWriter $ x)
    , (snd . runWriter $ f) <> (snd . runWriter $ x) )

instance Monoid w => Monad (Writer w) where
  mx >>= f = Writer $ (val2, log1 <> log2) where
    (val1, log1) = runWriter mx
    (val2, log2) = runWriter . f $ val1
