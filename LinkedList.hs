import Data.Monoid

data LL a = Nil | Cons a (LL a) deriving Show

fromList :: [a] -> LL a
fromList = foldr Cons Nil

instance Monoid (LL a) where
  mempty = Nil
  mappend Nil l2 = l2
  mappend (Cons x l) l2 = Cons x (l <> l2)

instance Functor LL where
  fmap f Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance Applicative LL where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f lf) <*> l = (f <$> l) <> (lf <*> l)
