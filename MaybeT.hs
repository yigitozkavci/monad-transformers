import Control.Applicative

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  f <*> x = MaybeT $ liftA2 (<*>) (runMaybeT f) (runMaybeT x)

instance Monad m => Monad (MaybeT m) where
  m >>= f = MaybeT $ runMaybeT m >>= \i ->
    case i of
      Just a -> runMaybeT $ f a
      Nothing -> return Nothing

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe mayb =
  MaybeT $ case mayb of
    Just a -> return (Just a)
    Nothing -> return Nothing

liftIO :: Monad m => m a -> MaybeT m a
liftIO mio =
  MaybeT $ mio >>= \x -> return $ Just x
