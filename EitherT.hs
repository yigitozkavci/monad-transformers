import Control.Applicative

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  f <*> x = EitherT $ liftA2 (<*>) (runEitherT f) (runEitherT x)

instance Monad m => Monad (EitherT e m) where
  return = EitherT . return . Right
  mx >>= f = EitherT $ runEitherT mx >>= either (return . Left) (runEitherT . f)

lift :: Functor m => m a -> EitherT e m a
lift mio = EitherT $ fmap Right mio

liftEither :: Monad m => Either e a -> EitherT e m a
liftEither ei = EitherT $ return ei
