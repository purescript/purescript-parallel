module Control.Monad.Parallel.Class 
  ( MonadPar, par
  , MonadRace, race
  , Parallel(), inParallel, runParallel
  ) where

import Prelude
import Control.Alt

class (Monad m) <= MonadPar m where
  par :: forall a b. m (a -> b) -> m a -> m b

class (MonadPar m) <= MonadRace m where
  race :: forall a. m a -> m a -> m a

newtype Parallel m a = Parallel (m a)

inParallel :: forall m a. m a -> Parallel m a
inParallel = Parallel

runParallel :: forall m a. Parallel m a -> m a
runParallel (Parallel ma) = ma

instance functorParallel :: (Functor m) => Functor (Parallel m) where
  map f = inParallel <<< map f <<< runParallel

instance applyParallel :: (MonadPar m) => Apply (Parallel m) where
  apply f a = inParallel (runParallel f `par` runParallel a)

instance applicativeParallel :: (MonadPar m) => Applicative (Parallel m) where
  pure = inParallel <<< return

instance altParallel :: (MonadRace m) => Alt (Parallel m) where
  alt a b = inParallel (runParallel a `race` runParallel b)
