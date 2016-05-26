module Control.Parallel.Class
  ( class MonadPar
  , par
  , parTraverse_
  , parTraverse
  , class MonadRace
  , stall
  , race
  , Parallel
  , parallel
  , runParallel
  ) where

import Prelude ( class Applicative
               , class Apply
               , class Functor
               , class Monad
               , (<<<)
               , ($)
               , bind
               , map
               , pure
               , unit
               , Unit
               )

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, traverse_)
import Data.Traversable (class Traversable, traverse)

-- | The `MonadPar` class abstracts over monads which support some notion of
-- | parallel composition.
-- |
-- | The `unit` and `par` functions should correspond to `pure` and `lift2` for
-- | some valid `Applicative` instance, but that instance need not arise from
-- | the underlying `Monad`.
class Monad m <= MonadPar m where
  par :: forall a b c. (a -> b -> c) -> m a -> m b -> m c

-- | Traverse a collection in parallel, discarding any results.
parTraverse_
  :: forall m t a b
   . (MonadPar m, Foldable t)
  => (a -> m b)
  -> t a
  -> m Unit
parTraverse_ f = runParallel <<< traverse_ (Parallel <<< f)

-- | Traverse a collection in parallel.
parTraverse
  :: forall m t a b
   . (MonadPar m, Traversable t)
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f = runParallel <<< traverse (Parallel <<< f)

-- | The `MonadRace` class extends `MonadPar` to those monads which can be
-- | _raced_. That is, monads for which two computations can be
-- |
-- | The `stall` and `race` functions should satisfy the `Alternative` laws.
class MonadPar m <= MonadRace m where
  stall :: forall a. m a
  race :: forall a. m a -> m a -> m a

unsafeWithRef :: forall eff a. Eff (ref :: REF | eff) a -> Eff eff a
unsafeWithRef = unsafeInterleaveEff

instance monadParContT :: MonadPar (ContT Unit (Eff eff)) where
  par f ca cb = ContT \k -> do
    ra <- unsafeWithRef (newRef Nothing)
    rb <- unsafeWithRef (newRef Nothing)

    runContT ca \a -> do
      mb <- unsafeWithRef (readRef rb)
      case mb of
        Nothing -> unsafeWithRef (writeRef ra (Just a))
        Just b -> k (f a b)

    runContT cb \b -> do
      ma <- unsafeWithRef (readRef ra)
      case ma of
        Nothing -> unsafeWithRef (writeRef rb (Just b))
        Just a -> k (f a b)

instance monadParExceptT :: MonadPar m => MonadPar (ExceptT e m) where
  par f (ExceptT e1) (ExceptT e2) = ExceptT (par (lift2 f) e1 e2)

instance monadParReaderT :: MonadPar m => MonadPar (ReaderT e m) where
  par f (ReaderT r1) (ReaderT r2) = ReaderT \r -> par f (r1 r) (r2 r)

instance monadRaceContT :: MonadRace (ContT Unit (Eff eff)) where
  stall = ContT \_ -> pure unit
  race c1 c2 = ContT \k -> do
    done <- unsafeWithRef (newRef false)

    runContT c1 \a -> do
      b <- unsafeWithRef (readRef done)
      if b
        then pure unit
        else do
          unsafeWithRef (writeRef done true)
          k a

    runContT c2 \a -> do
      b <- unsafeWithRef (readRef done)
      if b
        then pure unit
        else do
          unsafeWithRef (writeRef done true)
          k a

instance monadRaceExceptT :: MonadRace m => MonadRace (ExceptT e m) where
  stall = ExceptT stall
  race (ExceptT e1) (ExceptT e2) = ExceptT (race e1 e2)

instance monadRaceReaderT :: MonadRace m => MonadRace (ReaderT e m) where
  stall = ReaderT \_ -> stall
  race (ReaderT r1) (ReaderT r2) = ReaderT \r -> race (r1 r) (r2 r)

-- | The `Parallel` type constructor provides `Applicative` and `Alternative`
-- | instances for any type monad with `MonadPar` and `MonadRace` instances
-- | respectively.
-- |
-- | - The definition of `apply` from `Apply` runs two computations in parallel and applies
-- |   a function when both complete.
-- | - The definition of `alt` from the `Alt` type class runs two computations in parallel
-- |   and returns the result of the computation which completes first.
-- |
-- | Parallel sections of code can be embedded in sequential code by using
-- | the `parallel` and `runParallel` functions:
-- |
-- | ```purescript
-- | loadModel :: ContT Unit (Eff (ajax :: AJAX)) Model
-- | loadModel = do
-- |   token <- authenticate
-- |   runParallel $
-- |     Model <$> parallel (get "/products/popular/" token)
-- |           <*> parallel (get "/categories/all" token)
-- | ```
newtype Parallel m a = Parallel (m a)

-- | Lift a computation to be run in parallel from a computation in the
-- | corresponding `Monad`.
parallel :: forall m a. m a -> Parallel m a
parallel = Parallel

-- | Lower a parallel computation to the underlying `Monad`, so that it may be
-- | used in a larger sequential computation.
runParallel :: forall m a. Parallel m a -> m a
runParallel (Parallel ma) = ma

instance functorParallel :: Functor m => Functor (Parallel m) where
  map f = parallel <<< map f <<< runParallel

instance applyParallel :: MonadPar m => Apply (Parallel m) where
  apply f a = parallel (par ($) (runParallel f) (runParallel a))

instance applicativeParallel :: MonadPar m => Applicative (Parallel m) where
  pure = parallel <<< pure

instance altParallel :: MonadRace m => Alt (Parallel m) where
  alt a b = parallel (runParallel a `race` runParallel b)

instance plusParallel :: MonadRace m => Plus (Parallel m) where
  empty = parallel stall

instance alternativeParallel :: MonadRace m => Alternative (Parallel m)
