module Control.Parallel.Class
  ( class MonadPar
  , parTraverse_
  , parTraverse
  , Parallel
  , parallel
  , sequential
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Except (mapExceptT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (mapReaderT, ReaderT)
import Control.Monad.Writer.Trans (mapWriterT, WriterT)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Traversable (class Traversable, traverse)

-- | The `MonadPar` class abstracts over monads which support
-- | parallel composition via some related `Applicative`.
class (Monad m, Applicative f) <= MonadPar f m | m -> f, f -> m where
  parallel   :: m ~> f
  sequential :: f ~> m

-- | Traverse a collection in parallel, discarding any results.
parTraverse_
  :: forall f m t a b
   . (MonadPar f m, Foldable t)
  => (a -> m b)
  -> t a
  -> m Unit
parTraverse_ f = sequential <<< traverse_ (parallel <<< f)

-- | Traverse a collection in parallel.
parTraverse
  :: forall f m t a b
   . (MonadPar f m, Traversable t)
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f = sequential <<< traverse (parallel <<< f)

-- | The `Parallel` type constructor provides an `Applicative` instance
-- | based on `ContT Unit m`, which waits for multiple continuations to be
-- | resumed simultaneously.
-- |
-- | Parallel sections of code can be embedded in sequential code by using
-- | the `parallel` and `sequential` functions:
-- |
-- | ```purescript
-- | loadModel :: ContT Unit (Eff (ajax :: AJAX)) Model
-- | loadModel = do
-- |   token <- authenticate
-- |   sequential $
-- |     Model <$> parallel (get "/products/popular/" token)
-- |           <*> parallel (get "/categories/all" token)
-- | ```
newtype Parallel m a = Parallel (ContT Unit m a)

instance functorParallel :: MonadEff eff m => Functor (Parallel m) where
  map f = parallel <<< map f <<< sequential

instance applyParallel :: MonadEff eff m => Apply (Parallel m) where
  apply (Parallel ca) (Parallel cb) = Parallel $ ContT \k -> do
    ra <- liftEff $ unsafeWithRef (newRef Nothing)
    rb <- liftEff $ unsafeWithRef (newRef Nothing)

    runContT ca \a -> do
      mb <- liftEff $ unsafeWithRef (readRef rb)
      case mb of
        Nothing -> liftEff $ unsafeWithRef (writeRef ra (Just a))
        Just b -> k (a b)

    runContT cb \b -> do
      ma <- liftEff $ unsafeWithRef (readRef ra)
      case ma of
        Nothing -> liftEff $ unsafeWithRef (writeRef rb (Just b))
        Just a -> k (a b)

instance applicativeParallel :: MonadEff eff m => Applicative (Parallel m) where
  pure = parallel <<< pure

instance altParallel :: MonadEff eff m => Alt (Parallel m) where
  alt (Parallel c1) (Parallel c2) = Parallel $ ContT \k -> do
    done <- liftEff $ unsafeWithRef (newRef false)

    runContT c1 \a -> do
      b <- liftEff $ unsafeWithRef (readRef done)
      if b
        then pure unit
        else do
          liftEff $ unsafeWithRef (writeRef done true)
          k a

    runContT c2 \a -> do
      b <- liftEff $ unsafeWithRef (readRef done)
      if b
        then pure unit
        else do
          liftEff $ unsafeWithRef (writeRef done true)
          k a

instance plusParallel :: MonadEff eff m => Plus (Parallel m) where
  empty = Parallel $ ContT \_ -> pure unit

instance alternativeParallel :: MonadEff eff m => Alternative (Parallel m)

instance monadParParallel :: MonadEff eff m => MonadPar (Parallel m) (ContT Unit m) where
  parallel = Parallel
  sequential (Parallel ma) = ma

instance monadParExceptT :: MonadPar f m => MonadPar (ExceptT e f) (ExceptT e m) where
  parallel = mapExceptT parallel
  sequential = mapExceptT sequential

instance monadParReaderT :: MonadPar f m => MonadPar (ReaderT e f) (ReaderT e m) where
  parallel = mapReaderT parallel
  sequential = mapReaderT sequential

instance monadParWriterT :: (Monoid w, MonadPar f m) => MonadPar (WriterT w f) (WriterT w m) where
  parallel = mapWriterT parallel
  sequential = mapWriterT sequential

-- This instance doesn't work yet, since the Applicative instance for MaybeT is
-- too restrictive.

-- instance monadParMaybeT :: MonadPar f m => MonadPar (MaybeT f) (MaybeT m) where
--   parallel = mapMaybeT ?p
--   sequential = mapMaybeT ?s

unsafeWithRef :: forall eff a. Eff (ref :: REF | eff) a -> Eff eff a
unsafeWithRef = unsafeInterleaveEff
