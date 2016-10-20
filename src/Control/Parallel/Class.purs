module Control.Parallel.Class
  ( class Parallel
  , parallel
  , sequential
  , ParCont(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Trans (mapReaderT, ReaderT)
import Control.Monad.Writer.Trans (mapWriterT, WriterT)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Control.Plus (class Plus)

import Data.Either (Either)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

-- | The `Parallel` class abstracts over monads which support
-- | parallel composition via some related `Applicative`.
class (Monad m, Applicative f) <= Parallel f m | m -> f, f -> m where
  parallel   :: m ~> f
  sequential :: f ~> m

instance monadParExceptT :: Parallel f m => Parallel (Compose f (Either e)) (ExceptT e m) where
  parallel (ExceptT ma) = Compose (parallel ma)
  sequential (Compose fa) = ExceptT (sequential fa)

instance monadParReaderT :: Parallel f m => Parallel (ReaderT e f) (ReaderT e m) where
  parallel = mapReaderT parallel
  sequential = mapReaderT sequential

instance monadParWriterT :: (Monoid w, Parallel f m) => Parallel (WriterT w f) (WriterT w m) where
  parallel = mapWriterT parallel
  sequential = mapWriterT sequential

instance monadParMaybeT :: Parallel f m => Parallel (Compose f Maybe) (MaybeT m) where
  parallel (MaybeT ma) = Compose (parallel ma)
  sequential (Compose fa) = MaybeT (sequential fa)

-- | The `ParCont` type constructor provides an `Applicative` instance
-- | based on `ContT Unit m`, which waits for multiple continuations to be
-- | resumed simultaneously.
-- |
-- | ParCont sections of code can be embedded in sequential code by using
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
newtype ParCont m a = ParCont (ContT Unit m a)

derive instance newtypeParCont :: Newtype (ParCont m a) _

instance functorParCont :: MonadEff eff m => Functor (ParCont m) where
  map f = parallel <<< map f <<< sequential

instance applyParCont :: MonadEff eff m => Apply (ParCont m) where
  apply (ParCont ca) (ParCont cb) = ParCont $ ContT \k -> do
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

instance applicativeParCont :: MonadEff eff m => Applicative (ParCont m) where
  pure = parallel <<< pure

instance altParCont :: MonadEff eff m => Alt (ParCont m) where
  alt (ParCont c1) (ParCont c2) = ParCont $ ContT \k -> do
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

instance plusParCont :: MonadEff eff m => Plus (ParCont m) where
  empty = ParCont $ ContT \_ -> pure unit

instance alternativeParCont :: MonadEff eff m => Alternative (ParCont m)

instance monadParParCont :: MonadEff eff m => Parallel (ParCont m) (ContT Unit m) where
  parallel = ParCont
  sequential (ParCont ma) = ma

unsafeWithRef :: forall eff a. Eff (ref :: REF | eff) a -> Eff eff a
unsafeWithRef = unsafeCoerceEff
