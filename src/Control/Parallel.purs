module Control.Parallel (Parallel(..), runParallel) where

import Data.Maybe

import Control.Alt
import Control.Alternative
import Control.Plus

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Unsafe

import Control.Monad.Cont.Trans

refs :: forall eff a. Eff (ref :: Ref | eff) a -> Eff eff a
refs = unsafeInterleaveEff

par :: forall a b r eff. (a -> b -> r) -> ContT Unit (Eff eff) a -> ContT Unit (Eff eff) b -> ContT Unit (Eff eff) r
par f ca cb = ContT $ \k -> do
  ra <- refs $ newRef Nothing
  rb <- refs $ newRef Nothing
  
  runContT ca $ \a -> do
    mb <- refs $ readRef rb
    case mb of
      Nothing -> refs $ writeRef ra $ Just a
      Just b -> k (f a b)

  runContT cb $ \b -> do
    ma <- refs $ readRef ra
    case ma of
      Nothing -> refs $ writeRef rb $ Just b
      Just a -> k (f a b)

race :: forall a eff. ContT Unit (Eff eff) a -> ContT Unit (Eff eff) a -> ContT Unit (Eff eff) a
race c1 c2 = ContT $ \k -> do
  done <- refs $ newRef false

  runContT c1 $ \a -> do
    b <- refs $ readRef done
    if b
      then return unit
      else do
        refs $ writeRef done true
        k a

  runContT c2 $ \a -> do
    b <- refs $ readRef done
    if b
      then return unit
      else do
        refs $ writeRef done true
        k a

newtype Parallel eff a = Parallel (ContT Unit (Eff eff) a)

runParallel :: forall eff a. Parallel eff a -> ContT Unit (Eff eff) a
runParallel (Parallel c) = c

instance functorParallel :: Functor (Parallel eff) where
  (<$>) f (Parallel c) = Parallel (f <$> c)

instance applyParallel :: Apply (Parallel eff) where
  (<*>) (Parallel f) (Parallel x) = Parallel (par ($) f x)

instance applicativeParallel :: Applicative (Parallel eff) where
  pure a = Parallel $ pure a

instance altParallel :: Alt (Parallel eff) where
  (<|>) (Parallel c1) (Parallel c2) = Parallel (race c1 c2)

instance plusParallel :: Plus (Parallel eff) where
  empty = Parallel $ ContT $ \_ -> return unit

instance alternativeParallel :: Alternative (Parallel eff)
