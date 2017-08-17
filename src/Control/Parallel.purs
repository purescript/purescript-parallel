module Control.Parallel
  ( parTraverse
  , parTraverse_
  , parSequence
  , parSequence_
  , parOneOf
  , module Control.Parallel.Class
  ) where

import Prelude

import Control.Alternative (class Alternative, empty, alt)
import Control.Parallel.Class (class Parallel, parallel, sequential, ParCont(..))

import Data.Foldable (class Foldable, traverse_, oneOf, foldr)
import Data.Traversable (class Traversable, traverse)

-- | Traverse a collection in parallel.
parTraverse
  :: forall f m t a b
   . Parallel f m
  => Traversable t
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f = sequential <<< traverse (parallel <<< f)

-- | Traverse a collection in parallel, discarding any results.
parTraverse_
  :: forall f m t a b
   . Parallel f m
  => Foldable t
  => (a -> m b)
  -> t a
  -> m Unit
parTraverse_ f = sequential <<< traverse_ (parallel <<< f)

parSequence
  :: forall a t m f
   . Parallel f m
  => Traversable t
  => t (m a)
  -> m (t a)
parSequence = parTraverse id

parSequence_
  :: forall a t m f
   . Parallel f m
  => Foldable t
  => t (m a)
  -> m Unit
parSequence_ = parTraverse_ id

-- | Race a collection in parallel.
parOneOf
  :: forall a t m f
   . Parallel f m
  => Alternative f
  => Foldable t
  => Functor t
  => t (m a)
  -> m a
parOneOf = sequential <<< oneOf <<< map parallel

-- | Race a collection in parallel while mapping to some effect.
parOneOfMap
  :: forall a b t m f
   . Parallel f m
  => Alternative f
  => Foldable t
  => Functor t
  => (a -> m b)
  -> t a
  -> m b
parOneOfMap f = sequential <<< foldr (alt <<< parallel <<< f) empty
