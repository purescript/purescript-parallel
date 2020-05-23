module Learn.Parallel.Syntax where

import Prelude

import Control.Parallel (parOneOf, parTraverse_)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

-- Create an array from 1 to 99 as Strings.
array :: Array String
array = map show $ range 1 99

main :: Effect Unit
main =
  -- Starts running a computation and blocks until it finishes.
  let runComputation computationName computation = do
        liftEffect $ log $ "Running computation in 1 second: " <> computationName
        delay (Milliseconds 1000.0)
        computation

  in launchAff_ do
    runComputation "print all items in array sequentially" do
      for_ array \element -> do
        liftEffect $ log element

    let delayComputationWithRandomAmount = do
          delayAmount <- liftEffect $ randomInt 20 1000
          delay $ Milliseconds $ toNumber delayAmount

    runComputation "print all items in array in parallel" do
      sequential $ for_ array \element -> do
        -- slow down speed of computation based on some random value
        -- to show that things are working in parallel.
        parallel do
          delayComputationWithRandomAmount
          liftEffect $ log element

    -- same computation as before but with less boilerplate.
    runComputation "print all items in array in parallel using parTraverse" do
      array # parTraverse_ \element -> do
        delayComputationWithRandomAmount
        liftEffect $ log element

    runComputation "race multiple computations & stop all others when one finishes" do
      let
        -- same as before
        arrayComputation index = do
          let shownIndex = "Array " <> show index <> ": "
          array # traverse \element -> do
            delayComputationWithRandomAmount
            liftEffect $ log $ shownIndex <> element

      void $ parOneOf [ arrayComputation 1
                      , arrayComputation 2
                      , arrayComputation 3
                      , arrayComputation 4
                      ]
