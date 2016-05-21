module Test.Main where

import Prelude

import Data.Traversable (traverse)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel (Parallel, runParallelWith, withCallback)

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import data HTTP :: !

foreign import getImpl
  :: forall eff
   . Request
  -> (String -> Eff (http :: HTTP | eff) Unit)
  -> Eff (http :: HTTP | eff) Unit

get :: forall eff. Request -> Parallel (http :: HTTP | eff) String
get req = withCallback (getImpl req)

request :: String -> Request
request host = Request { host: host, path: "/" }

main :: forall eff. Eff (http :: HTTP, console :: CONSOLE | eff) Unit
main = runParallelWith logShow $ traverse (get <<< request) resources
  where
  resources :: Array String
  resources =
    [ "www.purescript.org"
    , "try.purescript.org"
    , "community.purescript.org"
    ]

