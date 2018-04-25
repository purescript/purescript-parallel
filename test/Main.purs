module Test.Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Parallel (parTraverse)
import Effect (Effect)
import Effect.Console (logShow)

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import getImpl :: Request -> (String -> Effect Unit) -> Effect Unit

get :: Request -> ContT Unit Effect String
get req = ContT (getImpl req)

request :: String -> Request
request host = Request { host: host, path: "/" }

main :: Effect Unit
main = runContT (parTraverse (get <<< request) resources) logShow
  where
    resources :: Array String
    resources =
      [ "www.purescript.org"
      , "try.purescript.org"
      , "community.purescript.org"
      ]
