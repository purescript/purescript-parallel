module Test.Main where

import Prelude (Unit, (<<<))

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel (parTraverse)

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import data HTTP :: Effect

foreign import getImpl
  :: forall eff
   . Request
  -> (String -> Eff (http :: HTTP | eff) Unit)
  -> Eff (http :: HTTP | eff) Unit

get :: forall eff. Request -> ContT Unit (Eff (http :: HTTP | eff)) String
get req = ContT (getImpl req)

request :: String -> Request
request host = Request { host: host, path: "/" }

main :: forall eff. Eff (http :: HTTP, console :: CONSOLE | eff) Unit
main = runContT (parTraverse (get <<< request) resources) logShow
  where
    resources :: Array String
    resources =
      [ "www.purescript.org"
      , "try.purescript.org"
      , "community.purescript.org"
      ]
