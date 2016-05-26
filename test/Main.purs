module Test.Main where

import Prelude

import Control.Alt
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Parallel.Class
import Data.Function.Uncurried (Fn2, runFn2)

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import data HTTP :: !

foreign import getImpl
  :: forall eff
   . Fn2 Request
         (String -> Eff (http :: HTTP | eff) Unit)
         (Eff (http :: HTTP | eff) Unit)

get :: forall eff. Request -> ContT Unit (Eff (http :: HTTP | eff)) String
get req = ContT \k -> runFn2 getImpl req k

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
