module Main where

import Data.Function
import Data.Traversable

import Debug.Trace

import Control.Parallel

import Control.Alt
import Control.Monad.Eff
import Control.Monad.Cont.Trans

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import data HTTP :: !

foreign import getImpl """
  function getImpl(opts, done) {
    return function() {
      require('http').request(opts, function(res) {
        var body = '';
        res.setEncoding('utf8');
        res.on('data', function (s) {
          body += s;
        });
        res.on('end', function () {
          done(body)();
        });
      }).end();
    };
  }""" :: forall eff. Fn2 Request 
                          (String -> Eff (http :: HTTP | eff) Unit) 
                          (Eff (http :: HTTP | eff) Unit)

get :: forall eff. Request -> Parallel (http :: HTTP | eff) String
get req = withCallback $ \k -> runFn2 getImpl req k

request :: String -> Request
request host = Request { host: host, path: "/" }

app :: forall eff. ContT Unit (Eff (http :: HTTP | eff)) [String]
app = runParallel $ traverse (get <<< request) resources
  where
  resources :: [String]
  resources = 
    [ "www.purescript.org"
    , "try.purescript.org"
    , "community.purescript.org"
    ]

main = runContT app print

