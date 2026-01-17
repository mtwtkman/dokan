module Main where

import Dokan.Config (loadConfig)
import Control.Monad.Except (runExceptT)
import Dokan.Listener.Http (runHttp)

main :: IO ()
main = do
  result <- runExceptT (loadConfig "example.yaml")
  case result  of
    Right routing -> runHttp routing
    Left e -> print e

