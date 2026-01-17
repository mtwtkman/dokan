module Main where

import Dokan.Config (loadConfig)
import Control.Monad.Except (runExceptT)


main :: IO ()
main = do
  result <- runExceptT (loadConfig "example.yaml")
  case result of
    Right v -> print v
    Left e -> print e
