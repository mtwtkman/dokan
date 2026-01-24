module Main where

import Control.Concurrent.Async (concurrently_)
import Control.Monad.Except (runExceptT)
import Data.Functor ((<&>))
import Dokan.Config (loadConfig)
import Dokan.Listener.Http (runHttp)
import Dokan.Listener.Https (runHttps)
import System.Environment (getArgs)

defaultRouteMap :: FilePath
defaultRouteMap = "dokan.yaml"

main :: IO ()
main = do
  configName <- getArgs <&> \a -> if null a then defaultRouteMap else head a
  config <- runExceptT (loadConfig configName)
  case config of
    Right routing -> do
      putStrLn $ "Dokan using " <> configName <> " to route"
      concurrently_
        (runHttp routing)
        (runHttps routing)
    Left e -> print e
