module Main where

import Control.Concurrent.Async (concurrently_)
import Control.Monad.Except (runExceptT)
import Data.Functor ((<&>))
import Dokan.Config (loadConfig, DokanConfig(dokanRoutingTable))
import Dokan.Listener.Http (runHttp)
import Dokan.Listener.Https (runHttps)
import System.Environment (getArgs)

defaultRouteMap :: FilePath
defaultRouteMap = "dokan.yaml"

main :: IO ()
main = do
  configName <- getArgs <&> \a -> if null a then defaultRouteMap else head a
  result <- runExceptT (loadConfig configName)
  case result of
    Right config -> do
      putStrLn $ "Dokan using " <> configName <> " to route"
      concurrently_
        (runHttp (dokanRoutingTable config))
        (runHttps config)
    Left e -> print e
