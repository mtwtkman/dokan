module Main where

import Control.Monad.Except (runExceptT)
import Data.Functor ((<&>))
import Dokan.Config (loadConfig)
import Dokan.Listener.Http (runHttp)
import System.Environment (getArgs)

defaultRouteMap :: FilePath
defaultRouteMap = "dokan.yaml"

main :: IO ()
main = do
  configName <- getArgs <&> \a -> if not (null a) then head a else defaultRouteMap
  result <- runExceptT (loadConfig configName)
  case result of
    Right routing -> do
      putStrLn $ "Proxy with " <> configName
      runHttp routing
    Left e -> print e
