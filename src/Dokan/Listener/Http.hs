{-# LANGUAGE OverloadedStrings #-}

module Dokan.Listener.Http (
  runHttp,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dokan.Proxy (proxyToBackend)
import Dokan.Types (
  Backend (..),
  Protocol (Http),
  RequestFrom (RequestFrom),
  RoutingTable,
 )
import Network.HTTP.Types (status400, status502)
import Network.Wai (
  Application,
  Request,
  Response,
  requestHeaders,
  responseLBS,
 )
import Network.Wai.Handler.Warp (run)

data HttpError = MissingHost | NoRoute deriving (Show, Eq)
type HttpResult a = Either HttpError a

runHttp :: RoutingTable -> IO ()
runHttp routing =
  run 8080 (app routing)

app :: RoutingTable -> Application
app routing req respond = do
  case resolve routing req of
    Left err -> respond (errorResponse err)
    Right (_, backend) -> do
      resp <- proxyToBackend backend req
      respond resp

errorResponse :: HttpError -> Response
errorResponse MissingHost = responseLBS status400 [] "Host header missing"
errorResponse NoRoute = responseLBS status502 [] "No backend route"

extractHost :: Request -> HttpResult T.Text
extractHost req =
  case lookup (CI.mk "Host") (requestHeaders req) of
    Nothing ->
      Left MissingHost
    Just raw ->
      Right $ T.pack $ takeWhile (/= ':') $ BS.unpack raw

resolve :: RoutingTable -> Request -> HttpResult (T.Text, Backend)
resolve routing req = do
  host <- extractHost req
  backend <- maybe (Left NoRoute) Right $ M.lookup (RequestFrom Http host) routing
  pure (host, backend)
