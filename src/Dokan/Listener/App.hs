{-# LANGUAGE OverloadedStrings #-}

module Dokan.Listener.App (app) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dokan.Proxy.Transfer (proxyToBackend)
import Dokan.Types (
  Protocol (Http),
  RequestFrom (RequestFrom),
  Route,
  RoutingTable,
 )
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (status400, status502)
import Network.Wai (Application, Response, responseLBS, Request, requestHeaders)

type HttpResult a = Either HttpError a
data HttpError = MissingHost | NoRoute deriving (Show, Eq)

app :: Manager -> RoutingTable -> Application
app manager routing req respond = do
  case resolve routing req of
    Left err -> respond (errorResponse err)
    Right (originalHost, route) -> do
      resp <- proxyToBackend manager route (TE.encodeUtf8 originalHost) req
      respond resp

errorResponse :: HttpError -> Response
errorResponse MissingHost = responseLBS status400 [] "Host header missing"
errorResponse NoRoute = responseLBS status502 [] "No backend route"

resolve :: RoutingTable -> Request -> HttpResult (T.Text, Route)
resolve routing req = do
  host <- extractHost req
  route <- maybe (Left NoRoute) Right $ M.lookup (RequestFrom Http host) routing -- TODO: Enalbe Https resolving
  pure (host, route)

extractHost :: Request -> HttpResult T.Text
extractHost req =
  case lookup (CI.mk "Host") (requestHeaders req) of
    Nothing ->
      Left MissingHost
    Just raw ->
      Right $ T.pack $ takeWhile (/= ':') $ BS.unpack raw
