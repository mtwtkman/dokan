{-# LANGUAGE OverloadedStrings #-}

module Dokan.Listener.App (app) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dokan.Proxy.Transfer (proxyToBackend)
import Dokan.Types (Backend, RoutingTable)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (status400, status502)
import Network.Wai (Application, Request, Response, requestHeaders, responseLBS)

type HttpResult a = Either HttpError a
data HttpError = MissingHost | NoRoute deriving (Show, Eq)

app :: Bool -> Manager -> RoutingTable -> Application
app isSecure manager routing req respond = do
  case resolve routing req of
    Left err -> respond (errorResponse err)
    Right (originalHost, route) -> do
      resp <- proxyToBackend isSecure manager route (TE.encodeUtf8 originalHost) req
      respond resp

errorResponse :: HttpError -> Response
errorResponse MissingHost = responseLBS status400 [] "Host header missing"
errorResponse NoRoute = responseLBS status502 [] "No backend route"

resolve :: RoutingTable -> Request -> HttpResult (T.Text, Backend)
resolve routing req = do
  host <- extractHost req
  backend <- maybe (Left NoRoute) Right $ M.lookup (T.unpack host) routing
  pure (host, backend)

extractHost :: Request -> HttpResult T.Text
extractHost req =
  case lookup (CI.mk "Host") (requestHeaders req) of
    Nothing ->
      Left MissingHost
    Just raw ->
      Right $ T.pack $ takeWhile (/= ':') $ BS.unpack raw
