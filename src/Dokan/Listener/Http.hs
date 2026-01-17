{-# LANGUAGE OverloadedStrings #-}

module Dokan.Listener.Http (
  runHttp,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dokan.Types (
  Backend (..),
  Protocol (Http),
  RequestFrom (RequestFrom),
  RoutingTable,
 )
import Network.HTTP.Types (status200, status400, status502)
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
  respond $ either errorResponse successResponse $ resolve routing req

errorResponse :: HttpError -> Response
errorResponse MissingHost = responseLBS status400 [] "Host header missing"
errorResponse NoRoute = responseLBS status502 [] "No backend route"

successResponse :: (T.Text, Backend) -> Response
successResponse (host, backend) = responseLBS status200 [] (LB.fromStrict $ debugResponse host backend)

extractHost :: Request -> HttpResult T.Text
extractHost req =
  case lookup (CI.mk "Host") (requestHeaders req) of
    Nothing ->
      Left MissingHost
    Just raw ->
      Right $ T.pack $ takeWhile (/= ':') $ BS.unpack raw

debugResponse :: T.Text -> Backend -> BS.ByteString
debugResponse host backend =
  "host="
    <> TE.encodeUtf8 host
    <> "\nbackend="
    <> TE.encodeUtf8 (backendHost backend)
    <> ":"
    <> fromString (show (backendPort backend))

resolve :: RoutingTable -> Request -> HttpResult (T.Text, Backend)
resolve routing req = do
  host <- extractHost req
  backend <- maybe (Left NoRoute) Right $ M.lookup (RequestFrom Http host) routing
  pure (host, backend)
