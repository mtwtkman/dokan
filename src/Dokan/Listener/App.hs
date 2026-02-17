{-# LANGUAGE OverloadedStrings #-}

module Dokan.Listener.App (app) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.List (intercalate, find)
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dokan.Proxy.Transfer (proxyToBackend)
import Dokan.Types (
  Backend,
  DokanConfig (DokanConfig),
  HostExactIndexId (HostExactIndexId),
  HostPattern (HostExact, HostWildcard),
  Route (Route),
 )
import Dokan.Utils (splitBy)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (status400, status502)
import Network.Wai (Application, Request, Response, requestHeaders, responseLBS)

type HttpResult a = Either HttpError a
data HttpError = MissingHost | NoRoute deriving (Show, Eq)

app :: Manager -> DokanConfig -> Application
app manager config req respond = do
  case resolve config req of
    Left err -> respond (errorResponse err)
    Right (originalHost, route) -> do
      resp <- proxyToBackend manager route (TE.encodeUtf8 originalHost) req
      respond resp

errorResponse :: HttpError -> Response
errorResponse MissingHost = responseLBS status400 [] "Host header missing"
errorResponse NoRoute = responseLBS status502 [] "No backend route"

resolve :: DokanConfig -> Request -> HttpResult (T.Text, Backend)
resolve config req = do
  host <- extractHost req
  backend <- maybe (Left NoRoute) Right $ findBackend config host
  pure (host, backend)

extractHost :: Request -> HttpResult T.Text
extractHost req =
  case lookup (CI.mk "Host") (requestHeaders req) of
    Nothing ->
      Left MissingHost
    Just raw ->
      Right $ T.pack $ takeWhile (/= ':') $ BS.unpack raw

findBackend :: DokanConfig -> T.Text -> Maybe Backend
findBackend (DokanConfig exactMap wildcards) host =
  case M.lookup (HostExactIndexId (T.unpack host)) exactMap of
    Just (Route _ _ backend) -> Just backend
    Nothing ->
       case find (isMatchWildcard (intercalate "." $ "*" : drop 1 (splitBy "." (T.unpack host)))) wildcards of
            Just (Route _ _ backend) -> Just backend
            Nothing -> Nothing

 where
   isMatchWildcard :: String -> Route -> Bool
   isMatchWildcard _ (Route (HostExact _) _ _) = False
   isMatchWildcard ptn (Route (HostWildcard (_, w)) _ _) = ptn == w
