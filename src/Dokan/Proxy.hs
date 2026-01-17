{-# LANGUAGE OverloadedStrings #-}

module Dokan.Proxy where

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Dokan.Types (Backend (backendHost, backendPort))
import Network.HTTP.Client (RequestBody (RequestBodyLBS))
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (mkStatus, statusCode)
import qualified Network.Wai as W

hopByHopHeaders :: [CI.CI B.ByteString]
hopByHopHeaders =
  [ "connection"
  , "keep-alive"
  , "proxy-authenticate"
  , "proxy-authorization"
  , "te"
  , "trailers"
  , "transfer-encoding"
  , "upgrade"
  ]

mkProxyRequest :: Backend -> W.Request -> IO HC.Request
mkProxyRequest backend waiReq = do
  body <- W.strictRequestBody waiReq
  let req =
        HC.defaultRequest
          { HC.method = W.requestMethod waiReq
          , HC.secure = False
          , HC.host = TE.encodeUtf8 (backendHost backend)
          , HC.port = backendPort backend
          , HC.path = W.rawPathInfo waiReq <> W.rawQueryString waiReq
          , HC.requestHeaders = filter (\(k, _) -> k `notElem` hopByHopHeaders) (W.requestHeaders waiReq)
          , HC.requestBody = RequestBodyLBS body
          }
  pure req

proxyToBackend :: Backend -> W.Request -> IO W.Response
proxyToBackend backend waiReq = do
  manager <- HC.newManager HC.defaultManagerSettings
  proxyReq <- mkProxyRequest backend waiReq
  resp <- HC.httpLbs proxyReq manager
  pure $
    W.responseLBS
      (mkStatus (statusCode $ HC.responseStatus resp) "")
      (filter (\(k, _) -> k `notElem` hopByHopHeaders) (HC.responseHeaders resp))
      (HC.responseBody resp)
