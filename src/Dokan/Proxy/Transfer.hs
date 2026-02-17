{-# LANGUAGE OverloadedStrings #-}

module Dokan.Proxy.Transfer (proxyToBackend) where

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dokan.Proxy.Headers (rewriteHostHeaders)
import Dokan.Types (Backend (backendHost, backendPort))
import Network.HTTP.Client (RequestBody (RequestBodyLBS))
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (RequestHeaders, mkStatus, statusCode)
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

mkProxyRequest :: Backend -> RequestHeaders -> W.Request -> IO HC.Request
mkProxyRequest backend headers waiReq = do
  body <- W.lazyRequestBody waiReq
  let req =
        HC.defaultRequest
          { HC.method = W.requestMethod waiReq
          , HC.secure = False
          , HC.host = TE.encodeUtf8 (T.pack $ backendHost backend)
          , HC.port = backendPort backend
          , HC.path = W.rawPathInfo waiReq <> W.rawQueryString waiReq
          , HC.requestHeaders = headers
          , HC.requestBody = RequestBodyLBS body
          }
  pure req

proxyToBackend :: HC.Manager -> Backend -> B.ByteString -> W.Request -> IO W.Response
proxyToBackend manager backend originalHost waiReq = do
  let headers' = rewriteHostHeaders backend originalHost (W.requestHeaders waiReq)
  proxyReq <- mkProxyRequest backend headers' waiReq
  resp <- HC.httpLbs proxyReq manager
  pure $
    W.responseLBS
      (mkStatus (statusCode $ HC.responseStatus resp) "")
      (filter (\(k, _) -> k `notElem` hopByHopHeaders) (HC.responseHeaders resp))
      (HC.responseBody resp)
