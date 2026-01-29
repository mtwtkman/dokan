{-# LANGUAGE OverloadedStrings #-}

module Dokan.Proxy.Headers (
  rewriteHostHeaders,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Dokan.Types (Backend (..))
import Network.HTTP.Types (RequestHeaders)

rewriteHostHeaders ::
  Backend ->
  B.ByteString ->
  RequestHeaders ->
  RequestHeaders
rewriteHostHeaders backend originalHost headers =
  addForwardedHeaders
    originalHost
    (replaceHost backend headers)

replaceHost :: Backend -> RequestHeaders -> RequestHeaders
replaceHost backend =
  (hostHeader backend :) . filter (not . isHostHeader)

hostHeader :: Backend -> (CI.CI B.ByteString, B.ByteString)
hostHeader backend =
  (CI.mk "Host", backendHostPort backend)

backendHostPort :: Backend -> B.ByteString
backendHostPort backend =
  TE.encodeUtf8 (backendHost backend) <> ":" <> BC.pack (show (backendPort backend))

isHostHeader :: (CI.CI B.ByteString, B.ByteString) -> Bool
isHostHeader (k, _) = CI.foldCase k == "host"

addForwardedHeaders :: B.ByteString -> RequestHeaders -> RequestHeaders
addForwardedHeaders originalHost headers =
  headers
    <> [ ("X-Forwarded-Host", originalHost)
       , ("X-Forwarded-Proto", "http")
       ]
