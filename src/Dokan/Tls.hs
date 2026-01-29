{-# LANGUAGE OverloadedStrings #-}

module Dokan.Tls where

import Dokan.Types (CertStore)
import Network.TLS (ServerHooks)
import Network.TLS.Extra.Cipher ()

data TlsError
  = NotFoundX509FormattedCert String
  | WildcardCertificationNotAllowed
  deriving (Show, Eq)

type TlsResult a = Either TlsError a

sniHooks :: CertStore -> ServerHooks
sniHooks certs = undefined
