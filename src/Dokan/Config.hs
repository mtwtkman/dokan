{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Dokan.Config (
  loadConfig,
) where

import Control.Exception (try)
import Control.Monad (join)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Yaml (
  FromJSON (parseJSON),
  decodeFileThrow,
  withObject,
  (.:),
  (.:?),
 )
import Dokan.Types (
  Backend (Backend),
  DokanConfig (DokanConfig),
  HostExactIndexId (HostExactIndexId),
  HostExactMap,
  HostPattern (HostExact, HostWildcard),
  HostScheme (Http, Https),
  IP (IPv4, IPv6),
  Route (Route),
 )
import Dokan.Utils (splitBy)
import Network.TLS (Credential, credentialLoadX509)
import qualified Network.URI as URI

data ConfigError
  = CannotLoadTlsCert String
  | WildcardMustBeSingleSubdomain
  | InvalidWildcardFormat
  | PortNotFound
  | UrlAuthorityNotFound
  | FailedParsingUrl
  | InvalidIPAddressFormat
  deriving (Eq, Show)

newtype RawDnsConfig = RawDnsConfig
  { rawDefaultAddress :: String
  }
  deriving (Show)

instance FromJSON RawDnsConfig where
  parseJSON = withObject "RawDnsConfig" $ \o ->
    RawDnsConfig <$> o .: "defaultAddress"

data RawTlsConfig = RawTlsConfig
  { rawCert :: FilePath
  , rawKey :: FilePath
  }
  deriving (Show)

instance FromJSON RawTlsConfig where
  parseJSON = withObject "RawTlsConfig" $ \o ->
    RawTlsConfig
      <$> o .: "cert"
      <*> o .: "key"

newtype RawProxyConfig = RawProxyConfig
  { rawUpstream :: String
  }
  deriving (Show)

instance FromJSON RawProxyConfig where
  parseJSON = withObject "RawProxyConfig" $ \o ->
    RawProxyConfig <$> o .: "upstream"

newtype RawOverwriteDnsConfig = RawOverwriteDnsConfig
  { rawOverwriteDnsAddress :: String
  }
  deriving (Show)

instance FromJSON RawOverwriteDnsConfig where
  parseJSON = withObject "RawOverwriteDnsConfig" $ \o ->
    RawOverwriteDnsConfig <$> o .: "address"

data RawHostConfig = RawHostConfig
  { rawHostNames :: [String]
  , rawTls :: Maybe RawTlsConfig
  , rawProxy :: RawProxyConfig
  , rawOverwriteDns :: Maybe RawOverwriteDnsConfig
  }
  deriving (Show)

instance FromJSON RawHostConfig where
  parseJSON = withObject "RawHostConfig" $ \o ->
    RawHostConfig
      <$> o .: "names"
      <*> o .:? "tls"
      <*> o .: "proxy"
      <*> o .:? "dns"

data RawConfig = RawConfig
  { rawDns :: RawDnsConfig
  , rawHosts :: [RawHostConfig]
  }
  deriving (Show)

instance FromJSON RawConfig where
  parseJSON = withObject "RawConfig" $ \o ->
    RawConfig
      <$> o .: "dns"
      <*> o .: "hosts"

loadConfig :: FilePath -> ExceptT ConfigError IO DokanConfig
loadConfig configPath = decodeFileThrow configPath >>= buildDokanConfig

buildDokanConfig :: RawConfig -> ExceptT ConfigError IO DokanConfig
buildDokanConfig (RawConfig dnsConfig hostsConfig) = do
  routes <- traverse (buildRoutes dnsConfig) hostsConfig
  let routes' = join routes
  return $ DokanConfig (mkHostExactMap routes') (filter isWildcard routes')

buildRoutes :: RawDnsConfig -> RawHostConfig -> ExceptT ConfigError IO [Route]
buildRoutes (RawDnsConfig defaultDns) (RawHostConfig names tls proxy customDns) = do
  let dns = maybe defaultDns rawOverwriteDnsAddress customDns
  traverse (toRoute tls proxy dns) names

toRoute :: Maybe RawTlsConfig -> RawProxyConfig -> String -> String -> ExceptT ConfigError IO Route
toRoute tls proxy dns name =
  Route
    <$> buildHostPattern tls name
    <*> toIPAddress dns
    <*> buildBackend proxy

buildHostPattern :: Maybe RawTlsConfig -> String -> ExceptT ConfigError IO HostPattern
buildHostPattern tls s
  | wildcardCount > 1 = throwError WildcardMustBeSingleSubdomain
  | take 1 s /= "*" && wildcardCount > 0 = throwError InvalidWildcardFormat
  | wildcardCount == 0 = detectHostScheme tls <&> HostExact . (,s)
  | otherwise = detectHostScheme tls <&> HostWildcard . (,s)
 where
  wildcardCount = length (L.elemIndices '*' s)

detectHostScheme :: Maybe RawTlsConfig -> ExceptT ConfigError IO HostScheme
detectHostScheme Nothing = return Http
detectHostScheme (Just (RawTlsConfig cert key)) = do
  result <- liftIO $ try (credentialLoadX509 cert key) :: ExceptT ConfigError IO (Either IOError (Either String Credential))
  case result of
    Right (Right c) -> return $ Https c
    Right (Left _) -> throwError $ CannotLoadTlsCert "Reason: unknown"
    Left e -> throwError $ CannotLoadTlsCert (show e)

toIPAddress :: String -> ExceptT ConfigError IO IP
toIPAddress s =
  let maybeV4 = splitBy "." s
      maybeV6 = splitBy "::" s
   in case (maybeV4, maybeV6) of
        ([v1, v2, v3, v4], _) -> return $ IPv4 v1 v2 v3 v4
        (_, [v1, v2, v3, v4, v5, v6, v7, v8]) -> return $ IPv6 v1 v2 v3 v4 v5 v6 v7 v8
        _ -> throwError InvalidIPAddressFormat

mkHostExactMap :: [Route] -> HostExactMap
mkHostExactMap = go M.empty
 where
  go :: HostExactMap -> [Route] -> HostExactMap
  go acc [] = acc
  go acc (r@(Route (HostExact (_, n)) _ _) : rest) = go (M.insert (HostExactIndexId n) r acc) rest
  go acc ((Route (HostWildcard _) _ _ : rest)) = go acc rest

buildBackend :: (MonadFail m) => RawProxyConfig -> ExceptT ConfigError m Backend
buildBackend (RawProxyConfig uri) = case URI.parseURI uri of
  Nothing -> throwError FailedParsingUrl
  Just v -> case URI.uriAuthority v of
    Just (URI.URIAuth _ host port) -> Backend host <$> extendPort port
    Nothing -> throwError UrlAuthorityNotFound
 where
  extendPort :: (MonadFail m) => String -> ExceptT ConfigError m Int
  extendPort s = case splitBy ":" s of
    _ : [port] -> return (read port)
    _ -> throwError PortNotFound

isWildcard :: Route -> Bool
isWildcard (Route (HostExact _) _ _) = False
isWildcard (Route (HostWildcard _) _ _) = True
