{-# LANGUAGE OverloadedStrings #-}

module Dokan.Config (
  loadConfig,
  DokanConfig (..),
) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (
  FromJSON (parseJSON),
  decodeFileThrow,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Dokan.Types (
  Backend (Backend),
  CertStore (CertStore),
  HostName,
  HostPatternSet (..),
  LoadedCert (LoadedCert),
  RoutingTable,
 )
import Network.TLS (credentialLoadX509)

data DokanConfig = DokanConfig
  { dokanRoutingTable :: RoutingTable
  , dokanCertStore :: CertStore
  }
  deriving (Show)

data ConfigError
  = InvalidBackendFormat T.Text
  | InvalidPort T.Text
  | CannotLoadTlsCert String
  | CannotCombineHostPattern
  | NoHostNameDetected
  deriving (Eq, Show)

type ConfigParseResult a = Either ConfigError a

data RawTlsConfig = RawTlsConfig
  { rawTlsCert :: T.Text
  , rawTlsKey :: T.Text
  , rawTlsHosts :: NE.NonEmpty HostName
  }
  deriving (Show)

instance FromJSON RawTlsConfig where
  parseJSON = withObject "RawTlsConfig" $ \o ->
    RawTlsConfig
      <$> o .: "cert"
      <*> o .: "key"
      <*> o .: "hosts"

data RawConfig = RawConfig
  { rawHosts :: M.Map HostName T.Text
  , rawTLsConfig :: [RawTlsConfig]
  }
  deriving (Show)

instance FromJSON RawConfig where
  parseJSON = withObject "RawConfig" $ \o ->
    RawConfig
      <$> o .: "hosts"
      <*> o .:? "tls" .!= []

loadConfig :: FilePath -> ExceptT ConfigError IO DokanConfig
loadConfig path = do
  raw <- decodeFileThrow path
  buildDokanConfig raw

buildDokanConfig :: RawConfig -> ExceptT ConfigError IO DokanConfig
buildDokanConfig (RawConfig hostMapping tlsCerts) = do
  case traverse parseBackend hostMapping of
    Right routingTable -> do
      certStore <- buildCertStore tlsCerts
      return $ DokanConfig routingTable certStore
    Left e -> throwError e

parseBackend :: T.Text -> ConfigParseResult Backend
parseBackend value =
  case T.splitOn ":" value of
    [host, portTxt] ->
      case readMaybeInt portTxt of
        Just port -> Right $ Backend host port
        Nothing -> Left $ InvalidPort value
    _ -> Left $ InvalidBackendFormat value
 where
  readMaybeInt :: T.Text -> Maybe Int
  readMaybeInt t =
    case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

buildCertStore :: [RawTlsConfig] -> ExceptT ConfigError IO CertStore
buildCertStore configs = CertStore <$> traverse loadTlsConfig configs

loadTlsConfig :: RawTlsConfig -> ExceptT ConfigError IO LoadedCert
loadTlsConfig (RawTlsConfig cert key hosts) = do
  cred <- liftIO $ credentialLoadX509 (T.unpack cert) (T.unpack key)
  case cred of
    Right c -> do
      case validateHostPattern hosts of
        Right hps -> return $ LoadedCert c hps
        Left e -> throwError e
    Left e -> throwError $ CannotLoadTlsCert e

validateHostPattern :: NE.NonEmpty HostName -> ConfigParseResult HostPatternSet
validateHostPattern hosts =
  let (wildcards, exacts) = NE.partition isWildcardHost hosts
   in case (NE.nonEmpty wildcards, NE.nonEmpty exacts) of
        (Just ws, Nothing) -> Right $ HostWildcards ws
        (Nothing, Just es) -> Right $ HostExacts es
        (Nothing, Nothing) -> Left NoHostNameDetected
        (Just _, Just _) -> Left CannotCombineHostPattern

isWildcardHost :: HostName -> Bool
isWildcardHost = T.isPrefixOf "*."
