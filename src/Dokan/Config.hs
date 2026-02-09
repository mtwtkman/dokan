{-# LANGUAGE OverloadedStrings #-}

module Dokan.Config (
  loadConfig,
  DokanConfig (..),
) where

import Control.Exception (try)
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
  HostPatternSet (..),
  LoadedCert (LoadedCert),
  RoutingTable,
 )
import Network.Socket (HostName)
import Network.TLS (Credential, credentialLoadX509)

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
buildCertStore configs = CertStore . sortByHostPattern <$> traverse loadTlsConfig configs

sortByHostPattern :: [LoadedCert] -> [LoadedCert]
sortByHostPattern certs = let (e, w) = go ([], []) certs in e <> w
 where
  go :: ([LoadedCert], [LoadedCert]) -> [LoadedCert] -> ([LoadedCert], [LoadedCert])
  go acc [] = acc
  go (es, ws) (x@(LoadedCert _ (HostExacts _)) : xs) = go (x : es, ws) xs
  go (es, ws) (x@(LoadedCert _ (HostWildcards _)) : xs) = go (es, x : ws) xs

loadTlsConfig :: RawTlsConfig -> ExceptT ConfigError IO LoadedCert
loadTlsConfig (RawTlsConfig cert key hosts) = do
  cred <- readCredential cert key
  LoadedCert cred <$> liftEither (validateHostPattern hosts)
 where
  readCredential :: T.Text -> T.Text -> ExceptT ConfigError IO Credential
  readCredential cp kp = do
    result <- liftIO $ try (credentialLoadX509 (T.unpack cp) (T.unpack kp)) :: ExceptT ConfigError IO (Either IOError (Either String Credential))
    case result of
      Right (Right c) -> return c
      Right (Left e) -> throwError $ CannotLoadTlsCert e
      Left e -> throwError $ CannotLoadTlsCert (show e)

  liftEither :: ConfigParseResult a -> ExceptT ConfigError IO a
  liftEither = either throwError return

validateHostPattern :: NE.NonEmpty HostName -> ConfigParseResult HostPatternSet
validateHostPattern hosts =
  let (wildcards, exacts) = NE.partition isWildcardHost hosts
   in case (NE.nonEmpty wildcards, NE.nonEmpty exacts) of
        (Just ws, Nothing) -> Right $ HostWildcards ws
        (Nothing, Just es) -> Right $ HostExacts es
        (Nothing, Nothing) -> Left NoHostNameDetected
        (Just _, Just _) -> Left CannotCombineHostPattern

isWildcardHost :: HostName -> Bool
isWildcardHost = (== "*.") . take 2
