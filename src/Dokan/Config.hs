{-# LANGUAGE OverloadedStrings #-}

module Dokan.Config (
  loadConfig,
) where

import Control.Monad.Except (ExceptT)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (FromJSON (parseJSON), decodeFileThrow, withObject, (.!=), (.:?))

data Protocol
  = Http
  | Https
  deriving
    (Eq, Ord, Show)

type HostName = T.Text

data Backend = Backend
  { backendHost :: T.Text
  , backendPort :: Int
  }
  deriving (Eq, Show)

data RequestFrom = RequestFrom Protocol HostName deriving (Show, Eq, Ord)

type RoutingTable = M.Map RequestFrom Backend

data ConfigError
  = InvalidBackendFormat Protocol T.Text
  | InvalidPort Protocol T.Text
  deriving (Eq, Show)

type ConfigParseResult a = Either ConfigError a

data RawConfig = RawConfig
  { rawHttp :: M.Map HostName T.Text
  , rawHttps :: M.Map HostName T.Text
  }
  deriving (Show)

instance FromJSON RawConfig where
  parseJSON = withObject "RawConfig" $ \o ->
    RawConfig
      <$> o .:? "http" .!= M.empty
      <*> o .:? "https" .!= M.empty

loadConfig :: FilePath -> ExceptT ConfigError IO RoutingTable
loadConfig path = do
  raw <- decodeFileThrow path
  case buildRoutingTable raw of
    Right t -> pure t
    Left e -> fail (renderConfigError e)
 where
  renderConfigError :: ConfigError -> String
  renderConfigError (InvalidBackendFormat proto v) = buildErrorMessage proto v "Expected format <host>:<port>"
  renderConfigError (InvalidPort proto v) = buildErrorMessage proto v ": Invalid port number detected"

  buildErrorMessage :: Protocol -> T.Text -> String -> String
  buildErrorMessage proto v msg = show proto <> ": " <> msg <> " but " <> "'" <> T.unpack v <> "'"

buildRoutingTable :: RawConfig -> ConfigParseResult RoutingTable
buildRoutingTable (RawConfig httpRoutes httpsRoutes) = do
  httpTable <- traverse (parseBackend Http) httpRoutes
  httpsTable <- traverse (parseBackend Https) httpsRoutes
  pure $ M.union (makeTable Http httpTable) (makeTable Https httpsTable)
 where
  makeTable :: Protocol -> M.Map HostName Backend -> RoutingTable
  makeTable proto m = M.fromList $ map (first (RequestFrom proto)) (M.assocs m)

parseBackend :: Protocol -> T.Text -> ConfigParseResult Backend
parseBackend proto value =
  case T.splitOn ":" value of
    [host, portTxt] ->
      case readMaybeInt portTxt of
        Just port -> Right $ Backend host port
        Nothing -> Left $ InvalidPort proto value
    _ -> Left $ InvalidBackendFormat proto value

readMaybeInt :: T.Text -> Maybe Int
readMaybeInt t =
  case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing
