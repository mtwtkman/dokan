module Dokan.Types (
  RequestFrom (..),
  RoutingTable,
  Protocol (..),
  Backend (..),
  HostName,
) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Protocol
  = Http
  | Https
  deriving
    (Eq, Ord, Show)

type HostName = T.Text

data RequestFrom = RequestFrom Protocol HostName deriving (Show, Eq, Ord)

data Backend = Backend
  { backendHost :: T.Text
  , backendPort :: Int
  }
  deriving (Eq, Show)

type RoutingTable = M.Map RequestFrom Backend
