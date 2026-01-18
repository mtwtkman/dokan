module Dokan.Types (
  RequestFrom (..),
  RoutingTable,
  Protocol (..),
  Backend (..),
  HostName,
  HostPolicy (..),
  Route (..),
  hostPolicyFromBool,
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

data HostPolicy
  = RewriteHost
  | PreserveHost
  deriving (Show, Eq)

hostPolicyFromBool :: Bool -> HostPolicy
hostPolicyFromBool False = RewriteHost
hostPolicyFromBool True = PreserveHost

data Route = Route
  { routeBackend :: Backend
  , routeHostPolicy :: HostPolicy
  }
  deriving (Show, Eq)

type RoutingTable = M.Map RequestFrom Route
