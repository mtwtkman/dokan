module Dokan.Types (
  DokanConfig (..),
  IP (..),
  HostPattern (..),
  Route (..),
  HostExactMap,
  HostExactIndexId (..),
  HostScheme (..),
  Backend(..),
) where

import qualified Data.Map as M
import Network.Socket (HostName)
import Network.TLS (Credential)

data IP
  = IPv4 String String String String
  | IPv6 String String String String String String String String
  deriving (Show, Eq)

data HostScheme = Http | Https Credential
  deriving (Show, Eq)

data HostPattern
  = HostExact (HostScheme, HostName)
  | HostWildcard (HostScheme, HostName)
  deriving (Show, Eq)

data Backend = Backend
  { backendHost :: String
  , backendPort :: Int
  }
  deriving (Show, Eq)

data Route = Route
  { routeHostPattern :: HostPattern
  , routeDns :: IP
  , routeBackend :: Backend
  }
  deriving (Show, Eq)

newtype HostExactIndexId = HostExactIndexId HostName deriving (Show, Eq, Ord)

type HostExactMap = M.Map HostExactIndexId Route

data DokanConfig = DokanConfig
  { dokanHostExactMap :: HostExactMap
  , dokanHostWildcards :: [Route]
  }
  deriving (Show, Eq)
