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

import Data.Word (Word8, Word16)
import qualified Data.Map as M
import Network.Socket (HostName)
import Network.TLS (Credential)

data IP
  = IPv4 Word8 Word8 Word8 Word8
  | IPv6 Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16
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
  { dokanHostExacts :: HostExactMap
  , dokanHostWildcards :: [Route]
  }
  deriving (Show, Eq)
