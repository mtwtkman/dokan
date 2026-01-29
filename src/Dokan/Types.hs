module Dokan.Types (
  RoutingTable,
  Backend (..),
  HostName,
  HostPatternSet (..),
  LoadedCert (..),
  CertStore (..),
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Network.TLS (Credential)

type HostName = T.Text

data Backend = Backend
  { backendHost :: T.Text
  , backendPort :: Int
  }
  deriving (Eq, Show)

type RoutingTable = M.Map HostName Backend

data HostPatternSet
  = HostExacts (NonEmpty HostName)
  | HostWildcards (NonEmpty HostName)
  deriving (Show, Eq, Ord)

data LoadedCert = LoadedCert
  { lcCredential :: Credential
  , lcHostPatterns :: HostPatternSet
  }
  deriving (Show)

newtype CertStore = CertStore
  { unCertStore :: [LoadedCert]
  }
  deriving (Show)
