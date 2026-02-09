module Dokan.Listener.Https (
  runHttps,
) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Dokan.Config (DokanConfig (DokanConfig))
import Dokan.Listener.App (app)
import Dokan.Types (
  CertStore (CertStore),
  HostPatternSet (HostExacts, HostWildcards),
  LoadedCert (LoadedCert),
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket (HostName)
import Network.TLS (Credential, Credentials (Credentials))
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettingsSni)

runHttps :: DokanConfig -> IO ()
runHttps (DokanConfig routing certStore) = do
  let warp = setPort 8443 defaultSettings
  tls <- makeTlsSettings certStore
  manager <- newManager defaultManagerSettings
  runTLS tls warp (app manager routing)

makeTlsSettings :: CertStore -> IO TLSSettings
makeTlsSettings certs = return $ tlsSettingsSni (lookupCert certs)

lookupCert :: CertStore -> Maybe HostName -> IO Credentials
lookupCert _ Nothing = error "host name must be provided"
lookupCert (CertStore certs) (Just host) =
  return $
    Credentials
      ( map
          ( \c -> case findHost c host of
              Nothing -> error "host name not found"
              Just v -> v
          )
          certs
      )

findHost :: LoadedCert -> HostName -> Maybe Credential
findHost (LoadedCert cred (HostExacts hosts)) host = if host `elem` hosts then Just cred else Nothing
findHost (LoadedCert cred (HostWildcards hosts)) host
  | take 2 host /= "*." = Nothing
  | host `elem` domains = Just cred
  | otherwise = Nothing
 where
  domains = NE.map (L.intercalate "" . tail . splitBy '.') hosts

splitBy :: Char -> String -> [String]
splitBy delimiter s = case L.span (/= '.') s of
  (x, _ : y) -> x : splitBy delimiter y
  (x, "") -> [x]
