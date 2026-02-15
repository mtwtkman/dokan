module Dokan.Listener.Https (
  runHttps,
) where

import qualified Data.List as L
import Dokan.Utils (splitBy)
import Data.Maybe (isJust)
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
lookupCert _ Nothing = do
  putStrLn "[TLS] ClientHello withou SNI"
  return $ Credentials []
lookupCert (CertStore certs) (Just host) = do
  putStrLn $ "[TLS] SNI received: " <> host
  case L.find (\c -> isJust (findHost c host)) certs of
    Nothing -> return $ Credentials []
    Just c ->
      let Just cred = findHost c host
       in return $ Credentials [cred]

findHost :: LoadedCert -> HostName -> Maybe Credential
findHost (LoadedCert cred (HostExacts hosts)) host = if host `elem` hosts then Just cred else Nothing
findHost (LoadedCert cred (HostWildcards patterns)) host =
  if any (wildcardMatch host) patterns
    then Just cred
    else Nothing

wildcardMatch :: HostName -> HostName -> Bool
wildcardMatch host pattern =
  case pattern of
    ('*' : '.' : rest) ->
      case splitBy '.' host of
        (_ : xs) -> L.intercalate "." xs == rest
        _ -> False
    _ -> False
