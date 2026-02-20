module Dokan.Listener.Https (
  runHttps,
) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Dokan.Listener.App (app)
import Dokan.Types (
  DokanConfig (DokanConfig),
  HostExactIndexId (HostExactIndexId),
  HostPattern (HostExact, HostWildcard),
  HostScheme (Https),
  Route (Route),
 )
import Dokan.Utils (splitBy)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.TLS (Credential, Credentials (Credentials), HostName)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettingsSni)

runHttps :: DokanConfig -> IO ()
runHttps config = do
  let warp = setPort 8443 defaultSettings
  tls <- makeTlsSettings config
  manager <- newManager defaultManagerSettings
  runTLS tls warp (app manager config)

makeTlsSettings :: DokanConfig -> IO TLSSettings
makeTlsSettings config = do
  return $ tlsSettingsSni (lookupCert config)

lookupCert :: DokanConfig -> Maybe HostName -> IO Credentials
lookupCert _ Nothing = return $ Credentials []
lookupCert (DokanConfig exactMap wildcards) (Just hostname) = do
  let found = case M.lookup (HostExactIndexId hostname) exactMap of
        Just (Route (HostExact (Https cred, _)) _ _) -> Just cred
        Just _ -> Nothing
        Nothing -> findWildcardCert wildcards hostname
  return $ Credentials (maybeToList found)

findWildcardCert :: [Route] -> HostName -> Maybe Credential
findWildcardCert [] _ = Nothing
findWildcardCert ((Route (HostWildcard (Https cred, name)) _ _) : rest) hostname =
  if L.intercalate "." ("*" : drop 1 (splitBy "." hostname)) == name
    then Just cred
    else findWildcardCert rest hostname
findWildcardCert _ _ = Nothing
