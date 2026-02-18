module Dokan.Listener.Https (
  runHttps,
) where

import qualified Data.List as L
import qualified Data.Map as M
import Dokan.Listener.App (app)
import Dokan.Types (
  DokanConfig (DokanConfig),
  HostExactIndexId (HostExactIndexId),
  HostPattern (HostExact, HostWildcard),
  HostScheme (Https),
  Route (Route),
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.TLS (Credential, Credentials (Credentials), HostName)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettingsSni)

type CredentialMap = M.Map HostName Credential

runHttps :: DokanConfig -> IO ()
runHttps config = do
  let warp = setPort 8443 defaultSettings
  tls <- makeTlsSettings config
  manager <- newManager defaultManagerSettings
  runTLS tls warp (app manager config)

makeTlsSettings :: DokanConfig -> IO TLSSettings
makeTlsSettings config = return $ tlsSettingsSni (lookupCert config)

lookupCert :: DokanConfig -> Maybe HostName -> IO Credentials
lookupCert _ Nothing = return $ Credentials []
lookupCert (DokanConfig exactMap wildcards) (Just hostname) = do
  case M.lookup (HostExactIndexId hostname) exactMap of
    Just (Route (HostExact (Https cred, _)) _ _) -> return $ Credentials [cred]
    Just _ -> return $ Credentials []
    Nothing -> return $ findWildcardCert wildcards hostname

findWildcardCert :: [Route] -> HostName -> Credentials
findWildcardCert [] _ = Credentials []
findWildcardCert ((Route (HostWildcard (Https cred, name)) _ _) : rest) hostname =
  let domain = "*." <> drop 2 hostname
   in if domain == name then Credentials [cred] else findWildcardCert rest hostname
findWildcardCert _ _ = Credentials []
