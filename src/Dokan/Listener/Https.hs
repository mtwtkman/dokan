module Dokan.Listener.Https (
  runHttps,
) where

import Dokan.Listener.App (app)
import Dokan.Types (RoutingTable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

runHttps :: RoutingTable -> IO ()
runHttps routing = do
  let tls =
        tlsSettings
          "certs/cert.pem"
          "certs/key.pem"
      warp = setPort 8443 defaultSettings
  manager <- newManager defaultManagerSettings
  runTLS tls warp (app manager routing)
