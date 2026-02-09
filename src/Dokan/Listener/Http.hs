module Dokan.Listener.Http (
  runHttp,
) where

import Dokan.Listener.App (app)
import Dokan.Types (RoutingTable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)

runHttp :: RoutingTable -> IO ()
runHttp routing = do
  manager <- newManager defaultManagerSettings
  run 8080 (app False manager routing)
