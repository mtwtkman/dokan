module Dokan.Listener.Http (
  runHttp,
) where

import Dokan.Listener.App (app)
import Dokan.Types (DokanConfig)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)

runHttp :: DokanConfig -> IO ()
runHttp config = do
  manager <- newManager defaultManagerSettings
  run 8080 (app manager config)
