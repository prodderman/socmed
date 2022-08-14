module Server where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, run)
import           Servant.Server           (serve)

import           Server.Handler           (handler)
import           Server.Schema            (api)

app :: Application
app = serve api handler

runServer :: Port -> IO ()
runServer port = run port app
