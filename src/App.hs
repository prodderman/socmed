{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Logger             (runStderrLoggingT)
import           Data.ByteString                  (ByteString)
import           Data.Pool
import           Database.Esqueleto.Experimental  (Entity)
import           Database.Persist.Postgresql      (ConnectionString, PersistStoreWrite (insert),
                                                   SqlBackend, runMigration, runSqlPersistMPool,
                                                   withPostgresqlPool)
import           Network.Wai                      (Application, Request)
import           Network.Wai.Handler.Warp         (Port, run)
import           Servant.Server                   (Context (EmptyContext, (:.)), Handler,
                                                   HasServer (ServerT, hoistServerWithContext),
                                                   hoistServer, serve, serveWithContext)
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Server.Handler                   (server)

import           Control.Monad.Reader
import           Crypto.JOSE                      (JWK)
import           Data.Aeson                       (decode)
import qualified Data.ByteString.Lazy             as BS
import           Data.Proxy                       (Proxy (Proxy))
import           DB.Entities
import           DB.Utils                         (cleanDB, setupDB)
import           Server.Auth.Handler              (authHandler)
import           Server.Schema                    (API, api)
import           Server.Types

context :: Context (AuthHandler Request (Entity User) ': '[])
context = authHandler :. EmptyContext

app :: Pool SqlBackend -> JWK -> Application
app pool jwk =
  let env = Env pool jwk
      contextProxy = Proxy :: Proxy (AuthHandler Request (Entity User) ': '[])
      convert r = runReaderT r env
      server' = hoistServerWithContext api contextProxy convert server
  in serveWithContext api context server'

runApp :: Port -> ConnectionString -> IO ()
runApp port dcs = do
  Just jwk <- decode <$> BS.readFile "key.json"
  runStderrLoggingT $
    withPostgresqlPool dcs 10 $ \pool -> liftIO $ run port (app pool jwk)
