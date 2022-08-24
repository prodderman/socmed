{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.ByteString             (ByteString)
import           Data.Pool
import           Database.Persist.Postgresql (ConnectionString, PersistStoreWrite (insert),
                                              SqlBackend, runMigration, runSqlPersistMPool,
                                              withPostgresqlPool)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (Port, run)
import           Servant.Server              (serve)
import           Server.Handler              (server)

import           DB.Entities
import           DB.Utils                    (cleanDB, setupDB)
import           Server.Schema               (api)

app :: Pool SqlBackend  -> Application
app pool = serve api (server pool)

runApp :: Port -> ConnectionString -> IO ()
runApp port dcs = runStderrLoggingT $
  withPostgresqlPool dcs 10 $
    \pool -> liftIO $ do
      runSqlPersistMPool cleanDB pool
      runSqlPersistMPool setupDB pool
      run port (app pool)
