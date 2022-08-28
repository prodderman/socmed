module Server.Types where

import           Control.Monad.Reader        (ReaderT)
import           Crypto.JOSE                 (JWK)
import           Data.Pool                   (Pool)
import           Database.Persist.Postgresql (SqlBackend)
import           Servant.Server              (Handler, HasServer (ServerT))

data Env = Env
  { getPool :: Pool SqlBackend
  , getJwk  :: JWK
  }

type ReaderHandler = ReaderT Env Handler

type ServerM api = ServerT api ReaderHandler
