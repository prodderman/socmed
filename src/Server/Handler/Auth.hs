{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Server.Handler.Auth where

import           Control.Lens
import           Control.Monad.Time          (currentTime)
import           Control.Monad.Trans         (liftIO)
import           Data.Pool
import           Data.Text                   (pack)
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Database.Persist.Postgresql (Entity (entityKey, entityVal), SqlBackend, fromSqlKey)
import           Servant

import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Reader        (asks)
import           Crypto.JOSE                 (Error, encodeCompact)
import           Data.Text.Encoding          (encodeUtf8)
import           DB.Entities
import           DB.Query
import           DB.Utils
import           Server.Auth.JWT             (mintJWT)
import           Server.Entities
import           Server.Schema
import           Server.Types

authHandler :: ServerM Auth
authHandler = login :<|> refresh :<|> logout
  where
    login (UserCredential email password) = do
      pool <- asks getPool
      muser <- liftIO $ runQuery pool (findUserBy UserEmail email)
      case muser of
        Nothing   -> throwError error
        Just user -> if comparePasswords password (userPassword $ entityVal user)
          then do
            jwk <- asks getJwk
            now <- liftIO currentTime -- TODO: set expiry
            let userId = pack . show . fromSqlKey . entityKey $ user
            token <- liftIO $ runExceptT @Error $ mintJWT jwk userId now
            case token of
              Left e    -> throwError err500
              Right jwt -> pure $ AccessToken $ decodeUtf8 $ encodeCompact jwt
          else throwError error
      where
        error = err403 { errBody = "Invalid email or password" }
        comparePasswords resPwd userPwd = hashPassword (encodeUtf8 resPwd) == userPwd

    refresh _ = pure ""
    logout _ _ = pure ""
