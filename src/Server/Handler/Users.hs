{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Users where

import           Control.Monad.Reader
import           Control.Monad.Trans         (liftIO)
import           Data.Pool
import           Data.Text                   (pack)
import           Database.Persist.Postgresql (SqlBackend)
import           Servant

import           DB.Query
import           Server.Schema
import           Server.Types

usersHandler :: ServerM Users
usersHandler = getUsers
  where
    getUsers = do
      pool <- asks getPool
      users <- liftIO $ fetchUsers pool
      pure $ pack . show <$> users
