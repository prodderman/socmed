{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Users where

import           Control.Monad.Trans         (liftIO)
import           Data.Pool
import           Data.Text                   (pack)
import           Database.Persist.Postgresql (SqlBackend)
import           Servant

import           DB.Query
import           Server.Schema

usersHandler :: Pool SqlBackend -> Server Users
usersHandler pool = getUsers
  where
    getUsers = do
      users <- liftIO $ fetchUsers pool
      pure $ pack . show <$> users
