{-# LANGUAGE OverloadedStrings #-}

module Server.Handler where

import           Data.ByteString           (ByteString)
import           Database.Persist.Sql      (SqlBackend)
import           Servant.API
import           Servant.Server

import           Server.Handler.Articles
import           Server.Handler.Auth
import           Server.Handler.Categories
import           Server.Handler.Users
import           Server.Schema
import           Server.Types

server :: ServerM API
server  =
   authHandler
   :<|> articlesHandler
   :<|> categoriesHandler
   :<|> usersHandler

