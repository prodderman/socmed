{-# LANGUAGE OverloadedStrings #-}

module Server.Handler where

import           Data.ByteString         (ByteString)
import           Data.Pool
import           Data.Text               (pack)
import           Database.Persist.Sql    (SqlBackend)
import           Servant.API
import           Servant.Server

import           Server.Handler.Articles
import           Server.Handler.Users
import           Server.Schema

server :: Pool SqlBackend -> Server API
server pool =
   articlesHandler pool
   :<|> (getCategories :<|> createCategory :<|> updateCategory :<|> deleteCategory)
   :<|> usersHandler pool
  where
    getCategories = pure "get categories"
    createCategory body = pure body
    updateCategory id body = pure id
    deleteCategory id = pure id

