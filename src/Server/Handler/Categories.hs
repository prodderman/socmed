{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Categories where

import           Control.Monad.Trans         (liftIO)
import           Database.Persist.Postgresql (SqlBackend)
import           Servant

import           DB.Query
import           Server.Schema
import           Server.Types

categoriesHandler :: ServerM Categories
categoriesHandler = getCategories :<|> createCategory :<|> updateCategory :<|> deleteCategory
  where
    getCategories = pure "get categories"
    createCategory body = pure body
    updateCategory id body = pure id
    deleteCategory id = pure id
