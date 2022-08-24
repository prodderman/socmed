module Server.Handler.Articles where

import           Data.Pool
import           Data.Text
import           Database.Persist.Postgresql (SqlBackend)
import           Servant

import           DB.Query
import           Server.Schema

articlesHandler :: Pool SqlBackend -> Server Articles
articlesHandler pool = getArticles :<|> getArticle :<|> createArticle :<|> updateArticle :<|> deleteArticle
  where
    getArticles createdAt _ _ mAuthor mCategory mTitle mContent mSortBy = pure
      [ pack $ show createdAt
      , pack $ show mAuthor
      , pack $ show mCategory
      , pack $ show mTitle
      , pack $ show mContent
      , pack $ show mSortBy
      ]
    getArticle id = pure id
    createArticle body = pure body
    updateArticle id _ = pure id
    deleteArticle id = pure id

