module Server.Handler.Articles where

import           Data.Text
import           Servant

import           DB.Query
import           Server.Schema
import           Server.Types

articlesHandler :: ServerM Articles
articlesHandler = getArticles :<|> getArticle :<|> createArticle :<|> updateArticle :<|> deleteArticle
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

