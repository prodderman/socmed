module Server.Handler where

import           Data.Text      (pack)
import           Servant.API
import           Servant.Server

import           Server.Schema

handler :: Server API
handler = getArticles :<|> getArticle
  where
    getArticles createdAt _ _ mAuthor mCategory mTitle mContent mSortBy = pure
      [ pack $ show createdAt
      , pack $ show mAuthor
      , pack $ show mCategory
      , pack $ show mTitle
      , pack $ show mContent
      , pack $ show mSortBy
      ]
    getArticle = undefined
