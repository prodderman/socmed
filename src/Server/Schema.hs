{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Schema where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Text
import           Data.Time       (UTCTime)
import           Servant.API

import qualified Server.Entities as Entities

data SortBy = Date | Author | Category deriving (Show)

instance FromHttpApiData SortBy where
  parseUrlPiece "author"   = Right Author
  parseUrlPiece "date"     = Right Date
  parseUrlPiece "Category" = Right Category
  parseUrlPiece _          = Left "wrong parameter: sortBy"

type API =
         "api"
      :> "articles"
      :> QueryParam "created_at" UTCTime
      :> QueryParam "created_until" UTCTime
      :> QueryParam "created_since" UTCTime
      :> QueryParam "author" Text
      :> QueryParam "category" Text
      :> QueryParam "title" Text
      :> QueryParam "content" Text
      :> QueryParam "sortBy" SortBy
      :> Get '[JSON] [Text]
  :<|>
         "api"
      :> "articles"
      :> Capture "id" Text
      :> Get '[JSON] Text

api :: Proxy API
api = Proxy
