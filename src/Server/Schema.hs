{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Schema where

import           Control.Lens
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Swagger
import           Data.Text
import           Data.Time                  (UTCTime)
import           Database.Persist           (Entity)
import           GHC.Generics               (Generic)
import           Servant.API
import           Servant.Swagger

import           DB.Entities                (User)
import qualified Server.Entities            as Entities

data SortBy = Date | Author | Category deriving (Generic, Show)

instance FromHttpApiData SortBy where
  parseUrlPiece "author"   = Right Author
  parseUrlPiece "category" = Right Category
  parseUrlPiece _          = Right Date

instance ToParamSchema SortBy where

type API = DomainAPI

type ApiRoute a = "api" :> a

type DomainAPI =
       ApiRoute Articles
  :<|> ApiRoute Categories
  :<|> ApiRoute Users

type Articles =
       GetArticles
  :<|> GetArticle
  :<|> CreateArticle
  :<|> UpdateArticle
  :<|> DeleteArticle

type GetArticles = "articles"
  :> QueryParam "created_at" UTCTime
  :> QueryParam "created_until" UTCTime
  :> QueryParam "created_since" UTCTime
  :> QueryParam "author" Text
  :> QueryParam "category" Text
  :> QueryParam "title" Text
  :> QueryParam "content" Text
  :> QueryParam "sortBy" SortBy
  :> Get '[JSON] [Text]

type GetArticle = "articles"
  :> Capture "id" Text
  :> Get '[JSON] Text

type CreateArticle = "articles"
  :> ReqBody '[JSON] Text
  :> Post '[JSON] Text

type UpdateArticle = "articles"
  :> Capture "id" Text
  :> ReqBody '[JSON] Text
  :> Put '[JSON] Text

type DeleteArticle = "articles"
  :> Capture "id" Text
  :> Delete '[JSON] Text


type Categories =
       GetCategories
  :<|> CreateCategory
  :<|> UpdateCategory
  :<|> DeleteCategory

type GetCategories = "categories"
  :> Get '[JSON] Text

type CreateCategory = "categories"
  :> ReqBody '[JSON] Text
  :> Post '[JSON] Text

type UpdateCategory = "categories"
  :> Capture "id" Text
  :> ReqBody '[JSON] Text
  :> Put '[JSON] Text

type DeleteCategory = "categories"
  :> Capture "id" Text
  :> Delete '[JSON] Text


type Users = GetUsers

type GetUsers = "users"
  :> Get '[JSON] [Text]

type SwaggerAPI = "swagger" :> "swagger.json" :> Get '[JSON] Swagger

api :: Proxy API
api = Proxy

-- mkSwagger :: Swagger
-- mkSwagger = toSwagger (Proxy :: Proxy DomainAPI)
--   & info.title   .~ "Todo API"
--   & info.version .~ "1.0"
--   & info.description ?~ "This is an API that tests swagger integration"

-- writeSwaggerJSON :: IO ()
-- writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty mkSwagger)
