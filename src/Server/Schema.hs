{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Schema where


import           Control.Monad.Reader
import           Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Data.Pool                        (Pool)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Swagger
import           Data.Text
import           Data.Time                        (UTCTime)
import           Database.Persist                 (Entity)
import           Database.Persist.Postgresql      (SqlBackend)
import           GHC.Generics                     (Generic)
import           Servant.API
import           Servant.Server
import           Servant.Server.Experimental.Auth (AuthServerData)
import           Servant.Swagger

import           DB.Entities                      (User)
import qualified Server.Entities                  as Entities

data SortBy = Date | Author | Category deriving (Generic, Show)

instance FromHttpApiData SortBy where
  parseUrlPiece "author"   = Right Author
  parseUrlPiece "category" = Right Category
  parseUrlPiece _          = Right Date

instance ToParamSchema SortBy

type API = DomainAPI

type Protected = AuthProtect "auth"
type instance AuthServerData Protected = Entity User

type DomainAPI = "api" :> (Auth :<|> Articles :<|> Categories :<|> Users)

type Auth =
  "auth"
  :> ( Login
  :<|> Refresh
  :<|> Logout )

type Login =
  "login"
  :> ReqBody '[JSON] Entities.UserCredential
  :> Post '[JSON] Entities.AccessToken

type Refresh =
 "refresh"
  :> ReqBody '[JSON] Text
  :> Post '[JSON] Text

type Logout =
  Protected
  :> "logout"
  :> ReqBody '[JSON] Text
  :> Post '[JSON] Text

type Articles =
  "articles"
  :> ( GetArticles
  :<|> GetArticle
  :<|> CreateArticle
  :<|> UpdateArticle
  :<|> DeleteArticle )

type GetArticles =
     QueryParam "created_at" UTCTime
  :> QueryParam "created_until" UTCTime
  :> QueryParam "created_since" UTCTime
  :> QueryParam "author" Text
  :> QueryParam "category" Text
  :> QueryParam "title" Text
  :> QueryParam "content" Text
  :> QueryParam "sortBy" SortBy
  :> Get '[JSON] [Text]

type GetArticle =
  Capture "id" Text
  :> Get '[JSON] Text

type CreateArticle =
  ReqBody '[JSON] Text
  :> Post '[JSON] Text

type UpdateArticle =
  Capture "id" Text
  :> ReqBody '[JSON] Text
  :> Put '[JSON] Text

type DeleteArticle =
  Capture "id" Text
  :> Delete '[JSON] Text


type Categories =
  "categories"
  :> ( GetCategories
  :<|> CreateCategory
  :<|> UpdateCategory
  :<|> DeleteCategory )

type GetCategories =
  Get '[JSON] Text

type CreateCategory =
  ReqBody '[JSON] Text
  :> Post '[JSON] Text

type UpdateCategory =
  Capture "id" Text
  :> ReqBody '[JSON] Text
  :> Put '[JSON] Text

type DeleteCategory =
  Capture "id" Text
  :> Delete '[JSON] Text


type Users = "users" :> GetUsers

type GetUsers = Get '[JSON] [Text]

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
