{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Entities where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.ByteString
import           Data.Text
import           GHC.Generics    (Generic)
import           Servant         (FromHttpApiData (parseUrlPiece))

data User = User
  { uName         :: Text
  , uLogin        :: Text
  , uPasswordHash :: Text
  , uCreated      :: Text
  , uAdmin        :: Bool
  , uCanCreate    :: Bool
  }
  deriving (FromJSON, Generic, ToJSON)

data Category = Root Text
              | SubCategory Category Text
  deriving (FromJSON, Generic, Show, ToJSON)

data Article = Article
  { pTitle     :: Text
  , pCreated   :: Text
  , pCreator   :: User
  , pCategory  :: Category
  , pContent   :: Text
  , pMedia     :: [Text]
  , pPublished :: Bool
  }
  deriving (FromJSON, Generic, ToJSON)

