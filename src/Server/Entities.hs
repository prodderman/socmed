
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Entities where

import           Data.Aeson
import           Data.ByteString
import           Data.Text
import qualified Data.Text.Lazy  as LT
import           GHC.Generics    (Generic)
import           Servant         (FromHttpApiData (parseUrlPiece))

data UserCredential = UserCredential Text Text
  deriving (Show)

instance FromJSON UserCredential where
  parseJSON (Object v) = UserCredential <$> v .: "email" <*> v .: "password"
  parseJSON _          = mempty

instance ToJSON UserCredential where
  toJSON (UserCredential email password) = object ["email" .= email, "password" .= password]

newtype AccessToken = AccessToken LT.Text
  deriving (Show)

instance ToJSON AccessToken where
  toJSON (AccessToken t)  = object ["accessToken" .= t]

