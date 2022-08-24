{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module DB.Entities where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.ByteString
import           Data.Int
import           Data.Text
import           Data.Time           (UTCTime (UTCTime))
import qualified Database.Persist.TH as PTH
import           GHC.Generics        (Generic)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Category sql=categories
    name Text
    parentId CategoryId Maybe
    UniqueName name
    deriving Show Read Eq

  User sql=users
    name Text
    email Text
    password Text
    admin Bool
    UniqueEmail email
    deriving Show Read Eq

  Article sql=articles
    title Text
    content Text
    creator UserId
    category CategoryId
    createdAt UTCTime
|]
