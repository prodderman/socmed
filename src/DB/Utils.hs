{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Utils where

import           Control.Monad               (void)
import           Control.Monad.Logger        (MonadLogger)
import           Control.Monad.Trans         (MonadIO)
import           Data.ByteString             (ByteString)
import           Database.Persist.Postgresql (PersistStoreWrite (insert), SqlPersistT, rawExecute,
                                              runMigration)

import           DB.Entities

cleanDB :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
cleanDB = do
  dropTable "articles"
  dropTable "users"
  dropTable "categories"
  where
    dropTable tableName = rawExecute ("DROP TABLE " <> tableName) []

setupDB :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
setupDB = do
  runMigration migrateAll
  void $ insert $ User "admin1" "admin@one.io" "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92" True
  void $ insert $ User "admin2" "admin@two.io" "65e84be33532fb784c48129675f9eff3a682b27168c0ea744b2cf58ee02337c5" True
  void $ insert $ User "admin3" "admin@three.io" "8588310a98676af6e22563c1559e1ae20f85950792bdcd0c8f334867c54581cd" True
  mainCategory <- insert $ Category "Programming Languages" Nothing
  void $ insert $ Category "Functional Programming Languages" (Just mainCategory)
  void $ insert $ Category "Object-Oriented Programming Languages" (Just mainCategory)
