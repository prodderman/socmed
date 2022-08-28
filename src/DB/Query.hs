{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module DB.Query where

import           Control.Monad.Reader
import           Data.Pool                       (Pool)
import           Database.Esqueleto.Experimental
import           Database.Persist.Postgresql     (SqlBackend)

import           Control.Monad.Logger            (MonadLogger, NoLoggingT)
import           DB.Entities

fetchUsers :: (MonadIO m) => Pool SqlBackend -> m [Entity User]
fetchUsers pool =
  let query = select $ from $ table @User
  in liftIO $ runQuery pool query

findUserBy :: (MonadIO m, PersistField typ) => EntityField User typ -> typ -> SqlReadT m (Maybe (Entity User))
findUserBy field value = selectOne $ do
    user <- from $ table @User
    where_ (user ^. field ==. val value)
    pure user

getArticles :: (MonadIO m, MonadLogger m) => Pool SqlBackend -> m [(Entity Article, Maybe (Entity Category))]
getArticles pool =
  let query = select $ do
        (article :& category) <- from $ table @Article `leftJoin` table @Category
          `on` (\(article :& category) -> just (article ^. ArticleCategory) ==. category ?. CategoryId)
        pure (article, category)
  in liftIO $ runQuery pool query

runQuery pool query =  runSqlPersistMPool query pool
