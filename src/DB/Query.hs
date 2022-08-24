{-# LANGUAGE FlexibleContexts    #-}
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

type QueryT m a = SqlReadT m a

fetchUsers :: (MonadIO m) => Pool SqlBackend -> m [Entity User]
fetchUsers pool =
  let query = select $ from $ table @User
  in liftIO $ runQuery pool query

getArticles :: (MonadIO m, MonadLogger m) => Pool SqlBackend -> m [(Entity Article, Maybe (Entity Category))]
getArticles pool =
  let query = select $ do
        (article :& category) <- from $ table @Article `leftJoin` table @Category
          `on` (\(article :& category) -> just (article ^. ArticleCategory) ==. category ?. CategoryId)
        pure (article, category)
  in liftIO $ runQuery pool query


-- runQuery :: (MonadIO m, MonadLogger m) => Pool SqlBackend -> QueryT m a -> IO a
runQuery pool query =  runSqlPersistMPool query pool
