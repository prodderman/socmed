{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Utils where

import           Control.Lens
import           Control.Monad               (void)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Logger        (MonadLogger)
import           Control.Monad.Time
import           Control.Monad.Trans         (MonadIO)
import           Crypto.Hash.Algorithms
import           Crypto.JOSE                 (Digest, Error, KeyMaterialGenParam (RSAGenParam),
                                              MonadRandom (getRandomBytes), base64url, digest,
                                              encodeCompact, genJWK, jwkKid, makeJWSHeader,
                                              thumbprint)
import           Crypto.JWT                  (Audience (Audience), ClaimsSet,
                                              NumericDate (NumericDate), claimAud, claimExp,
                                              claimIat, claimIss, claimSub, emptyClaimsSet,
                                              signClaims)
import           Crypto.KDF.PBKDF2
import           Data.Aeson
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.ByteString             (ByteString)
import           Data.Text
import           Database.Persist.Postgresql (PersistStoreWrite (insert), SqlPersistT, rawExecute,
                                              runMigration)

import           Data.Time
import           DB.Entities

hashPassword :: ByteString -> ByteString
hashPassword pwd = fastPBKDF2_SHA256 params pwd salt
  where
    salt :: ByteString
    salt = "V(P6YnAdya2tptHhWq"

    params :: Parameters
    params = Parameters 1000 64

cleanDB :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
cleanDB = do
  dropTable "articles"
  dropTable "users"
  dropTable "categories"
  dropTable "tokens"
  where
    dropTable tableName = rawExecute ("DROP TABLE IF EXISTS " <> tableName) []

setupDB :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
setupDB = do
  runMigration migrateAll
  let admin1PwdHash = hashPassword "123456"
      admin2PwdHash = hashPassword "qwerty"
      admin3PwdHash = hashPassword "asdfgh"
  void $ insert $ User "admin1" "admin@one.io" admin1PwdHash True 1
  void $ insert $ User "admin2" "admin@two.io" admin2PwdHash True 1
  void $ insert $ User "admin3" "admin@three.io" admin3PwdHash True 1
  mainCategory <- insert $ Category "Programming Languages" Nothing
  void $ insert $ Category "Functional Programming Languages" (Just mainCategory)
  void $ insert $ Category "Object-Oriented Programming Languages" (Just mainCategory)
