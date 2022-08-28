{-# LANGUAGE OverloadedStrings #-}

module Server.Auth.JWT where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Time
import           Crypto.JWT            as Jose
import           Data.Aeson
import qualified Data.ByteString.Lazy  as BS
import           Data.String
import           Data.Text             (Text, unpack)
import           Data.Text.Strict.Lens (utf8)
import           Data.Time

writeJWK :: IO ()
writeJWK = do
  jwk <- genJWK (RSAGenParam 256)
  let
    h = view thumbprint jwk :: Digest SHA256
    kId = view (re (base64url . digest) . utf8) h
    jwk' = set jwkKid (Just kId) jwk
  BS.writeFile "key.json" $ encode jwk'

mkClaims :: Text -> UTCTime -> ClaimsSet
mkClaims userId expiry = emptyClaimsSet
    & claimSub ?~ fromString (unpack userId)
    & claimExp ?~ NumericDate expiry

mintJWT :: (MonadRandom m, MonadError e m, AsError e) => JWK ->  Text -> UTCTime -> m SignedJWT
mintJWT jwk userId expiry = do
  let claims = mkClaims userId expiry
  h <- makeJWSHeader jwk
  signClaims jwk h claims

