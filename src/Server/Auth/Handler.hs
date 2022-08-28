{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Auth.Handler where

import           Database.Esqueleto.Experimental
import           Network.Wai                      (Request)
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

import           DB.Entities
import           Servant

authHandler :: AuthHandler Request (Entity User)
authHandler = mkAuthHandler handler
  where
    handler req = throwError (err403 { errBody = "Unauthorized" })
