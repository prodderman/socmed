{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           DB.Entities

main :: IO ()
main = do
  runApp 8080 "host=localhost port=5432 dbname=socmed"
