{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time

import           Server

main = do
  time <- getCurrentTime
  print time
  runServer 8080
