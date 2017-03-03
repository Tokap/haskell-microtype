{-# LANGUAGE OverloadedStrings #-}
module Start where

import StartDb
import StartServer

main :: IO ()
main = do
  startServer
