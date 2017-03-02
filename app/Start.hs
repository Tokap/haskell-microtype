{-# LANGUAGE OverloadedStrings #-}
module Start where

import StartDb
import StartServer

main :: IO ()
main = do
  startServer
  -- z <- updateTraversalInProgress myConnDetails 1
  -- print z
  -- z <- getByNetworkId myConnDetails 2
  -- print z
