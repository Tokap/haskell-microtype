{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mid where

import StartDataTypes
import MidDataTypes
import Http
import MidServer

main :: IO ()
main = do
  startServer
