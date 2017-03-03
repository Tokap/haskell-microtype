{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mid where

import StartDataTypes
import MidDataTypes
import Http
import MidServer
import Helpers

import Data.UnixTime

import Data.Time.Clock.POSIX

main :: IO ()
main = do
  midServer
