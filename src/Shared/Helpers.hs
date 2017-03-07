{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared.Helpers where

import Data.Time.Clock.POSIX
import Data.UUID.V4
import Data.UUID

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = do
  z <- round `fmap` getPOSIXTime
  return (z :: Integer)

makeUUID :: IO String
makeUUID = toString <$> nextRandom
