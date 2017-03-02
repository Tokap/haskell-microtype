{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Allows for multiple uses of data types
{-# LANGUAGE DeriveGeneric #-}

module StartDataTypes where

import Prelude hiding (id)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

--------------------------------------------------------------------------------
---------------------------- Social Zombie -------------------------------------
--------------------------------------------------------------------------------

data TraversalResponse = TraversalResponse {
  trav_table_id  :: Int,
  net_table_id   :: Int,
  user_id        :: String,
  username       :: String,
  network_code   :: String
} deriving (Show, Generic)
instance ToJSON TraversalResponse
instance FromJSON TraversalResponse

makeTraversalResponse :: Int -> Int -> String -> String -> String -> TraversalResponse
makeTraversalResponse tId nId uId un nc = TraversalResponse {
  trav_table_id  = tId,
  net_table_id   = nId,
  user_id        = uId,
  username       = un,
  network_code   = nc
}


data ResponseWithCallback = ResponseWithCallback {
  success_callback :: String,
  fail_callback    :: String,
  response         :: [TraversalResponse]
} deriving (Show, Generic)
instance ToJSON ResponseWithCallback
instance FromJSON ResponseWithCallback

makeCbResponse :: String -> String -> [TraversalResponse] -> ResponseWithCallback
makeCbResponse succ_callback fail_callback tr = ResponseWithCallback {
  success_callback = succ_callback
, fail_callback = fail_callback
, response = tr
}
