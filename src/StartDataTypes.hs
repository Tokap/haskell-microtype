{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Allows for multiple uses of data types
{-# LANGUAGE DeriveGeneric #-}

module StartDataTypes where

import Prelude hiding (id)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Int (Int64)

--------------------------------------------------------------------------------
---------------------------- Response Data -------------------------------------
--------------------------------------------------------------------------------

data TraversalDetails = TraversalDetails {
  trav_table_id  :: Int
, net_table_id   :: Int
, user_id        :: String
, username       :: String
, network_code   :: String
} deriving (Show, Generic)
instance ToJSON TraversalDetails
instance FromJSON TraversalDetails

makeTraversalDetails :: Int -> Int -> String -> String -> String -> TraversalDetails
makeTraversalDetails tId nId uId un nc = TraversalDetails {
  trav_table_id  = tId
, net_table_id   = nId
, user_id        = uId
, username       = un
, network_code   = nc
}

getNetId :: TraversalDetails -> Int
getNetId tr = net_table_id tr


data ResponseWithCallback = ResponseWithCallback {
  callback  :: String
, uuid      :: String
, response  :: [TraversalDetails]
} deriving (Show, Generic)
instance ToJSON ResponseWithCallback
instance FromJSON ResponseWithCallback

makeCbResponse :: String -> String -> [TraversalDetails] -> ResponseWithCallback
makeCbResponse succ_callback uuid tr = ResponseWithCallback {
  callback = succ_callback
, uuid     = uuid
, response = tr
}

getFirstTraversal :: ResponseWithCallback -> TraversalDetails
getFirstTraversal response' = head (response response')


data FinalResponse = FinalResponse {
  code :: Int64
} deriving (Show, Generic)
instance ToJSON FinalResponse
instance FromJSON FinalResponse

makeFinalResponse :: Int64 -> FinalResponse
makeFinalResponse status = FinalResponse { code = status }
