{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Confirmation.Db where

import Control.Monad
import Database.MySQL.Simple
import Data.Word (Word16)
import Data.Int (Int64)

import Confirmation.DataTypes
--------------------------------------------------------------------------------
-------------------------- CONNECTION POOL -------------------------------------
--------------------------------------------------------------------------------

data ConnectionDetails = ConnectionDetails {
  host :: String
, port :: Word16
, user :: String
, pass :: String
, db   :: String
}


makeConnection :: ConnectionDetails -> IO Connection
makeConnection connDetails = do
  let connectionInfo = defaultConnectInfo {
                      connectHost     = (host connDetails)
                    , connectPort     = (port connDetails)
                    , connectUser     = (user connDetails)
                    , connectPassword = (pass connDetails)
                    , connectDatabase = (db connDetails)
                    }
  conn <- connect connectionInfo
  return conn

myConnDetails :: ConnectionDetails
myConnDetails = ConnectionDetails {
  host = "127.0.0.1"
, port = 3306
, user = "root"
, pass = ""
, db   = "ip_hoarder"
}

--------------------------------------------------------------------------------
----------------------- Traversal Queries --------------------------------------
--------------------------------------------------------------------------------

-- QUERIES:
allPendingQuery :: Query
allPendingQuery = "SELECT \
    	\T.id AS trav_table_id, \
    	\NA.id AS net_table_id, \
    	\NA.user_id, \
    	\NA.username, \
    	\NT.code AS network_code \
    \FROM traversal AS T \
    \LEFT JOIN network_account AS NA \
    \ON T.network_account_id=NA.id \
    \LEFT JOIN network_type AS NT \
    \ON NA.network_type_id=NT.id \
    \WHERE `history_completed` IS NULL \
    \AND `processor_uuid` IS NULL;"

getTraversalByNetIdQuery :: Query
getTraversalByNetIdQuery = "SELECT \
    	\T.id AS trav_table_id, \
    	\NA.id AS net_table_id, \
    	\NA.user_id, \
    	\NA.username, \
    	\NT.code AS network_code \
    \FROM traversal AS T \
    \LEFT JOIN network_account AS NA \
    \ON T.network_account_id=NA.id \
    \LEFT JOIN network_type AS NT \
    \ON NA.network_type_id=NT.id \
    \WHERE `history_completed` IS NULL \
    \AND `processor_uuid` IS NULL \
    \AND NA.id=?"

initialTraversalUpdate :: Query
initialTraversalUpdate = "UPDATE traversal \
          \SET traversal_status_id=3,\
          \processor_uuid=? \
          \WHERE network_account_id=?;"

finishTraversalUpdate :: Query
finishTraversalUpdate = "UPDATE traversal \
          \SET traversal_status_id=1,\
          \history_completed=1, \
          \last_updated=? \
          \WHERE network_account_id=?;"

--------------------------------------------------------------------------------
----------------------- Traversal Functions ------------------------------------
--------------------------------------------------------------------------------

--- READ:

getAllTraversals :: ConnectionDetails -> IO [TraversalDetails]
getAllTraversals connDetails = do
  conn <- makeConnection connDetails
  xs <- query_ conn allPendingQuery

  return $ map ( \(tId, nId, uId, un, nc) -> makeTraversalDetails tId nId uId un nc) xs

getByNetworkId :: ConnectionDetails -> Int -> IO [TraversalDetails]
getByNetworkId connDetails i = do
  let myId = show i
  conn <- makeConnection connDetails
  xs <- query conn getTraversalByNetIdQuery (Only myId)

  return $ map ( \(tId, nId, uId, un, nc) -> makeTraversalDetails tId nId uId un nc) xs

--- UPDATE:

--- Used For Initial Update & to add UUID
setTraversalInProgress :: ConnectionDetails -> String -> Int -> IO Int64
setTraversalInProgress connDetails uuid nId = do
  conn <- makeConnection connDetails
  execute conn initialTraversalUpdate (uuid :: String, nId :: Int) :: IO Int64

--- Used Upon Completion - Updates Status
setTraversalComplete :: ConnectionDetails -> Int -> Integer -> IO Int64
setTraversalComplete connDetails nId unixStamp = do
  conn <- makeConnection connDetails
  execute conn finishTraversalUpdate (unixStamp :: Integer, nId :: Int)
