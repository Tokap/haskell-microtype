{-# LANGUAGE OverloadedStrings #-}
module Trigger where

import Confirmation.DataTypes
import Confirmation.Db
import DataProcess.DataTypes
import Shared.Http

import Database.MySQL.Simple
import Control.Concurrent.Async
import Web.Scotty

main :: IO ()
main = startServer

--------------------------------------------------------------------------------
-------------------------- Request Helpers -------------------------------------
--------------------------------------------------------------------------------

reqDetails :: RequestDetails
reqDetails = RequestDetails {
  destination_url = "http://localhost:3000/network-account"
}

makeServerAddress :: Int -> String
makeServerAddress i = "http://localhost:4000/init/network-account/" ++ (show i)

--------------------------------------------------------------------------------
--------------------------------- Server ---------------------------------------
--------------------------------------------------------------------------------

startServer :: IO ()
startServer = do
  putStrLn "Starting Server on Port: 5000"
  scotty 5000 $ do
    post "/go/nuts/" $ do
      allPending <- liftAndCatchIO $ getAllTraversals myConnDetails
      let addressList = getIdAndProcess allPending

      confirmed <- liftAndCatchIO $ mapConcurrently (initConnection reqDetails) addressList
      json (confirmed :: [Confirmation])

--------------------------------------------------------------------------------
---------------------------------- Http ---------------------------------------
--------------------------------------------------------------------------------

getIdAndProcess :: [TraversalDetails] -> [String]
getIdAndProcess trList = map (\x ->  makeServerAddress (getNetId x) :: String) trList
