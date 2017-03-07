{-# LANGUAGE OverloadedStrings #-}
module Trigger where

import Database.MySQL.Simple
import Control.Concurrent.Async
import Web.Scotty

import Confirmation.DataTypes
import Confirmation.Db
import DataProcess.DataTypes
import Shared.Http

import Unpack.Db

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

    post "/unpack/network-account/:id/" $ do
      nId <- param "id"
      postDetailsList <- liftAndCatchIO $ getPostPagesById myConnDetails nId
      json (postDetailsList :: [[PostDetails]])


--------------------------------------------------------------------------------
---------------------------------- Http ---------------------------------------
--------------------------------------------------------------------------------

getIdAndProcess :: [TraversalDetails] -> [String]
getIdAndProcess trList = map (\x ->  makeServerAddress (getNetId x) :: String) trList
