{-# LANGUAGE OverloadedStrings #-}
module Final where

import StartDataTypes
import StartDb
import MidDataTypes
import Database.MySQL.Simple
import Http

-- import Network.HTTP.Client.TLS

import Web.Scotty

main :: IO ()
main = do
  triggerServer

reqDetails :: RequestDetails
reqDetails = RequestDetails {
  destination_url = "http://localhost:3000/network-account"
}

makeServerAddress :: Int -> String
makeServerAddress i = "http://localhost:4000/init/network-account/" ++ (show i)

triggerServer :: IO ()
triggerServer = do
  putStrLn "Starting Server on Port: 5000"
  scotty 5000 $ do
    post "/go/nuts/" $ do
      allPending <- liftAndCatchIO $ getAllTraversals myConnDetails
      let addressList = getIdAndProcess allPending

      confirmed <- liftAndCatchIO $ mapM (initConnection reqDetails) addressList
      json (confirmed :: [Confirmation])


getIdAndProcess :: [TraversalResponse] -> [String]
getIdAndProcess trList = map (\x ->  makeServerAddress (getNetId x) :: String) trList

-- [Confirmation]
