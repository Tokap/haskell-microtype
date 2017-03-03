{-# LANGUAGE OverloadedStrings #-}
module Final where

import StartDataTypes
import StartDb
import MidDataTypes
import Database.MySQL.Simple

import Web.Scotty

main :: IO ()
main = do
  triggerServer



makeServerAddress :: Int -> String
makeServerAddress i = "http://localhost:4000/init/network-account/" ++ (show i)

triggerServer :: IO ()
triggerServer = do
  putStrLn "Starting Server on Port: 5000"
  scotty 5000 $ do
    post "/go/nuts/" $ do
      allPending <- liftAndCatchIO $ getAllTraversals myConnDetails
      let addressList = getIdAndProcess allPending
      json (addressList :: [String])

getIdAndProcess :: [TraversalResponse] -> [String]
getIdAndProcess trList = map (\x ->  makeServerAddress (getNetId x) :: String) trList

-- "destination_url": "http://localhost:3000/network-account"
-- [Confirmation]
