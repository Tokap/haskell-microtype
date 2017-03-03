{-# LANGUAGE DuplicateRecordFields #-}

module Http where

import           MidDataTypes
import           StartDataTypes
import           Data.Aeson
import           Network.HTTP.Simple

--------------------------------------------------------------------------------
--------------------------- SZ Connections -------------------------------------
--------------------------------------------------------------------------------
socialZombieBase :: String
socialZombieBase = "GET http://localhost:3199/user/"

makeZombieUrl :: String -> String -> Int -> String
makeZombieUrl network username userId = do
  let myId = show (userId :: Int)
  concat [ socialZombieBase, network, "/id/", myId, "/username/", username, "/timeline" ]

getZombie :: String -> String -> Int -> IO SzTwitterResponse
getZombie network username userId = do
    let requestUrl = makeZombieUrl network username userId
    request <- parseRequest requestUrl
    response <- httpJSON request

    let postOutput = getResponseBody response :: SzTwitterResponse
    return postOutput

--------------------------------------------------------------------------------
------------------- Get & Save Connections -------------------------------------
--------------------------------------------------------------------------------
hitStartUrl :: String -> IO ResponseWithCallback
hitStartUrl url = do
  request <- parseRequest $ "POST " ++ url
  response <- httpJSON request
  return (getResponseBody response :: ResponseWithCallback)

successCallbackConnection :: String -> IO FinalResponse
successCallbackConnection url = do
  request <- parseRequest $ "POST " ++ url
  response <- httpJSON request
  return (getResponseBody response :: FinalResponse)
