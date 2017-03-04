{-# LANGUAGE DuplicateRecordFields #-}

module Shared.Http where

import           Data.Aeson
import           Network.HTTP.Simple
import qualified Data.ByteString as BS

import           Confirmation.DataTypes
import           DataProcess.DataTypes

--------------------------------------------------------------------------------
--------------------------- SZ Connections -------------------------------------
--------------------------------------------------------------------------------
socialZombieBase :: String
socialZombieBase = "GET http://localhost:3199/user/"

makeZombieUrl :: String -> String -> Int -> String
makeZombieUrl network username userId = do
  let myId = show (userId :: Int)
  concat [ socialZombieBase, network, "/id/", myId, "/username/", username, "/timeline/" ]

getZombie :: String -> String -> Int -> IO SzTwitterResponse
getZombie network username userId = do
    let requestUrl = makeZombieUrl network username userId
    getZombieByUrl requestUrl

getZombieByUrl :: String -> IO SzTwitterResponse
getZombieByUrl requestUrl = do
    request <- parseRequest requestUrl
    response <- httpJSON request

    let postOutput = getResponseBody response :: SzTwitterResponse
    return postOutput

--------------------------------------------------------------------------------
------------------- Get & Save Connections -------------------------------------
--------------------------------------------------------------------------------
getTraversalDetails :: String -> IO ResponseWithCallback
getTraversalDetails url = do
  request <- parseRequest $ "POST " ++ url
  response <- httpJSON request
  return (getResponseBody response :: ResponseWithCallback)

successCallbackConnection :: String -> IO FinalResponse
successCallbackConnection url = do
  request <- parseRequest $ "POST " ++ url
  response <- httpJSON request
  return (getResponseBody response :: FinalResponse)

--------------------------------------------------------------------------------
------------------- Initialization Transaction ---------------------------------
--------------------------------------------------------------------------------
initConnection :: RequestDetails -> String -> IO Confirmation
initConnection requestDetails url = do
  initialRequest <- parseRequest ("POST " ++ url)
  let request
        = setRequestBodyLBS (encode requestDetails)
        $ initialRequest

  response <- httpJSON request
  return (getResponseBody response :: Confirmation)
