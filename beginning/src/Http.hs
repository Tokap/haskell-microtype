{-# LANGUAGE DuplicateRecordFields #-}

module Http where

import           MidDataTypes
import           Data.Aeson
import           Network.HTTP.Simple

--------------------------------------------------------------------------------
------------------------------------ POSTS -------------------------------------
--------------------------------------------------------------------------------
socialZombieBase :: String
socialZombieBase = "GET http://localhost:3199/user/"

makeZombieUrl :: String -> String -> Int -> String
makeZombieUrl network username userId = do
  let myId = show (userId :: Int)
  concat [ socialZombieBase, network, "/id/", myId, "/username/", username, "/timeline" ]


getTestZombie :: IO SzTwitterResponse
getTestZombie = do
    request <- parseRequest "GET http://localhost:3199/user/twitter/id/613/username/jerry/timeline"
    response <- httpJSON request

    let szOutput = getResponseBody response :: SzTwitterResponse
    return szOutput

getZombie :: String -> String -> Int -> IO SzTwitterResponse
getZombie network username userId = do
    let requestUrl = makeZombieUrl network username userId
    request <- parseRequest requestUrl
    response <- httpJSON request

    let postOutput = getResponseBody response :: SzTwitterResponse
    return postOutput
