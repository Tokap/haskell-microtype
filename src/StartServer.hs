{-# LANGUAGE OverloadedStrings #-}

module StartServer where

import Web.Scotty
import Data.Int (Int64)


import StartDb
import StartDataTypes
import Helpers (makeUUID, getCurrentUnixTime)


successBase :: String
successBase = "http://localhost:3000/network-account/save/results/"

startServer :: IO ()
startServer = do
  putStrLn "Starting Server on Port: 3000"

  scotty 3000 $ do
--------------------------------------------------------------------------------
---------------------------------- Post Reqs -----------------------------------
--------------------------------------------------------------------------------

--- Core Endpoint:
    post "/network-account/:id/" $ do
      pId <- param "id"
      let succUrl = successBase ++ (show pId)
      genUuid <- liftAndCatchIO $ makeUUID :: ActionM String

      traversalDetails <- liftAndCatchIO $ getByNetworkId myConnDetails pId :: ActionM [TraversalResponse]
      confSave <- liftAndCatchIO $ setTraversalInProgress myConnDetails genUuid pId

      json (makeCbResponse succUrl genUuid traversalDetails)

--- Callback Endpoint:
    post "/network-account/save/results/:id/" $ do
      pId <- param "id"
      unixTime <- liftAndCatchIO $ getCurrentUnixTime :: ActionM Integer
      confSave <- liftAndCatchIO $ setTraversalComplete myConnDetails pId unixTime
      let finalResponse = (makeFinalResponse (confSave :: Int64)) :: FinalResponse

      json finalResponse
