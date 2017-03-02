{-# LANGUAGE OverloadedStrings #-}

module StartServer where

import Web.Scotty

import StartDb
import StartDataTypes


successBase :: String
successBase = "http://localhost:3000/network-account/save/results/"

failBase :: String
failBase = "http://localhost:3000/network-account/fail/results/"


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
      let failUrl = failBase ++ (show pId)

      traversalDetails <- liftAndCatchIO $ getByNetworkId myConnDetails pId :: ActionM [TraversalResponse]
      confSave <- liftAndCatchIO $ updateTraversalInProgress myConnDetails pId

      json (makeCbResponse succUrl failUrl traversalDetails)

--- Callback Endpoints:
    post "/network-account/save/results/:id/" $ do
      pId <- param "id"
      confSave <- liftAndCatchIO $ updateTraversalSuccess myConnDetails pId

      json confSave  -- Returns Rows Affected Currently

    post "/network-account/fail/results/:id/" $ do
      pId <- param "id"
      confSave <- liftAndCatchIO $ updateTraversalFail myConnDetails pId

      json confSave -- Returns Rows Affected Currently
