{-# LANGUAGE OverloadedStrings #-}

module MidServer where

import Web.Scotty
import qualified Codec.Compression.GZip as GZip
import Data.Aeson (encode, decode)

import StartDb
import MidDataTypes
import StartDataTypes
import Http
import MidDb


deriveZombieUrlAndGet :: TraversalResponse -> IO SzTwitterResponse
deriveZombieUrlAndGet tr =
  getZombie (network_code tr) (username tr) (read $ user_id tr)

startServer :: IO ()
startServer = do
  putStrLn "Starting Server on Port: 4000"

  scotty 4000 $ do
--------------------------------------------------------------------------------
---------------------------------- Post Reqs -----------------------------------
--------------------------------------------------------------------------------
--- Init and Process:
    post "/init/network-account/:id" $ do

      ----- Get Details from Request -------
      nId <- param "id"
      reqDetails <- jsonData :: ActionM RequestDetails
      let finalUrl = (destination_url reqDetails) ++ "/" ++ (show (nId :: Int))
      initResponse <- liftAndCatchIO $ hitGetUrl finalUrl

      ----- Parse Response & Hit Zombie -------
      let accountDetails = getFirstTraversal (initResponse :: ResponseWithCallback)
      szResponse <- liftAndCatchIO $ deriveZombieUrlAndGet accountDetails

      ----- Save to MySQL -------
      let postData = makeNpdObject nId (GZip.compress $ encode szResponse)
      confSave <- liftAndCatchIO $ insertPostDetails myConnDetails postData

      ----- Hit Callback with Outcome -------
      let callback = (success_callback initResponse)
      finalSaveResponse <- liftAndCatchIO $ successCallbackConnection callback

      json (finalSaveResponse :: FinalResponse)



      -- json (szResponse :: SzTwitterResponse)
      -- json (accountDetails :: TraversalResponse)
