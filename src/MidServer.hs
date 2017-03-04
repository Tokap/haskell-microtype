{-# LANGUAGE OverloadedStrings #-}

module MidServer where

import Control.Concurrent.Async
import Data.Maybe
import Web.Scotty
import Data.Aeson (encode, decode)
import qualified Codec.Compression.GZip as GZip

import StartDb
import MidDataTypes
import StartDataTypes
import Http
import MidDb

-- Small Helper FN
deriveZombieUrl :: TraversalDetails -> String
deriveZombieUrl tr =
  makeZombieUrl (network_code tr) (username tr) (read $ user_id tr)

nextPageReview' :: Int -> String -> [SzTwitterResponse] -> IO [SzTwitterResponse]
nextPageReview' nId baseUrl szResponseList = do
  let szResponse = head szResponseList

  if (hasNextPage szResponse)
    then do
      let newUrl = baseUrl ++ (fromJust (getEndCursor szResponse)) -- This should instead account for Maybe
      newResponse <- getZombieByUrl newUrl
      nextPageReview' nId baseUrl (newResponse:szResponseList)
    else do
      return szResponseList

-- Start Server
midServer :: IO ()
midServer = do
  putStrLn "Starting Server on Port: 4000"

  scotty 4000 $ do
--------------------------------------------------------------------------------
----------------------- Primary Process Reqs -----------------------------------
--------------------------------------------------------------------------------
    post "/init/network-account/:id" $ do

      ----- Get Details from Request -------
      nId <- param "id"
      reqDetails <- jsonData :: ActionM RequestDetails
      let finalUrl = (destination_url reqDetails) ++ "/" ++ (show (nId :: Int))
      initResponse <- liftAndCatchIO $ getTraversalDetails finalUrl

      ----- Parse Response & Hit Zombie -------
      let accountDetails = getFirstTraversal (initResponse :: ResponseWithCallback)
      let zombieUrl = deriveZombieUrl accountDetails
      szResponse <- liftAndCatchIO $ getZombieByUrl zombieUrl

      ----- Parse Response & Hit Zombie Repeatedly -------
      mySzResultList <- liftAndCatchIO $ nextPageReview' nId zombieUrl [szResponse]
      let postLength = getNodeLengthSum mySzResultList

      -- ----- Save to MySQL -------
      let postData = makeNpdObject nId (GZip.compress $ encode mySzResultList)
      confSave <- liftAndCatchIO $ insertPostDetails myConnDetails postData

      -- ----- Hit Callback with Outcome -------
      let myCallback = (callback initResponse)
      saveResponse <- liftAndCatchIO $ successCallbackConnection myCallback
      --
      -- ----- Make & Return Confirmation (status & uuid) -------
      let closingObj = makeConfirmation (code saveResponse) (uuid initResponse) (postLength)
      json (closingObj :: Confirmation)
