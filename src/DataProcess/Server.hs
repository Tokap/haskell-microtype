{-# LANGUAGE OverloadedStrings #-}

module DataProcess.Server where

import Control.Concurrent.Async
import Data.Maybe
import Web.Scotty
import Data.Aeson (encode, decode)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.Async


import Confirmation.DataTypes
import Confirmation.Db
import DataProcess.DataTypes
import DataProcess.Db
import Shared.Http

-- Small Helper FNs
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

nextPageReview :: Int -> String -> [SzTwitterResponse] -> IO [[PostDetails]]
nextPageReview nId baseUrl szResponseList = do
  let szResponse = head szResponseList

  if (hasNextPage szResponse)
    then do
      let newUrl = baseUrl ++ (fromJust (getEndCursor szResponse)) -- This should instead account for Maybe
      newResponse <- getZombieByUrl newUrl
      nextPageReview nId baseUrl (newResponse:szResponseList)
    else do
      return (map getPostDetails szResponseList)


endcodeAndCompress :: [PostDetails] -> BS.ByteString
endcodeAndCompress postDetails = GZip.compress $ encode postDetails


saveAndnextPageReview :: Int -> Int -> String -> SzTwitterResponse -> IO Int
saveAndnextPageReview i nId baseUrl szResponse = do
  let encodedPosts = endcodeAndCompress (getPostDetails szResponse) :: BS.ByteString
  let netPostData = makeNpdObject i nId encodedPosts
  insertPostDetails myConnDetails netPostData

  if (hasNextPage szResponse)
    then do
      let newUrl = baseUrl ++ (fromJust (getEndCursor szResponse)) -- This should instead account for Maybe
      newResponse <- getZombieByUrl newUrl
      saveAndnextPageReview (i + 1) nId baseUrl newResponse
    else do
      return i



-- Start Server
startServer :: IO ()
startServer = do
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

      -- ----- Parse Response & Hit Zombie Repeatedly -------
      -- mySzResultList <- liftAndCatchIO $ nextPageReview nId zombieUrl [szResponse]
      -- let postLength = countPostDetails mySzResultList
      --
      -- ------- Save to MySQL As One Blob -------
      -- let postData = makeNpdObject nId (GZip.compress $ encode mySzResultList)
      -- confSave <- liftAndCatchIO $ insertPostDetails myConnDetails postData


----------------- Alt Logic to Process Posts Individually & Async ------------------------
      pagesAdded <- liftAndCatchIO $ saveAndnextPageReview 1 nId zombieUrl szResponse
------------------------------------------------------------------------------------------

      -- ----- Hit Callback with Outcome -------
      let myCallback = (callback initResponse)
      saveResponse <- liftAndCatchIO $ successCallbackConnection myCallback

      -- ----- Make & Return Confirmation (status & uuid) -------
      let closingObj = makeConfirmation (code saveResponse) (uuid initResponse) (pagesAdded)
      json (closingObj :: Confirmation)
