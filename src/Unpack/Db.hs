{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Unpack.Db where

import           Control.Monad
import           Control.Concurrent.Async
import           Database.MySQL.Simple
import           Data.Int (Int64)
import qualified Data.ByteString.Lazy as BS

import           Unpack.DataTypes
import           DataProcess.DataTypes
import           Confirmation.Db


-- QUERIES:
allPendingCompressedQuery :: Query
allPendingCompressedQuery = "SELECT \
                            	\id As pId, \
                            	\network_account_id, \
                            	\page_number, \
                            	\post_data \
                            \FROM network_post_data \

                            \WHERE `network_account_id`=?;"


getAllPackedData :: ConnectionDetails -> Int -> IO [PackedPostPage]
getAllPackedData connDetails nId = do
  conn <- makeConnection connDetails
  xs <- query conn allPendingCompressedQuery (Only nId)

  return $ map ( \(pId, naId, pgNum, postData) -> makePackedPostPage pId naId pgNum postData) xs

getPostPagesById :: ConnectionDetails -> Int -> IO [[PostDetails]]
getPostPagesById connDetails nId = do
  compressedPostDetailsList <- getAllPackedData connDetails nId
  return $ map getPostsAndDecompress compressedPostDetailsList
