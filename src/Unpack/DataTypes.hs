{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Unpack.DataTypes where

import Prelude hiding (id)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Int (Int64)
import Data.Maybe
import GHC.Generics

import DataProcess.DataTypes


--------------------------------------------------------------------------------
----------------------------- Packed Data --------------------------------------
--------------------------------------------------------------------------------

data PackedPostPage = PackedPostPage {
  pId                :: Int
, network_account_id :: Int
, page_numer         :: Int
, post_data_bs       :: BS.ByteString
} deriving (Show, Generic)


decodeAndDecompress :: BS.ByteString -> [PostDetails]
decodeAndDecompress byteString = fromJust $ decode ( GZip.decompress byteString ) :: [PostDetails]

getPostDataBs :: PackedPostPage -> BS.ByteString
getPostDataBs p3 = (post_data_bs p3)

getPostsAndDecompress :: PackedPostPage -> [PostDetails]
getPostsAndDecompress p3 = decodeAndDecompress $ getPostDataBs p3

makePackedPostPage :: Int -> Int -> Int -> BS.ByteString -> PackedPostPage
makePackedPostPage pId naId pgNum pd = PackedPostPage {
  pId                = pId
, network_account_id = naId
, page_numer         = pgNum
, post_data_bs       = pd
}
