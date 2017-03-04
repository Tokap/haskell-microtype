{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module MidDb where

import Control.Monad
import Database.MySQL.Simple
import Data.Word (Word16)
import Data.Int (Int64)

import StartDataTypes
import StartDb (ConnectionDetails, makeConnection)
import MidDataTypes
import qualified Data.ByteString.Lazy as BS


insertPostStatement :: Query
insertPostStatement = "INSERT into network_post_data (network_account_id,post_data,page_number) VALUES (?,?,?)"

insertPostDetails :: ConnectionDetails -> NetworkPostData -> IO Int64
insertPostDetails connDetails post = do
  conn <- makeConnection connDetails
  execute conn insertPostStatement (
      (getNetworkId post)  :: Int
    , (getPostData post)   :: BS.ByteString
    , (0)                  :: Int
    )
