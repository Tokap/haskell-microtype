{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module DataProcess.Db where

import Control.Monad
import Database.MySQL.Simple
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BS

import Confirmation.DataTypes
import Confirmation.Db (ConnectionDetails, makeConnection)
import DataProcess.DataTypes


insertPostStatement :: Query
insertPostStatement = "INSERT into network_post_data \
                      \(network_account_id,post_data,page_number)\
                      \ VALUES (?,?,?)"

insertPostDetails :: ConnectionDetails -> NetworkPostData -> IO Int64
insertPostDetails connDetails post = do
  conn <- makeConnection connDetails
  execute conn insertPostStatement (
      (getNetworkId post)  :: Int
    , (getPostData post)   :: BS.ByteString
    , (getPageNumber post) :: Int
    )
