{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module StartDb where

import Control.Monad
import Database.MySQL.Simple
import Data.Word (Word16)
import Data.Int (Int64)

import StartDataTypes
--------------------------------------------------------------------------------
-------------------------- CONNECTION POOL -------------------------------------
--------------------------------------------------------------------------------

data ConnectionDetails = ConnectionDetails {
  host :: String,
  port :: Word16,
  user :: String,
  pass :: String,
  db   :: String
}


makeConnection :: ConnectionDetails -> IO Connection
makeConnection connDetails = do
  let connectionInfo = defaultConnectInfo {
                      connectHost     = (host connDetails),
                      connectPort     = (port connDetails),
                      connectUser     = (user connDetails),
                      connectPassword = (pass connDetails),
                      connectDatabase = (db connDetails)
                    }
  conn <- connect connectionInfo
  return conn

myConnDetails :: ConnectionDetails
myConnDetails = ConnectionDetails {
  host = "127.0.0.1",
  port = 3306,
  user = "root",
  pass = "",
  db   = "ip_hoarder"
}

--------------------------------------------------------------------------------
---------------------------- Traversal DB --------------------------------------
--------------------------------------------------------------------------------

-- QUERIES:
allPendingQuery :: Query
allPendingQuery = "SELECT \
    	\T.id AS trav_table_id, \
    	\NA.id AS net_table_id, \
    	\NA.user_id, \
    	\NA.username, \
    	\NT.code AS network_code \
    \FROM traversal AS T \
    \LEFT JOIN network_account AS NA \
    \ON T.network_account_id=NA.id \
    \LEFT JOIN network_type AS NT \
    \ON NA.network_type_id=NT.id \
    \WHERE `history_completed` IS NULL \
    \AND `processor_uuid` IS NULL;"

updateTraversalStatus :: Query
updateTraversalStatus = "UPDATE traversal \
          \SET traversal_status_id=? \
          \WHERE id=?;"

-- selectPostQuery :: Query
-- selectPostQuery = "SELECT title,body,id,userId FROM post_table WHERE id=?"
--
-- insertPostStatement :: Query
-- insertPostStatement = "INSERT into post_table (title,body,id,userId) VALUES (?,?,?,?)"
--
-- insertPostStatement' :: Query
-- insertPostStatement' = "INSERT into post_table (title,body,userId) VALUES (?,?,?)"


-- READ:
getAllPosts :: ConnectionDetails -> IO [TraversalResponse]
getAllPosts connDetails = do
  conn <- makeConnection connDetails
  xs <- query_ conn allPendingQuery

  return $ map ( \(tId, nId, uId, un, nc) -> makeTraversalResponse tId nId uId un nc) xs

-- getPostById :: ConnectionDetails -> Int -> IO [Post]
-- getPostById connDetails i = do
--   conn <- makeConnection connDetails
--   xs <- query conn selectPostQuery [(i :: Int)]
--
--   return $ map ( \(t, b, i, ui) -> makePost t b i ui) xs
--
--
-- -- UPDATE: (returns number of affected rows)
updateTraversal :: ConnectionDetails -> Int -> Int -> IO Int64
updateTraversal connDetails status tId = do
  conn <- makeConnection connDetails
  execute conn updateTraversalStatus (status :: Int, tId :: Int) :: IO Int64

-- updateTraversalSuccess :: ConnectionDetails -> IO Int64
-- updateTraversalSuccess connDetails = do
--   conn <- makeConnection connDetails
--   execute conn updateTraversalStatus (3 :: Int, 1 :: Int) :: IO Int64
--
-- insertPost' :: ConnectionDetails -> Post -> IO Int64
-- insertPost' connDetails post = do
--   conn <- makeConnection connDetails
--   execute conn insertPostStatement' (
--       (getPostTitle post)  :: String
--     , (getPostBody post)   :: String
--     , (getPostUserId post) :: Int
--     )
