{-# LANGUAGE DuplicateRecordFields #-}

module Http where

import           DataTypes
import           Data.Aeson
import           Network.HTTP.Simple

--------------------------------------------------------------------------------
------------------------------------ POSTS -------------------------------------
--------------------------------------------------------------------------------

getZombie :: IO SzTwitter
getZombie = do
    request <- parseRequest "GET http://localhost:3199/user/twitter/id/613/username/jerry/timeline"
    response <- httpJSON request

    let szOutput = getResponseBody response :: SzTwitter
    return szOutput

-- getPosts :: IO [Post]
-- getPosts = do
--     request <- parseRequest "GET https://jsonplaceholder.typicode.com/posts"
--     response <- httpJSON request
--
--     let postOutput = getResponseBody response
--     return postOutput
