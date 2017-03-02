{-# LANGUAGE OverloadedStrings #-}

module MidServer where

import Web.Scotty

import StartDb
import MidDataTypes
import StartDataTypes
import Http



startServer :: IO ()
startServer = do
  putStrLn "Starting Server on Port: 4000"

  scotty 4000 $ do
--------------------------------------------------------------------------------
---------------------------------- Get Reqs ------------------------------------
--------------------------------------------------------------------------------

    -- get "/posts" $ do
    --   allPosts <- liftAndCatchIO $ getAllPosts myConnDetails :: ActionM [Post]
    --   json (allPosts :: [Post])
    --
    -- get "/comments" $ do
    --   allComments <- liftAndCatchIO $ getAllComments myConnDetails :: ActionM [Comment]
    --   json (allComments :: [Comment])
    --
    -- get "/posts/:id" $ do
    --   pId <- param "id"
    --   allPosts <- liftAndCatchIO $ getPostById myConnDetails pId :: ActionM [Post]
    --   json (allPosts :: [Post])
    --
    -- get "/comments/:id" $ do
    --   cId <- param "id"
    --   allComments <- liftAndCatchIO $ getCommentById myConnDetails cId :: ActionM [Comment]
    --   json (allComments :: [Comment])

--------------------------------------------------------------------------------
---------------------------------- Post Reqs -----------------------------------
--------------------------------------------------------------------------------

--- COMMENTS:
    post "/comments" $ do
      newComment <- jsonData :: ActionM Comment
      confSave <- liftAndCatchIO $ insertComment myConnDetails newComment
      json newComment

    post "/comments/add" $ do
      newComment <- liftAndCatchIO $ Req.getComment :: ActionM Comment
      confSave <- liftAndCatchIO $ insertComment myConnDetails newComment
      json (newComment :: Comment)

    post "/comments/add/alot" $ do
      newComments <- liftAndCatchIO $ Req.getComments :: ActionM [Comment]
      confSave <- liftAndCatchIO $ mapM (insertComment myConnDetails) newComments
      json (newComments :: [Comment])

--- POSTS:
    post "/posts" $ do
      newPost <- jsonData :: ActionM Post
      confSave <- liftAndCatchIO $ insertPost' myConnDetails newPost
      json newPost

    post "/posts/add" $ do
      newPost <- liftAndCatchIO $ Req.getPost :: ActionM Post
      confSave <- liftAndCatchIO $ insertPost' myConnDetails newPost
      json (newPost :: Post)

    post "/posts/add/alot" $ do
      newPosts <- liftAndCatchIO $ Req.getPosts :: ActionM [Post]
      confSave <- liftAndCatchIO $ mapM (insertPost' myConnDetails) newPosts
      json (newPosts :: [Post])
