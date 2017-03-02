{-# LANGUAGE OverloadedStrings #-}
module Mid where

import StartDataTypes
import MidDataTypes
import Http

main :: IO ()
main = do
  -- grrrrArrg <- getTestZombie
  -- print grrrrArrg

  -- grrrrArrg <- getZombie "twitter" "jerry" 613
  -- print grrrrArrg

  grrrrArrg <- hitPrimaryUrl
  print $ response grrrrArrg

-- determine how we intend to save to DB
