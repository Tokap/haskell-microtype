{-# LANGUAGE OverloadedStrings #-}
module Mid where

import MidDataTypes
import Http

main :: IO ()
main = do
  -- grrrrArrg <- getTestZombie
  -- print grrrrArrg

  grrrrArrg <- getZombie "twitter" "jerry" 613
  print grrrrArrg

-- determine how we intend to save to DB
