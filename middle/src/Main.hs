{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataTypes
import Http

main :: IO ()
main = do
  grrrrArrg <- getZombie
  print grrrrArrg
