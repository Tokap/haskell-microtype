{-# LANGUAGE OverloadedStrings #-}
module Start where

-- import StartDataTypes
import StartDb

main :: IO ()
main = do
  z <- updateTraversal myConnDetails 3 1
  print z
