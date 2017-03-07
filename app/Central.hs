{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Allows for multiple uses of data types
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}


module Central where

import DataProcess.Server

main :: IO ()
main = startServer
