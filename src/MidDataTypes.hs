{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Allows for multiple uses of data types
{-# LANGUAGE DeriveGeneric #-}

module MidDataTypes where
import Prelude hiding (id)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

--------------------------------------------------------------------------------
---------------------------- Social Zombie -------------------------------------
--------------------------------------------------------------------------------

data SzTwitterResponse = SzTwitterResponse {
  count     :: Int,
  page_info :: PageInfo,
  nodes     :: [PostDetails]
} deriving (Show, Generic)
instance ToJSON SzTwitterResponse
instance FromJSON SzTwitterResponse


data PageInfo = PageInfo {
  start_cursor      :: String,
  end_cursor        :: String,
  has_previous_page :: Bool,
  has_next_page     :: Bool
} deriving (Show, Generic)
instance ToJSON PageInfo
instance FromJSON PageInfo

data GeoLocation = GeoLocation {
latitude  :: Maybe String,
longitude :: Maybe String,
country   :: Maybe String,
state     :: Maybe String,
city      :: Maybe String,
zip       :: Maybe String
} deriving (Show, Generic)
instance ToJSON GeoLocation
instance FromJSON GeoLocation

data PostDetails = PostDetails {
network_type_code :: Maybe String,
foreign_user_id   :: Maybe String,
foreign_username  :: Maybe String,
foreign_post_id   :: Maybe String,
permalink         :: Maybe String,
image_url         :: Maybe String,
video_url         :: Maybe String,
source            :: Maybe String,
media_title       :: Maybe String,
media_caption     :: Maybe String,
media_description :: Maybe String,
geolocation       :: GeoLocation,
time_posted       :: Maybe Int,
expiration_time   :: Maybe Int,
view_count        :: Int,
replay_count      :: Int,
comment_count     :: Int,
like_count        :: Int,
dislike_count     :: Int,
favorite_count    :: Int,
share_count       :: Int,
is_image          :: Bool,
is_video          :: Bool,
is_edited         :: Bool,
is_ad             :: Bool,
is_origin         :: Bool,
is_reply          :: Bool,
tags              :: Maybe [String],
title             :: Maybe String,
text              :: Maybe String,
subtext           :: Maybe String
} deriving (Show, Generic)
instance ToJSON PostDetails
instance FromJSON PostDetails
