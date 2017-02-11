{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BoardModel where

import DB (NumberPlace (..))

import qualified Data.Aeson   as AE
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Internal as BI
import           GHC.Generics (Generic)

data NP = NP { board :: [[Int]] }
         deriving (Show, Generic)

data NPResponse = NPResponse { boards :: [[[Int]]], status :: Int, id :: Int }
                  deriving (Show, Generic)

instance AE.FromJSON NP
instance AE.ToJSON NP

instance AE.FromJSON NPResponse
instance AE.ToJSON NPResponse

decodeBiNp :: BI.ByteString -> Maybe NP
decodeBiNp biNp = (AE.decodeStrict biNp :: Maybe NP)

decodeBliNp :: BLI.ByteString -> Maybe NP
decodeBliNp bliNp = (AE.decode bliNp :: Maybe NP)


data NumberPlaces = NumberPlaces { number_places :: [NumberPlace], list_status :: Int}
                     deriving (Show, Generic)

instance AE.FromJSON NumberPlaces
instance AE.ToJSON NumberPlaces

numberPlacesError :: Int -> NumberPlaces
numberPlacesError status = NumberPlaces [NumberPlace [[0]] [[[0]]]] status
