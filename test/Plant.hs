{-# LANGUAGE DeriveGeneric #-}

module Plant where

import           Data.Aeson   ( FromJBL, ToJBL )

import           GHC.Generics

import           Prelude      hiding ( id )

data Plant = Plant { id          :: Maybe Int
                   , name        :: Maybe String
                   , isTree      :: Maybe Bool
                   , year        :: Maybe Int
                   , description :: Maybe String
                   , ratio       :: Maybe Double
                   }
    deriving ( Eq, Generic, Show )

instance FromJBL Plant

instance ToJBL Plant

nothingPlant :: Plant
nothingPlant = Plant Nothing Nothing Nothing Nothing Nothing Nothing
