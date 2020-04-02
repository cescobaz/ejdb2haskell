{-# LANGUAGE DeriveGeneric #-}

module Plant where

import           Data.Aeson   ( FromJSON, ToJSON )

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

instance FromJSON Plant

instance ToJSON Plant

nothingPlant :: Plant
nothingPlant = Plant Nothing Nothing Nothing Nothing Nothing Nothing
