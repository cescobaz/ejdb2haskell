{-# LANGUAGE DeriveGeneric #-}

module Plant where

import           Data.Aeson   ( FromJSON, Value )

import           GHC.Generics

import           Prelude      hiding ( id )

data Plant = Plant { id          :: Maybe Int
                   , name        :: Maybe String
                   , isTree      :: Maybe Bool
                   , year        :: Maybe Int
                   , description :: Maybe String
                   }
    deriving ( Eq, Generic, Show )

instance FromJSON Plant
