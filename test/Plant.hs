{-# LANGUAGE DeriveGeneric #-}

module Plant where

import           Data.HashSet   as HashSet

import           Database.EJDB2

import           GHC.Generics

import           Prelude        hiding ( id )

data Leaf = Leaf { color :: String, size :: Int }
    deriving ( Eq, Generic, Show )

instance FromJBL Leaf

instance ToJBL Leaf

data Plant = Plant { id          :: Maybe Int
                   , name        :: Maybe String
                   , isTree      :: Maybe Bool
                   , year        :: Maybe Int
                   , description :: Maybe String
                   , ratio       :: Maybe Double
                   , insects     :: Maybe [String]
                   , ids         :: Maybe (HashSet Int)
                   , leaf        :: Maybe Leaf
                   , theLeaf     :: Leaf
                   }
    deriving ( Eq, Generic, Show )

instance FromJBL Plant

instance ToJBL Plant

instance EJDB2IDObject Plant where
    setId oid o = o { id = Just (fromIntegral oid) }

nothingPlant :: Plant
nothingPlant =
    Plant Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          (Leaf "" 0)
