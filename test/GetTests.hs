{-# LANGUAGE DeriveGeneric #-}

module GetTests ( tests ) where

import           Data.Aeson       ( FromJSON )

import           Database.EJDB2

import           GHC.Generics

import           Test.Tasty
import           Test.Tasty.HUnit

data Plant = Plant { name        :: Maybe String
                   , isTree      :: Maybe Bool
                   , year        :: Maybe Int
                   , description :: Maybe String
                   }
    deriving ( Eq, Generic, Show )

instance FromJSON Plant

tests :: TestTree
tests = testGroup "get" [ getByIdAndParseToMap ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts = minimalOptions "./test/read-only-db"

getByIdAndParseToMap :: TestTree
getByIdAndParseToMap = withResource (open testReadOnlyDatabaseOpts) close $
    \databaseIO -> testCase "getByIdAndParseToMap" $ do
        database <- databaseIO
        plant <- getById database "plants" 1
        plant @?= Just Plant { name        = Just "pinus"
                             , isTree      = Just True
                             , year        = Just 1753
                             , description = Just "wow 🌲"
                             }

