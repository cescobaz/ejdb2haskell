module PutTests ( tests ) where

import           Database.EJDB2

import           Plant

import           Prelude          hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $ \databaseIO ->
    testGroup "put" [ putNewTest databaseIO, putOnNewIdTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/put-db" [ truncateOpenFlags ]

putNewTest :: IO Database -> TestTree
putNewTest databaseIO = testCase "putNewTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    storedPlant <- getById database "plants" id
    storedPlant @?= Just plant
  where
    plant = Plant { id          = Nothing
                  , name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }

putOnNewIdTest :: IO Database -> TestTree
putOnNewIdTest databaseIO = testCase "putOnNewIdTest" $ do
    database <- databaseIO
    put database "plants" plant 42
    storedPlant <- getById database "plants" 42
    storedPlant @?= Just plant
  where
    plant = Plant { id          = Nothing
                  , name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }
