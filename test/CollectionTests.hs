module CollectionTests ( tests ) where

import           Asserts

import           Database.EJDB2
import qualified Database.EJDB2.Query as Query

import           Plant

import           Prelude              hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $
    \databaseIO -> testGroup "collection" [ removeTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/collection-db" [ truncateOpenFlags ]

removeTest :: IO Database -> TestTree
removeTest databaseIO = testCase "removeTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    removeCollection database "plants"
    count <- Query.fromString "@plants/*" >>= getCount database
    count @?= 0
  where
    plant = Plant { id          = Nothing
                  , name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }

renameTest :: IO Database -> TestTree
renameTest databaseIO = testCase "removeTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    renameCollection database "plants" "vegetables"
    count <- Query.fromString "@plants/*" >>= getCount database
    count @?= 0
    storedPlant <- getById database "vegetables" id
    storedPlant @?= Just plant
  where
    plant = Plant { id          = Nothing
                  , name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }
