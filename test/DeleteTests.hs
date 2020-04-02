module DeleteTests ( tests ) where

import           Asserts

import           Database.EJDB2
import           Database.EJDB2.Options

import           Plant

import           Prelude                hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $ \databaseIO ->
    testGroup "delete"
              [ deleteTest databaseIO, deleteNotExistingTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/delete-db" [ truncateOpenFlags ]

deleteTest :: IO Database -> TestTree
deleteTest databaseIO = testCase "deleteTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    delete database "plants" id
    storedPlant <- getById database "plants" id :: IO (Maybe Plant)
    storedPlant @?= Nothing
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

deleteNotExistingTest :: IO Database -> TestTree
deleteNotExistingTest databaseIO = testCase "deleteNotExistingTest" $ do
    database <- databaseIO
    assertException (userError "ErrorNotFound") (delete database "plants" 42)
