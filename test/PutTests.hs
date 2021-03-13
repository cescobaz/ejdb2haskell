{-# LANGUAGE OverloadedStrings #-}

module PutTests ( tests ) where

import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as Map
import qualified Data.Vector            as Vector

import           Database.EJDB2
import           Database.EJDB2.Options

import           Plant

import           Prelude                hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $ \databaseIO ->
    testGroup "put"
              [ putNewTest databaseIO
              , putOnNewIdTest databaseIO
              , putOnExistingIdTest databaseIO
              , mergeOrPutNewTest databaseIO
              , mergeOrPutExistingTest databaseIO
              , patchTest databaseIO
              ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/put-db" [ truncateOpenFlags ]

putNewTest :: IO Database -> TestTree
putNewTest databaseIO = testCase "putNewTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    storedPlant <- getById database "plants" id
    storedPlant @?= Just plant
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         , insects     = Just [ "ant", "beetle" ]
                         , leaf        = Just (Leaf "canada" 42)
                         , theLeaf     = Leaf "mary" 420
                         }

putOnNewIdTest :: IO Database -> TestTree
putOnNewIdTest databaseIO = testCase "putOnNewIdTest" $ do
    database <- databaseIO
    put database "plants" plant 42
    storedPlant <- getById database "plants" 42
    storedPlant @?= Just plant
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

putOnExistingIdTest :: IO Database -> TestTree
putOnExistingIdTest databaseIO = testCase "putOnExistingIdTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    put database "plants" lastPlant id
    storedPlant <- getById database "plants" id
    storedPlant @?= Just lastPlant
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

    lastPlant = plant { description = Just "a tipical christmas tree" }

mergeOrPutNewTest :: IO Database -> TestTree
mergeOrPutNewTest databaseIO = testCase "mergeOrPutNewTest" $ do
    database <- databaseIO
    mergeOrPut database "plants" jsonPut 4242
    storedPlant <- getById database "plants" 4242
    storedPlant @?= Just plant
  where
    jsonPut = Aeson.Object $ Map.fromList [ ("name", "pinus")
                                          , ("isTree", Aeson.Bool True)
                                          , ("year", Aeson.Number 1753)
                                          , ("description", "wow 🌲")
                                          ]

    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

mergeOrPutExistingTest :: IO Database -> TestTree
mergeOrPutExistingTest databaseIO = testCase "mergeOrPutExistingTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    mergeOrPut database "plants" jsonPatch id
    storedPlant <- getById database "plants" id
    storedPlant @?= Just lastPlant
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

    jsonPatch = Aeson.Object $
        Map.fromList [ ("year", Aeson.Null)
                     , ("description", "a tipical christmas tree")
                     ]

    lastPlant =
        plant { year = Nothing, description = Just "a tipical christmas tree" }

patchTest :: IO Database -> TestTree
patchTest databaseIO = testCase "patchTest" $ do
    database <- databaseIO
    id <- putNew database "plants" plant
    patch database "plants" jsonPatch id
    storedPlant <- getById database "plants" id
    storedPlant @?= Just lastPlant
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

    jsonPatch = Aeson.Array $
        Vector.fromList [ Aeson.Object $
                          Map.fromList [ ("op", "remove"), ("path", "/year") ]
                        , Aeson.Object $
                          Map.fromList [ ("op", "replace")
                                       , ("path", "/description")
                                       , ("value", "a tipical christmas tree")
                                       ]
                        ]

    lastPlant =
        plant { year = Nothing, description = Just "a tipical christmas tree" }
