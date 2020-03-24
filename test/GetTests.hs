{-# LANGUAGE DeriveGeneric #-}

module GetTests ( tests ) where

import           Data.Aeson           ( FromJSON )

import           Database.EJDB2
import qualified Database.EJDB2.Query as Query

import           GHC.Generics

import           Prelude              hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

data Plant = Plant { id          :: Maybe Int
                   , name        :: Maybe String
                   , isTree      :: Maybe Bool
                   , year        :: Maybe Int
                   , description :: Maybe String
                   }
    deriving ( Eq, Generic, Show )

instance FromJSON Plant

tests :: TestTree
tests = withResource (open testReadOnlyDatabaseOpts) close $ \databaseIO ->
    testGroup "get"
              [ getByIdTest databaseIO
              , getByIdNotFoundTest databaseIO
              , getListTest databaseIO
              , getListTest' databaseIO
              ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts =
    minimalOptions "./test/read-only-db" [ readonlyOpenFlags ]

getByIdTest :: IO Database -> TestTree
getByIdTest databaseIO = testCase "getById" $ do
    database <- databaseIO
    plant <- getById database "plants" 1
    plant @?= Just Plant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

getByIdNotFoundTest :: IO Database -> TestTree
getByIdNotFoundTest databaseIO = testCase "getById - not found" $ do
    database <- databaseIO
    plant <- getById database "plants" 42
    plant @?= (Nothing :: Maybe Plant)

getListTest :: IO Database -> TestTree
getListTest databaseIO = testCase "getList" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[isTree=:tree] | asc /name"
    Query.setBool False "tree" query
    plants <- getList database query
    plants @?= [ ( 2
                     , Just Plant { id          = Nothing
                                  , name        = Just "gentiana brentae"
                                  , isTree      = Just False
                                  , year        = Just 2008
                                  , description = Just "violet 🌺flower"
                                  }
                     )
               , ( 3
                     , Just Plant { id          = Nothing
                                  , name        = Just "leontopodium"
                                  , isTree      = Just False
                                  , year        = Just 1817
                                  , description = Just "tipical alpine flower"
                                  }
                     )
               , ( 4
                     , Just Plant { id          = Nothing
                                  , name        = Just "leucanthemum vulgare"
                                  , isTree      = Just False
                                  , year        = Just 1778
                                  , description =
                                        Just "very common flower in Italy 🍕"
                                  }
                     )
               ]

getListTest' :: IO Database -> TestTree
getListTest' databaseIO = testCase "getList'" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[isTree=:tree] | asc /name"
    Query.setBool False "tree" query
    plants <- getList' database query
    plants
        @?= [ Just Plant { id          = Just 2
                         , name        = Just "gentiana brentae"
                         , isTree      = Just False
                         , year        = Just 2008
                         , description = Just "violet 🌺flower"
                         }
            , Just Plant { id          = Just 3
                         , name        = Just "leontopodium"
                         , isTree      = Just False
                         , year        = Just 1817
                         , description = Just "tipical alpine flower"
                         }
            , Just Plant { id          = Just 4
                         , name        = Just "leucanthemum vulgare"
                         , isTree      = Just False
                         , year        = Just 1778
                         , description = Just "very common flower in Italy 🍕"
                         }
            ]
