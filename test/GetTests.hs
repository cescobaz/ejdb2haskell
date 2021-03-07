module GetTests ( tests ) where

import           Asserts

import           Control.Exception
import           Control.Monad

import           Data.Aeson             ( Value )
import           Data.Int

import           Database.EJDB2
import           Database.EJDB2.Options

import           Plant

import           Prelude                hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testReadOnlyDatabaseOpts) close $ \databaseIO ->
    testGroup "get"
              [ getByIdTest databaseIO
              , getByIdNotFoundTest databaseIO
              , getCountTest databaseIO
              , getListTest databaseIO
              , getListTest' databaseIO
              , getByIdFromNotExistingCollectionTest databaseIO
              , getListFromNotExistingCollectionTest databaseIO
              ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts =
    minimalOptions "./test/read-only-db" [ readonlyOpenFlags ]

getByIdTest :: IO Database -> TestTree
getByIdTest databaseIO = testCase "getById" $ do
    database <- databaseIO
    plant <- getById database "plants" 1
    plant @?= Just nothingPlant { id          = Nothing
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

getCountTest :: IO Database -> TestTree
getCountTest databaseIO = testCase "getCount" $ do
    database <- databaseIO
    count <- getCount database $ Query "@plants/*" noBind
    count @?= 4

getListTestQuery :: Query ()
getListTestQuery =
    Query "@plants/[isTree=:tree] | asc /name" $ setBool False "tree"

getListTest :: IO Database -> TestTree
getListTest databaseIO = testCase "getList" $ do
    database <- databaseIO
    plants <- getList database getListTestQuery
    plants
        @?= [ ( 2
              , Just nothingPlant { id          = Nothing
                                  , name        = Just "gentiana brentae"
                                  , isTree      = Just False
                                  , year        = Just 2008
                                  , description = Just "violet 🌺flower"
                                  }
              )
            , ( 3
              , Just nothingPlant { id          = Nothing
                                  , name        = Just "leontopodium"
                                  , isTree      = Just False
                                  , year        = Just 1817
                                  , description = Just "tipical alpine flower"
                                  }
              )
            , ( 4
              , Just nothingPlant { id          = Nothing
                                  , name        = Just "leucanthemum vulgare"
                                  , isTree      = Just False
                                  , year        = Just 1778
                                  , description =
                                        Just "very common flower in Italy 🍕"
                                  , ratio       = Just 1.618
                                  }
              )
            ]

getListTest' :: IO Database -> TestTree
getListTest' databaseIO = testCase "getList'" $ do
    database <- databaseIO
    plants <- getList' database getListTestQuery
    plants @?= [ Just nothingPlant { id          = Just 2
                                   , name        = Just "gentiana brentae"
                                   , isTree      = Just False
                                   , year        = Just 2008
                                   , description = Just "violet 🌺flower"
                                   }
               , Just nothingPlant { id          = Just 3
                                   , name        = Just "leontopodium"
                                   , isTree      = Just False
                                   , year        = Just 1817
                                   , description = Just "tipical alpine flower"
                                   }
               , Just nothingPlant { id          = Just 4
                                   , name        = Just "leucanthemum vulgare"
                                   , isTree      = Just False
                                   , year        = Just 1778
                                   , description =
                                         Just "very common flower in Italy 🍕"
                                   , ratio       = Just 1.618
                                   }
               ]

getByIdFromNotExistingCollectionTest :: IO Database -> TestTree
getByIdFromNotExistingCollectionTest databaseIO =
    testCase "getByIdFromNotExistingCollection" $ do
        database <- databaseIO
        assertException (userError "ErrorNotExists") -- this happens only with readonlyOpenFlags
                        (getById database "noexisting" 1 :: IO (Maybe Int))

-- on ejdb_exec there is no error if collection doesn't exists
-- https://github.com/Softmotions/ejdb/blob/40fb43a30e410b4f1bce68f79f397ce44c272c78/src/ejdb2.c#L821
getListFromNotExistingCollectionTest :: IO Database -> TestTree
getListFromNotExistingCollectionTest databaseIO =
    testCase "getListFromNotExistingCollectionTest" $ do
        database <- databaseIO
        let query = Query "@noexisting/[isTree=:tree] | asc /name" $
                setBool False "tree"
        list <- getList database query :: IO [(Int64, Maybe Int)]
        list @?= []
