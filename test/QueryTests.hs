module QueryTests ( tests ) where

import           Asserts

import           Data.Aeson             ( Value )
import           Data.Int

import           Database.EJDB2
import           Database.EJDB2.Options
import qualified Database.EJDB2.Query   as Query

import           Plant

import           Prelude                hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testReadOnlyDatabaseOpts) close $ \databaseIO ->
    testGroup "get"
              [ getListWithBoolQueryTest databaseIO
              , getListWithI64QueryTest databaseIO
              , getListWithI64AtIndexQueryTest databaseIO
              , getListWithF64QueryTest databaseIO
              , getListWithF64AtIndexQueryTest databaseIO
              , getListWithStringQueryTest databaseIO
              , getListWithStringAtIndexQueryTest databaseIO
              , getListWithRegexQueryTest databaseIO
              , getListWithRegexAtIndexQueryTest databaseIO
              , getListWithNullQueryTest databaseIO
              , getListWithNullAtIndexQueryTest databaseIO
              , getListWithMixedQueryTest databaseIO
              , getListWithTwoStringsQueryTest databaseIO
              ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts =
    minimalOptions "./test/read-only-db" [ readonlyOpenFlags ]

getListWithBoolQueryTest :: IO Database -> TestTree
getListWithBoolQueryTest databaseIO = testCase "getListWithBoolQuery" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[isTree=:?] | asc /name"
    Query.setBoolAtIndex False 0 query
    plants <- getList database query
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
                                      , description =
                                            Just "tipical alpine flower"
                                      }
                  )
            , ( 4
                  , Just nothingPlant { id          = Nothing
                                      , name        =
                                            Just "leucanthemum vulgare"
                                      , isTree      = Just False
                                      , year        = Just 1778
                                      , description = Just "very common flower in Italy 🍕"
                                      , ratio       = Just 1.618
                                      }
                  )
            ]

getListWithI64QueryTest :: IO Database -> TestTree
getListWithI64QueryTest databaseIO = testCase "getListWithI64Query" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[year>:year] | asc /name"
    Query.setI64 1800 "year" query
    plants <- getList database query
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
                                      , description =
                                            Just "tipical alpine flower"
                                      }
                  )
            ]

getListWithI64AtIndexQueryTest :: IO Database -> TestTree
getListWithI64AtIndexQueryTest databaseIO = testCase "getListWithI64AtIndexQuery" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[year>:?] | asc /name"
    Query.setI64AtIndex 1800 0 query
    plants <- getList database query
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
                                      , description =
                                            Just "tipical alpine flower"
                                      }
                  )
            ]

getListWithF64QueryTest :: IO Database -> TestTree
getListWithF64QueryTest databaseIO = testCase "getListWithF64Query" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[ratio > :ratio] | asc /name"
    Query.setF64 1.6 "ratio" query
    plants <- getList database query
    plants @?= [ ( 4
                     , Just nothingPlant { id          = Nothing
                                         , name        =
                                               Just "leucanthemum vulgare"
                                         , isTree      = Just False
                                         , year        = Just 1778
                                         , description = Just "very common flower in Italy 🍕"
                                         , ratio       = Just 1.618
                                         }
                     )
               ]

getListWithF64AtIndexQueryTest :: IO Database -> TestTree
getListWithF64AtIndexQueryTest databaseIO =
    testCase "getListWithF64AtIndexQuery" $ do
        database <- databaseIO
        query <- Query.fromString "@plants/[ratio > :?] | asc /name"
        Query.setF64AtIndex 1.6 0 query
        plants <- getList database query
        plants @?= [ ( 4
                         , Just nothingPlant { id          = Nothing
                                             , name        =
                                                   Just "leucanthemum vulgare"
                                             , isTree      = Just False
                                             , year        = Just 1778
                                             , description = Just "very common flower in Italy 🍕"
                                             , ratio       = Just 1.618
                                             }
                         )
                   ]

getListWithStringQueryTest :: IO Database -> TestTree
getListWithStringQueryTest databaseIO = testCase "getListWithStringQuery" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[name=:name] | asc /name"
    Query.setString "pinus" "name" query
    plants <- getList database query
    plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         }
                     )
               ]

getListWithStringAtIndexQueryTest :: IO Database -> TestTree
getListWithStringAtIndexQueryTest databaseIO =
    testCase "getListWithStringAtIndexQuery" $ do
        database <- databaseIO
        query <- Query.fromString "@plants/[name=:?] | asc /name"
        Query.setStringAtIndex "pinus" 0 query
        plants <- getList database query
        plants @?= [ ( 1
                         , Just nothingPlant { id          = Nothing
                                             , name        = Just "pinus"
                                             , isTree      = Just True
                                             , year        = Just 1753
                                             , description = Just "wow 🌲"
                                             }
                         )
                   ]

getListWithRegexQueryTest :: IO Database -> TestTree
getListWithRegexQueryTest databaseIO = testCase "getListWithRegexQuery" $ do
    database <- databaseIO
    query
        <- Query.fromString "@plants/[description re :description ] | asc /name"
    Query.setRegex ".*Italy.*" "description" query
    plants <- getList database query
    plants @?= [ ( 4
                     , Just nothingPlant { id          = Nothing
                                         , name        =
                                               Just "leucanthemum vulgare"
                                         , isTree      = Just False
                                         , year        = Just 1778
                                         , description = Just "very common flower in Italy 🍕"
                                         , ratio       = Just 1.618
                                         }
                     )
               ]

getListWithRegexAtIndexQueryTest :: IO Database -> TestTree
getListWithRegexAtIndexQueryTest databaseIO =
    testCase "getListWithRegexAtIndexQuery" $ do
        database <- databaseIO
        query <- Query.fromString "@plants/[description re :?] | asc /name"
        Query.setRegexAtIndex "very.*Italy" 0 query
        plants <- getList database query
        plants @?= [ ( 4
                         , Just nothingPlant { id          = Nothing
                                             , name        =
                                                   Just "leucanthemum vulgare"
                                             , isTree      = Just False
                                             , year        = Just 1778
                                             , description = Just "very common flower in Italy 🍕"
                                             , ratio       = Just 1.618
                                             }
                         )
                   ]

getListWithNullQueryTest :: IO Database -> TestTree
getListWithNullQueryTest databaseIO = testCase "getListWithNullQuery" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[ratio=:ratio] | asc /name"
    Query.setNull "ratio" query
    plants <- getList database query
    plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         , ratio       = Nothing
                                         }
                     )
               ]

getListWithNullAtIndexQueryTest :: IO Database -> TestTree
getListWithNullAtIndexQueryTest databaseIO =
    testCase "getListWithNullAtIndexQuery" $ do
        database <- databaseIO
        query <- Query.fromString "@plants/[ratio=:?] | asc /name"
        Query.setNullAtIndex 0 query
        plants <- getList database query
        plants @?= [ ( 1
                         , Just nothingPlant { id          = Nothing
                                             , name        = Just "pinus"
                                             , isTree      = Just True
                                             , year        = Just 1753
                                             , description = Just "wow 🌲"
                                             , ratio       = Nothing
                                             }
                         )
                   ]

getListWithMixedQueryTest :: IO Database -> TestTree
getListWithMixedQueryTest databaseIO = testCase "getListWithMixedQuery" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/[year>:year] and /[isTree=:?] and /[name=:?] | asc /name"
    Query.setI64 1700 "year" query
    Query.setBoolAtIndex True 0 query
    Query.setStringAtIndex "pinus" 1 query
    plants <- getList database query
    plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         }
                     )
               ]

getListWithTwoStringsQueryTest :: IO Database -> TestTree
getListWithTwoStringsQueryTest databaseIO =
    testCase "getListWithTwoStringsQuery" $ do
        database <- databaseIO
        query <- Query.fromString "@plants/[description re :descr] and /[name=:name] | asc /name"
        Query.setRegex ".*wow.*" "descr" query
        Query.setString "pinus" "name" query
        plants <- getList database query
        plants @?= [ ( 1
                         , Just nothingPlant { id          = Nothing
                                             , name        = Just "pinus"
                                             , isTree      = Just True
                                             , year        = Just 1753
                                             , description = Just "wow 🌲"
                                             }
                         )
                   ]
