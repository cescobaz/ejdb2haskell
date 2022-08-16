module QueryTests ( tests ) where

import           Asserts

import           Data.Aeson             ( Value )
import qualified Data.HashSet           as HashSet
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
    let query = Query "@plants/[isTree=:?] | asc /name"
                      (setBoolAtIndex False 0)
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

getListWithI64QueryTest :: IO Database -> TestTree
getListWithI64QueryTest databaseIO = testCase "getListWithI64Query" $ do
    database <- databaseIO
    let query = Query "@plants/[year>:year] | asc /name" (setI64 1800 "year")
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
                                  , description = Just "tipical alpine flower"
                                  }
              )
            ]

getListWithI64AtIndexQueryTest :: IO Database -> TestTree
getListWithI64AtIndexQueryTest databaseIO = testCase "getListWithI64AtIndexQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[year>:?] | asc /name" (setI64AtIndex 1800 0)
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
                                  , description = Just "tipical alpine flower"
                                  }
              )
            ]

getListWithF64QueryTest :: IO Database -> TestTree
getListWithF64QueryTest databaseIO = testCase "getListWithF64Query" $ do
    database <- databaseIO
    let query = Query "@plants/[ratio > :ratio] | asc /name"
                      (setF64 1.6 "ratio")
    plants <- getList database query
    plants
        @?= [ ( 4
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

getListWithF64AtIndexQueryTest :: IO Database -> TestTree
getListWithF64AtIndexQueryTest databaseIO = testCase "getListWithF64AtIndexQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[ratio > :?] | asc /name" (setF64AtIndex 1.6 0)
    plants <- getList database query
    plants
        @?= [ ( 4
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

getListWithStringQueryTest :: IO Database -> TestTree
getListWithStringQueryTest databaseIO = testCase "getListWithStringQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[name=:name] | asc /name"
                      (setString "pinus" "name")
    plants <- getList database query
    plants @?= [ ( 1
                 , Just nothingPlant { id          = Nothing
                                     , name        = Just "pinus"
                                     , isTree      = Just True
                                     , year        = Just 1753
                                     , description = Just "wow 🌲"
                                     , insects     = Just [ "ant" ]
                                     , ids         =
                                           HashSet.fromList [ 42, 23, 35 ]
                                     , theLeaf     = Leaf "green" 420
                                     }
                 )
               ]

getListWithStringAtIndexQueryTest :: IO Database -> TestTree
getListWithStringAtIndexQueryTest databaseIO =
    testCase "getListWithStringAtIndexQuery" $ do
        database <- databaseIO
        let query = Query "@plants/[name=:?] | asc /name"
                          (setStringAtIndex "pinus" 0)
        plants <- getList database query
        plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         , insects     = Just [ "ant" ]
                                         , ids         =
                                               HashSet.fromList [ 42, 23, 35 ]
                                         , theLeaf     = Leaf "green" 420
                                         }
                     )
                   ]

getListWithRegexQueryTest :: IO Database -> TestTree
getListWithRegexQueryTest databaseIO = testCase "getListWithRegexQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[description re :description ] | asc /name"
                      (setRegex ".*Italy.*" "description")
    plants <- getList database query
    plants
        @?= [ ( 4
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

getListWithRegexAtIndexQueryTest :: IO Database -> TestTree
getListWithRegexAtIndexQueryTest databaseIO = testCase "getListWithRegexAtIndexQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[description re :?] | asc /name"
                      (setRegexAtIndex "very.*Italy" 0)
    plants <- getList database query
    plants
        @?= [ ( 4
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

getListWithNullQueryTest :: IO Database -> TestTree
getListWithNullQueryTest databaseIO = testCase "getListWithNullQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[ratio=:ratio] | asc /name" (setNull "ratio")
    plants <- getList database query
    plants @?= [ ( 1
                 , Just nothingPlant { id          = Nothing
                                     , name        = Just "pinus"
                                     , isTree      = Just True
                                     , year        = Just 1753
                                     , description = Just "wow 🌲"
                                     , ratio       = Nothing
                                     , insects     = Just [ "ant" ]
                                     , ids         =
                                           HashSet.fromList [ 42, 23, 35 ]
                                     , theLeaf     = Leaf "green" 420
                                     }
                 )
               ]

getListWithNullAtIndexQueryTest :: IO Database -> TestTree
getListWithNullAtIndexQueryTest databaseIO =
    testCase "getListWithNullAtIndexQuery" $ do
        database <- databaseIO
        let query = Query "@plants/[ratio=:?] | asc /name" (setNullAtIndex 0)
        plants <- getList database query
        plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         , ratio       = Nothing
                                         , insects     = Just [ "ant" ]
                                         , ids         =
                                               HashSet.fromList [ 42, 23, 35 ]
                                         , theLeaf     = Leaf "green" 420
                                         }
                     )
                   ]

getListWithMixedQueryTest :: IO Database -> TestTree
getListWithMixedQueryTest databaseIO = testCase "getListWithMixedQuery" $ do
    database <- databaseIO
    let query = Query "@plants/[year>:year] and /[isTree=:?] and /[name=:?] | asc /name" $ do
            setI64 1700 "year"
            setBoolAtIndex True 0
            setStringAtIndex "pinus" 1
    plants <- getList database query
    plants @?= [ ( 1
                 , Just nothingPlant { id          = Nothing
                                     , name        = Just "pinus"
                                     , isTree      = Just True
                                     , year        = Just 1753
                                     , description = Just "wow 🌲"
                                     , insects     = Just [ "ant" ]
                                     , ids         =
                                           HashSet.fromList [ 42, 23, 35 ]
                                     , theLeaf     = Leaf "green" 420
                                     }
                 )
               ]

getListWithTwoStringsQueryTest :: IO Database -> TestTree
getListWithTwoStringsQueryTest databaseIO =
    testCase "getListWithTwoStringsQuery" $ do
        database <- databaseIO
        let query = Query "@plants/[description re :descr] and /[name=:name] | asc /name" $ do
                setRegex ".*wow.*" "descr"
                setString "pinus" "name"
        plants <- getList database query
        plants @?= [ ( 1
                     , Just nothingPlant { id          = Nothing
                                         , name        = Just "pinus"
                                         , isTree      = Just True
                                         , year        = Just 1753
                                         , description = Just "wow 🌲"
                                         , insects     = Just [ "ant" ]
                                         , ids         =
                                               HashSet.fromList [ 42, 23, 35 ]
                                         , theLeaf     = Leaf "green" 420
                                         }
                     )
                   ]
