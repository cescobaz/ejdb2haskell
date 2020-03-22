module GetTests ( tests ) where

import qualified Data.HashMap.Strict as Map

import           Database.EJDB2

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "get" [ getByIdAndParseToMap ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts = minimalOptions "./test/read-only-db"

getByIdAndParseToMap :: TestTree
getByIdAndParseToMap = withResource (open testReadOnlyDatabaseOpts) (close) $
    \databaseIO -> testCase "getByIdAndParseToMap" $ do
        database <- databaseIO
        plant <- getById database "plants" 1
        putStrLn $ show plant
        plant @?= (Just (Map.fromList [ ("name", "pinus") ]))

