module FoldTests where

import           Data.Int

import qualified Database.EJDB2         as DB
import           Database.EJDB2.Options
import qualified Database.EJDB2.Query   as Query

import           Plant

import           Prelude                hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (DB.open testReadOnlyDatabaseOpts) DB.close $
    \databaseIO -> testGroup "get" [ foldOnIsTreeTest databaseIO ]

testReadOnlyDatabaseOpts :: Options
testReadOnlyDatabaseOpts =
    DB.minimalOptions "./test/read-only-db" [ DB.readonlyOpenFlags ]

foldOnIsTreeTest :: IO DB.Database -> TestTree
foldOnIsTreeTest databaseIO = testCase "foldOnIsTree" $ do
    database <- databaseIO
    query <- Query.fromString "@plants/*"
    result <- DB.fold database foldOnIsTree (0, 0) query
    result @?= (1, 3)

foldOnIsTree :: (Int, Int) -> (Int64, Maybe Plant) -> (Int, Int)
foldOnIsTree acc (_, Nothing) = acc
foldOnIsTree acc@(trueCount, falseCount) (_, Just plant) =
    maybe acc
          (\flag -> if flag
                    then (trueCount + 1, falseCount)
                    else (trueCount, falseCount + 1))
          (isTree plant)
