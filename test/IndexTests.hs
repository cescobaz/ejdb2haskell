module IndexTests ( tests ) where

import           Asserts

import qualified Data.Aeson                    as Aeson

import           Database.EJDB2
import qualified Database.EJDB2.CollectionMeta as CollectionMeta
import qualified Database.EJDB2.Meta           as Meta

import           Plant

import           Prelude                       hiding ( id )

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $ \databaseIO ->
    testGroup "index"
              [ getMetaTest databaseIO, createAndRemoveIndexTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/index-db" [ truncateOpenFlags ]

getMetaTest :: IO Database -> TestTree
getMetaTest databaseIO = testCase "getMetaTest" $ do
    database <- databaseIO
    Just meta <- getMeta database
    let (major : '.' : minor : '.' : _) = Meta.version meta
    [ major, '.', minor ] @?= "2.0"

createAndRemoveIndexTest :: IO Database -> TestTree
createAndRemoveIndexTest databaseIO = testCase "createAndRemoveIndexTest" $ do
    database <- databaseIO
    ensureCollection database "plants"
    ensureIndex database "plants" "/name" mode
    indexesCount <- getIndexesCount database
    indexesCount @?= 1
    removeIndex database "plants" "/name" mode
    indexesCountLast <- getIndexesCount database
    indexesCountLast @?= 0
  where
    mode = [ uniqueIndexMode, strIndexMode ]

getIndexesCount :: Database -> IO Int
getIndexesCount database = do
    Just meta <- getMeta database
    let indexes = CollectionMeta.indexes $ head $ Meta.collections meta
    return $ length indexes

