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
    testGroup "index" [ getMetaTest databaseIO, createIndexTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/index-db" [ truncateOpenFlags ]

getMetaTest :: IO Database -> TestTree
getMetaTest databaseIO = testCase "getMetaTest" $ do
    database <- databaseIO
    Just meta <- getMeta database
    let (major : '.' : minor : '.' : _) = Meta.version meta
    [ major, '.', minor ] @?= "2.0"

createIndexTest :: IO Database -> TestTree
createIndexTest databaseIO = testCase "createIndexTest" $ do
    database <- databaseIO
    ensureCollection database "plants"
    ensureIndex database "plants" "/name" [ uniqueIndexMode, strIndexMode ]
    Just meta <- getMeta database
    let indexes = CollectionMeta.indexes $ head $ Meta.collections meta
    length indexes @?= 1
