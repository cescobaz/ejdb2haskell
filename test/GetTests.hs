module GetTests (tests) where

import           Database.EJDB2
import           Database.EJDB2.Bindings.Types.EJDBOpts

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = TestGroup "get" []

testReadOnlyDatabaseOpts :: EJDBOpts
testReadOnlyDatabaseOpts = zero { kv = (kv zero) { path "./test/read-only-db" } }

getByIdAndParseToMap :: TestTree
getByIdAndParseToMap = withResource (open testReadOnlyDatabaseOpts) (close) $ 
