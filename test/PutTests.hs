module PutTests ( tests ) where

import           Data.Aeson       ()

import           Database.EJDB2

import           Plant

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $
    \databaseIO -> testGroup "put" []

testDatabaseOpts :: Options
testDatabaseOpts = minimalOptions "./test/put-db" [ truncateOpenFlags ]

