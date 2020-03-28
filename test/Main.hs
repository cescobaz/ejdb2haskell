module Main ( main ) where

import           CollectionTests

import           Database.EJDB2

import           DeleteTests

import           GetTests

import           IndexTests

import           OnlineBackupTests

import           Prelude           hiding ( init )

import           PutTests

import           Test.Tasty

main :: IO ()
main = init >> defaultMain Main.tests

tests :: TestTree
tests = testGroup "ejdb2"
                  [ GetTests.tests
                  , PutTests.tests
                  , DeleteTests.tests
                  , CollectionTests.tests
                  , IndexTests.tests
                  , OnlineBackupTests.tests
                  ]
