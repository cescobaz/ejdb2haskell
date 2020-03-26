module Main ( main ) where

import           CollectionTests

import           DeleteTests

import           GetTests

import           PutTests

import           Test.Tasty

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "ejdb2"
                  [ GetTests.tests
                  , PutTests.tests
                  , DeleteTests.tests
                  , CollectionTests.tests
                  ]
