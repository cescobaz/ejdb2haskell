module Main ( main ) where

import           Database.EJDB2

import           DeleteTests

import           GetTests

import           PutTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "ejdb2" [ GetTests.tests, PutTests.tests, DeleteTests.tests ]
