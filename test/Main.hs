module Main ( main ) where

import           Database.EJDB2

import           GetTests

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = testGroup "ejdb2" [ GetTests.tests ]
