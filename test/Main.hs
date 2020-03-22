module Main ( main ) where

import           Database.EJDB2

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain Main.tests

tests :: TestTree
tests = TestGroup "ejdb2" []
