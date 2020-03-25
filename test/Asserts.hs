module Asserts ( assertException ) where

import           Control.Exception
import           Control.Monad

import           Test.Tasty.HUnit

-- https://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
assertException :: (Exception e, Eq e, Show a) => e -> IO a -> IO ()
assertException ex action = handleJust isWanted (const $ return ()) $ do
    a <- action
    assertFailure $ "Expected exception: " ++ show ex
        ++ ", but got execution success: " ++ show a
  where
    isWanted = guard . (== ex)

