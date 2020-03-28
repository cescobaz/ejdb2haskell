module OnlineBackupTests ( tests ) where

import           Control.Exception
import           Control.Monad

import           Data.UnixTime

import           Database.EJDB2

import           Plant

import           Prelude           hiding ( id )

import           System.Directory

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $
    \databaseIO -> testGroup "onlineBackup" [ onlineBackupTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts =
    minimalOptions "./test/onlinebackup-db" [ truncateOpenFlags ]

backupFilePath :: IO String
backupFilePath = makeAbsolute "./test/onlinebackup-backup-db"

onlineBackupTest :: IO Database -> TestTree
onlineBackupTest databaseIO = testCase "onlineBackupTest" $ do
    backupFilePath <- backupFilePath
    doesFileExist backupFilePath
        >>= \exists -> when exists $ removeFile backupFilePath
    database <- databaseIO
    id <- putNew database "plants" plant
    time <- onlineBackup database backupFilePath
    now <- getUnixTime
    putStrLn $ show time
    putStrLn $ show now
    backupFileExists <- doesFileExist backupFilePath
    backupFileExists @? "backup file exists"
    databaseLast <- open $ minimalOptions backupFilePath [ readonlyOpenFlags ]
    catch (do
               backupPlant <- getById databaseLast "plants" id
               backupPlant @?= Just plant
               close databaseLast)
          (\e -> do
               close databaseLast
               throw (e :: IOException))
  where
    plant = Plant { id          = Nothing
                  , name        = Just "pinus"
                  , isTree      = Just True
                  , year        = Just 1753
                  , description = Just "wow 🌲"
                  }

