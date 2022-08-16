module OnlineBackupTests ( tests ) where

import           Control.Exception
import           Control.Monad

import           Database.EJDB2
import           Database.EJDB2.Options

import           Plant

import           Prelude                hiding ( id )

import           System.Directory

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = withResource (open testDatabaseOpts) close $
    \databaseIO -> testGroup "onlineBackup" [ onlineBackupTest databaseIO ]

testDatabaseOpts :: Options
testDatabaseOpts =
    minimalOptions "./test/onlinebackup-db" [ truncateOpenFlags ]

backupFilePath :: String
backupFilePath = "./test/onlinebackup-backup-db"

onlineBackupTest :: IO Database -> TestTree
onlineBackupTest databaseIO = testCase "onlineBackupTest" $ do
    doesFileExist backupFilePath
        >>= \exists -> when exists $ removeFile backupFilePath
    database <- databaseIO
    id <- putNew database "plants" plant
    _ <- onlineBackup database backupFilePath
    backupFileExists <- doesFileExist backupFilePath
    backupFileExists @? "backup file exists"
    databaseLast <- open $ minimalOptions backupFilePath []
    catch (do
               backupPlant <- getById databaseLast "plants" id
               backupPlant @?= Just plant
               close databaseLast)
          (\e -> do
               close databaseLast
               throw (e :: IOException))
  where
    plant = nothingPlant { id          = Nothing
                         , name        = Just "pinus"
                         , isTree      = Just True
                         , year        = Just 1753
                         , description = Just "wow 🌲"
                         }

