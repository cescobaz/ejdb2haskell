module Database.EJDB2 ( Database, open, close, getById ) where

import           Control.Monad

import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString                        as BS
import           Data.Int

import           Database.EJDB2.Bindings.EJDB2
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc  as EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec as EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVBase

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

newtype Database = Database (Ptr EJDB)

checkIWRC :: IWRC -> IO ()
checkIWRC iwrc = do
    let result = decodeResult iwrc
    if result == Ok then return () else fail $ show result

open :: EJDBOpts -> IO Database
open opts = do
    c_ejdb_init
    ejdb <- malloc
    alloca $ \optsPtr -> do
        poke optsPtr opts
        iwrc <- c_ejdb_open optsPtr ejdb
        let result = decodeResult iwrc
        if result == Ok
            then return $ Database ejdb
            else do
                free ejdb
                fail $ show result

close :: Database -> IO ()
close (Database ejdb) = do
    iwrc <- c_ejdb_close ejdb
    let result = decodeResult iwrc
    if result == Ok then free ejdb else fail $ show result

visitorPrint :: Ptr EJDBExec -> Ptr EJDBDoc -> Ptr CIntMax -> IO IWRC
visitorPrint execPtr docPtr _ = do
    putStrLn "VISITORRRR"
    return 0

-- getById :: (FromJSON a) => Database -> Int62 -> IO a
-- getById (Database ejdbPtr) id = do
--     ejdb <- peek ejdbPtr
--     alloca $ \execPtr -> 
--             let exec = EJDBExec.minimal ejdb jql visitorPrint
--             poke execPtr exec
--             c_ejdb_exec execPtr
getById :: (Aeson.FromJSON a) => Database -> String -> Int64 -> IO (Maybe a)
getById (Database ejdbPtr) collection id = do
    ejdb <- peek ejdbPtr
    cCollection <- newCString collection
    alloca $ \jblPtr -> do
        checkIWRC <$> c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
        jbl <- peek jblPtr
        jsonCString <- c_jbl_get_str jbl
        json <- BS.packCString jsonCString
        return $ Aeson.decodeStrict json
