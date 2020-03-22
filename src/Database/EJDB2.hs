module Database.EJDB2 ( Database, open, close ) where

import           Control.Monad

import           Database.EJDB2.Bindings
import           Database.EJDB2.Bindings.Types
import           Database.EJDB2.Bindings.Types.EJDBHttp    as EJDBHttp
import           Database.EJDB2.Bindings.Types.EJDBOpts    as EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVBase
import           Database.EJDB2.Bindings.Types.IWKVOpts    as IWKVOpts
import           Database.EJDB2.Bindings.Types.IWKVWalOpts as IWKVWalOpts

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

newtype Database = Database (Ptr EJDBPtr)

open :: EJDBOpts -> IO Database
open opts = do
    c_ejdb_init
    ejdb <- malloc
    alloca $ \optsPtr -> do
        poke optsPtr opts
        iwrc <- c_ejdb_open optsPtr ejdb
        let result = decodeResult iwrc
        if (result == Ok)
            then return $ Database ejdb
            else do
                free ejdb
                fail $ show result

close :: Database -> IO ()
close (Database ejdb) = do
    iwrc <- c_ejdb_close ejdb
    let result = decodeResult iwrc
    if (result == Ok) then free ejdb else fail $ show result
