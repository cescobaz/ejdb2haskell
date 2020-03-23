module Database.EJDB2
    ( Database
    , open
    , close
    , getById
    , minimalOptions
    , Options(..)
    ) where

import           Control.Exception
import           Control.Monad

import qualified Data.Aeson                             as Aeson
import           Data.Int

import           Database.EJDB2.Bindings.EJDB2
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc  as EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec as EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts as EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVBase
import           Database.EJDB2.Bindings.Types.IWKVOpts as IWKVOpts
import           Database.EJDB2.IWKV
import           Database.EJDB2.JBL

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

newtype Database = Database (Ptr EJDB)

type Options = EJDBOpts

minimalOptions :: String -> Options
minimalOptions path = EJDBOpts.zero { kv = IWKVOpts.zero { path = Just path } }

open :: Options -> IO Database
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

getById :: (Aeson.FromJSON a) => Database -> String -> Int64 -> IO (Maybe a)
getById (Database ejdbPtr) collection id = do
    ejdb <- peek ejdbPtr
    cCollection <- newCString collection
    alloca $ \jblPtr ->
        finally (do
                     result <- c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
                         >>= return . decodeResult
                     case result of
                         Ok -> decodeJBLPtr jblPtr
                         ErrorNotfound -> return Nothing
                         _ -> fail $ show result)
                (free cCollection >> c_jbl_destroy jblPtr)
