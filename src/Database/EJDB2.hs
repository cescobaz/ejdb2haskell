module Database.EJDB2
    ( Database
    , open
    , close
    , getById
    , minimalOptions
    , Options(..)
    ) where

import           Control.Monad

import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy                   as BS
import           Data.Char
import           Data.IORef
import           Data.Int

import           Database.EJDB2.Bindings.EJDB2
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc  as EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec as EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts as EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVBase
import           Database.EJDB2.Bindings.Types.IWKVOpts as IWKVOpts

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

newtype Database = Database (Ptr EJDB)

type Options = EJDBOpts

minimalOptions :: String -> Options
minimalOptions path = EJDBOpts.zero { kv = IWKVOpts.zero { path = Just path } }

checkIWRC :: IWRC -> IO ()
checkIWRC iwrc = do
    putStrLn "oooh checkIWRC"
    putStrLn $ show iwrc
    let result = decodeResult iwrc
    if result == Ok then return () else fail $ show result

checkIWRCFinally :: IO a -> IWRC -> IO a
checkIWRCFinally computation iwrc = do
    putStrLn "oooh checkIWRCFinally"
    let result = decodeResult iwrc
    if result == Ok
        then computation
        else do
            computation
            fail $ show result

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
    alloca $ \jblPtr -> do
        putStrLn "GET BY IDDDDDDD"
        c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
            >>= (checkIWRCFinally (free cCollection))
        decodeJBLPtr jblPtr

printer :: IORef BS.ByteString -> JBLJSONPrinter
printer ref d size (CChar ch) count op = do
    modifyIORef' ref $ \string -> BS.cons word string
    return 0
  where
    word = fromIntegral ch

decodeJBL :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decodeJBL jbl = do
    ref <- newIORef BS.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    iwrc <- c_jbl_as_json jbl thePrinter nullPtr 0
    checkIWRCFinally (freeHaskellFunPtr thePrinter) iwrc
    string <- readIORef ref
    return $ Aeson.decode (BS.reverse string)

decodeJBLPtr :: Aeson.FromJSON a => Ptr JBL -> IO (Maybe a)
decodeJBLPtr jblPtr = peek jblPtr >>= decodeJBL
