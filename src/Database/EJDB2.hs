module Database.EJDB2
    ( Database
    , Options(..)
    , OpenFlags
    , readonlyOpenFlags
    , truncateOpenFlags
    , noTrimOnCloseOpenFlags
    , minimalOptions
    , open
    , close
    , getById
    , getCount
    , getList
    , getList'
    , putNew
    , put
    ) where

import           Control.Exception
import           Control.Monad

import qualified Data.Aeson                             as Aeson
import           Data.IORef
import           Data.Int

import           Database.EJDB2.Bindings.EJDB2
import           Database.EJDB2.Bindings.IW
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc  as EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec as EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts as EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVOpts
                 ( OpenFlags, noTrimOnCloseOpenFlags, readonlyOpenFlags
                 , truncateOpenFlags )
import           Database.EJDB2.Bindings.Types.IWKVOpts as IWKVOpts
import           Database.EJDB2.JBL
import           Database.EJDB2.Query

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

newtype Database = Database (Ptr EJDB)

type Options = EJDBOpts

minimalOptions :: String -> [OpenFlags] -> Options
minimalOptions path openFlags =
    EJDBOpts.zero { kv = IWKVOpts.zero { path = Just path, oflags = openFlags }
                  }

open :: Options -> IO Database
open opts = do
    c_ejdb_init
    ejdb <- malloc
    alloca $ \optsPtr -> do
        poke optsPtr opts
        result <- decodeRC <$> c_ejdb_open optsPtr ejdb
        if result == Ok
            then return $ Database ejdb
            else free ejdb >> fail (show result)

close :: Database -> IO ()
close (Database ejdb) = do
    result <- decodeRC <$> c_ejdb_close ejdb
    if result == Ok then free ejdb else fail $ show result

getById :: Aeson.FromJSON a => Database -> String -> Int64 -> IO (Maybe a)
getById (Database ejdbPtr) collection id = do
    ejdb <- peek ejdbPtr
    alloca $ \jblPtr ->
        finally (do
                     rc <- withCString collection $ \cCollection ->
                         c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
                     let result = decodeRC rc
                     case result of
                         Ok -> peek jblPtr >>= decode
                         ErrorNotfound -> return Nothing
                         _ -> fail $ show result)
                (c_jbl_destroy jblPtr)

getCount :: Database -> Query -> IO Int64
getCount (Database ejdbPtr) query = do
    ejdb <- peek ejdbPtr
    jql <- peek query
    alloca $ \countPtr -> c_ejdb_count ejdb jql countPtr 0 >>= checkRC
        >> peek countPtr >>= \(CIntMax int) -> return int

getList :: Aeson.FromJSON a => Database -> Query -> IO [(Int64, Maybe a)]
getList = exec Database.EJDB2.visitor

visitor :: Aeson.FromJSON a => IORef [(Int64, Maybe a)] -> EJDBExecVisitor
visitor ref _ docPtr _ = do
    doc <- peek docPtr
    value <- decode (raw doc)
    modifyIORef' ref $ \list -> (fromIntegral $ EJDBDoc.id doc, value) : list
    return 0

getList' :: Aeson.FromJSON a => Database -> Query -> IO [Maybe a]
getList' = exec Database.EJDB2.visitor'

visitor' :: Aeson.FromJSON a => IORef [Maybe a] -> EJDBExecVisitor
visitor' ref _ docPtr _ = do
    doc <- peek docPtr
    value <- decode' (raw doc) (fromIntegral $ EJDBDoc.id doc)
    modifyIORef' ref $ \list -> value : list
    return 0

exec :: (IORef [a] -> EJDBExecVisitor) -> Database -> Query -> IO [a]
exec visitor (Database ejdbPtr) query = do
    ejdb <- peek ejdbPtr
    jql <- peek query
    ref <- newIORef []
    visitor <- mkEJDBExecVisitor (visitor ref)
    finally (alloca $ \execPtr -> do
                 let exec = EJDBExec.zero { db = ejdb
                                          , q = jql
                                          , EJDBExec.visitor = visitor
                                          }
                 poke execPtr exec
                 c_ejdb_exec execPtr >>= checkRC
                 reverse <$> readIORef ref)
            (freeHaskellFunPtr visitor)

putNew :: Aeson.ToJSON a => Database -> String -> a -> IO Int64
putNew (Database ejdbPtr) collection obj = do
    ejdb <- peek ejdbPtr
    encode obj $ \doc -> withCString collection $ \cCollection ->
        alloca $ \idPtr -> c_ejdb_put_new ejdb cCollection doc idPtr >>= checkRC
        >> peek idPtr >>= \(CIntMax int) -> return int

put :: Aeson.ToJSON a => Database -> String -> a -> Int64 -> IO ()
put (Database ejdbPtr) collection obj id = do
    ejdb <- peek ejdbPtr
    encode obj $ \doc -> withCString collection $ \cCollection ->
        c_ejdb_put ejdb cCollection doc (CIntMax id) >>= checkRC
