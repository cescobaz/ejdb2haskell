module Database.EJDB2
    ( Database
    , open
    , close
    , getById
    , getList
    , getList'
    , minimalOptions
    , Options(..)
    , OpenFlags
    , readonlyOpenFlags
    , truncateOpenFlags
    , noTrimOnCloseOpenFlags
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
            else do
                free ejdb
                fail $ show result

close :: Database -> IO ()
close (Database ejdb) = do
    result <- decodeRC <$> c_ejdb_close ejdb
    if result == Ok then free ejdb else fail $ show result

getById :: Aeson.FromJSON a => Database -> String -> Int64 -> IO (Maybe a)
getById (Database ejdbPtr) collection id = do
    ejdb <- peek ejdbPtr
    cCollection <- newCString collection
    alloca $ \jblPtr ->
        finally (do
                     result <- decodeRC
                         <$> c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
                     case result of
                         Ok -> peek jblPtr >>= decode
                         ErrorNotfound -> return Nothing
                         _ -> fail $ show result)
                (free cCollection >> c_jbl_destroy jblPtr)

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
    let exec = EJDBExec.zero { db = ejdb, q = jql, EJDBExec.visitor = visitor }
    alloca $ \execPtr -> do
        poke execPtr exec
        c_ejdb_exec execPtr >>= (\result -> do
                                     putStrLn $ show result
                                     return result)
            >>= checkRCFinally (freeHaskellFunPtr visitor)
        reverse <$> readIORef ref
