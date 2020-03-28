module Database.EJDB2
    ( init
    , Database
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
    , mergeOrPut
    , patch
    , delete
    , ensureCollection
    , removeCollection
    , renameCollection
    , getMeta
    , IndexMode
    , uniqueIndexMode
    , strIndexMode
    , f64IndexMode
    , i64IndexMode
    , ensureIndex
    , removeIndex
    , onlineBackup
    ) where

import           Control.Exception
import           Control.Monad

import qualified Data.Aeson                              as Aeson
import qualified Data.ByteString                         as BS
import           Data.IORef
import           Data.Int
import           Data.Word

import           Database.EJDB2.Bindings.EJDB2
import           Database.EJDB2.Bindings.IW
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc   as EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec  as EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts  as EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVOpts
                 ( OpenFlags, noTrimOnCloseOpenFlags, readonlyOpenFlags
                 , truncateOpenFlags )
import           Database.EJDB2.Bindings.Types.IWKVOpts  as IWKVOpts
import           Database.EJDB2.Bindings.Types.IndexMode
                 ( IndexMode, f64IndexMode, i64IndexMode, strIndexMode
                 , uniqueIndexMode )
import qualified Database.EJDB2.Bindings.Types.IndexMode as IndexMode
import           Database.EJDB2.JBL
import           Database.EJDB2.Query

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Prelude                                 hiding ( init )

data Database = Database (Ptr EJDB) EJDB

type Options = EJDBOpts

minimalOptions :: String -> [OpenFlags] -> Options
minimalOptions path openFlags =
    EJDBOpts.zero { kv = IWKVOpts.zero { path = Just path, oflags = openFlags }
                  }

init :: IO ()
init = c_ejdb_init >>= checkRC

open :: Options -> IO Database
open opts = do
    ejdbPtr <- malloc
    alloca $ \optsPtr -> do
        poke optsPtr opts
        result <- decodeRC <$> c_ejdb_open optsPtr ejdbPtr
        if result == Ok
            then Database ejdbPtr <$> peek ejdbPtr
            else free ejdbPtr >> fail (show result)

close :: Database -> IO ()
close (Database ejdbPtr _) = do
    result <- decodeRC <$> c_ejdb_close ejdbPtr
    if result == Ok then free ejdbPtr else fail $ show result

getById :: Aeson.FromJSON a => Database -> String -> Int64 -> IO (Maybe a)
getById (Database _ ejdb) collection id = alloca $ \jblPtr ->
    finally (do
                 rc <- withCString collection $ \cCollection ->
                     c_ejdb_get ejdb cCollection (CIntMax id) jblPtr
                 let result = decodeRC rc
                 case result of
                     Ok -> peek jblPtr >>= decode
                     ErrorNotFound -> return Nothing
                     _ -> fail $ show result)
            (c_jbl_destroy jblPtr)

getCount :: Database -> Query -> IO Int64
getCount (Database _ ejdb) query = do
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
exec visitor (Database _ ejdb) query = do
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
putNew (Database _ ejdb) collection obj = encode obj $
    \doc -> withCString collection $ \cCollection -> alloca $ \idPtr ->
    c_ejdb_put_new ejdb cCollection doc idPtr >>= checkRC >> peek idPtr
    >>= \(CIntMax int) -> return int

put :: Aeson.ToJSON a => Database -> String -> a -> Int64 -> IO ()
put (Database _ ejdb) collection obj id =
    encode obj $ \doc -> withCString collection $ \cCollection ->
    c_ejdb_put ejdb cCollection doc (CIntMax id) >>= checkRC

mergeOrPut :: Aeson.ToJSON a => Database -> String -> a -> Int64 -> IO ()
mergeOrPut (Database _ ejdb) collection obj id = withCString collection $
    \cCollection -> BS.useAsCString (encodeToByteString obj) $ \jsonPatch ->
    c_ejdb_merge_or_put ejdb cCollection jsonPatch (CIntMax id) >>= checkRC

patch :: Aeson.ToJSON a => Database -> String -> a -> Int64 -> IO ()
patch (Database _ ejdb) collection obj id = withCString collection $
    \cCollection -> BS.useAsCString (encodeToByteString obj) $ \jsonPatch ->
    c_ejdb_patch ejdb cCollection jsonPatch (CIntMax id) >>= checkRC

delete :: Database -> String -> Int64 -> IO ()
delete (Database _ ejdb) collection id = withCString collection $
    \cCollection -> c_ejdb_del ejdb cCollection (CIntMax id) >>= checkRC

ensureCollection :: Database -> String -> IO ()
ensureCollection (Database _ ejdb) collection =
    withCString collection (c_ejdb_ensure_collection ejdb >=> checkRC)

removeCollection :: Database -> String -> IO ()
removeCollection (Database _ ejdb) collection =
    withCString collection (c_ejdb_remove_collection ejdb >=> checkRC)

renameCollection :: Database -> String -> String -> IO ()
renameCollection (Database _ ejdb) collection newCollection =
    withCString collection $ \cCollection ->
    withCString newCollection
                (c_ejdb_rename_collection ejdb cCollection >=> checkRC)

getMeta :: Aeson.FromJSON a => Database -> IO (Maybe a)
getMeta (Database _ ejdb) = alloca $ \jblPtr -> c_ejdb_get_meta ejdb jblPtr
    >>= checkRC >> finally (peek jblPtr >>= decode) (c_jbl_destroy jblPtr)

ensureIndex :: Database -> String -> String -> [IndexMode] -> IO ()
ensureIndex (Database _ ejdb) collection path indexMode =
    withCString collection $ \cCollection -> withCString path $
    \cPath -> c_ejdb_ensure_index ejdb cCollection cPath mode >>= checkRC
  where
    mode = IndexMode.unIndexMode $ IndexMode.combineIndexMode indexMode

removeIndex :: Database -> String -> String -> [IndexMode] -> IO ()
removeIndex (Database _ ejdb) collection path indexMode =
    withCString collection $ \cCollection -> withCString path $
    \cPath -> c_ejdb_remove_index ejdb cCollection cPath mode >>= checkRC
  where
    mode = IndexMode.unIndexMode $ IndexMode.combineIndexMode indexMode

onlineBackup :: Database -> String -> IO Word64
onlineBackup (Database _ ejdb) filePath = withCString filePath $ \cFilePath ->
    alloca $ \timestampPtr -> c_ejdb_online_backup ejdb timestampPtr cFilePath
    >>= checkRC >> peek timestampPtr >>= \(CUIntMax t) -> return t
