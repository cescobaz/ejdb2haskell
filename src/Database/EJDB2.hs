module Database.EJDB2
    ( init
    , Database
    , Options(..)
    , KV.KVOptions(..)
    , KV.OpenFlags
    , KV.readonlyOpenFlags
    , KV.truncateOpenFlags
    , KV.noTrimOnCloseOpenFlags
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
import           Database.EJDB2.Bindings.Types.IndexMode
                 ( IndexMode, f64IndexMode, i64IndexMode, strIndexMode
                 , uniqueIndexMode )
import qualified Database.EJDB2.Bindings.Types.IndexMode as IndexMode
import qualified Database.EJDB2.Bindings.Types.KV        as KV
                 ( KVOptions(..), OpenFlags, noTrimOnCloseOpenFlags
                 , readonlyOpenFlags, truncateOpenFlags )
import qualified Database.EJDB2.Bindings.Types.KV        as KV
import           Database.EJDB2.JBL
import           Database.EJDB2.Query

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Prelude                                 hiding ( init )

-- | Reference to database. You can create it by 'open'.
data Database = Database (Ptr EJDB) EJDB

-- | Create minimal 'Options' for opening a database: just path to file and opening mode.
minimalOptions :: String -- ^ Database file path 
               -> [KV.OpenFlags] -- ^ Open mode
               -> Options -- ^ Options to use in 'open'

minimalOptions path openFlags =
    EJDBOpts.zero { kv = KV.zero { KV.path = Just path, KV.oflags = openFlags }
                  }

{-|
  ejdb2 initialization routine.

  /Must be called before using any of ejdb API function./
-}
init :: IO ()
init = c_ejdb_init >>= checkRC

{-|
  Open storage file.

  Storage can be opened only by one single process at time.

  /Remember to release database by 'close' when database is no longer required./
-}
open :: Options -> IO Database
open opts = do
    ejdbPtr <- malloc
    optsB <- build opts
    with optsB $ \optsPtr -> do
        result <- decodeRC <$> c_ejdb_open optsPtr ejdbPtr
        if result == Ok
            then Database ejdbPtr <$> peek ejdbPtr
            else free ejdbPtr >> fail (show result)

{-|
  Closes storage and frees up all resources.
-}
close :: Database -> IO ()
close (Database ejdbPtr _) = do
    result <- decodeRC <$> c_ejdb_close ejdbPtr
    if result == Ok then free ejdbPtr else fail $ show result

-- | Retrieve document identified by given id from collection.
getById :: Aeson.FromJSON a
        => Database
        -> String -- ^ Collection name
        -> Int64 -- ^ Document identifier. Not zero
        -> IO (Maybe a)
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

-- | Executes a given query and returns the number of documents.
getCount :: Database -> Query -> IO Int64
getCount (Database _ ejdb) (Query jql _) = alloca $
    \countPtr -> c_ejdb_count ejdb jql countPtr 0 >>= checkRC >> peek countPtr
    >>= \(CIntMax int) -> return int

{-|
  Executes a given query and builds a query result as list of tuple with id and document.
-}
getList :: Aeson.FromJSON a => Database -> Query -> IO [(Int64, Maybe a)]
getList = exec Database.EJDB2.visitor

visitor :: Aeson.FromJSON a => IORef [(Int64, Maybe a)] -> EJDBExecVisitor
visitor ref _ docPtr _ = do
    doc <- peek docPtr
    value <- decode (raw doc)
    modifyIORef' ref $ \list -> (fromIntegral $ EJDBDoc.id doc, value) : list
    return 0

{-|
  Executes a given query and builds a query result as list of documents with id injected as attribute.
-}
getList' :: Aeson.FromJSON a => Database -> Query -> IO [Maybe a]
getList' = exec Database.EJDB2.visitor'

visitor' :: Aeson.FromJSON a => IORef [Maybe a] -> EJDBExecVisitor
visitor' ref _ docPtr _ = do
    doc <- peek docPtr
    value <- decode' (raw doc) (fromIntegral $ EJDBDoc.id doc)
    modifyIORef' ref $ \list -> value : list
    return 0

exec :: (IORef [a] -> EJDBExecVisitor) -> Database -> Query -> IO [a]
exec visitor (Database _ ejdb) (Query jql _) = do
    ref <- newIORef []
    visitor <- mkEJDBExecVisitor (visitor ref)
    let exec = EJDBExec.zero { db = ejdb, q = jql, EJDBExec.visitor = visitor }
    finally (with exec $ \execPtr -> do
                 c_ejdb_exec execPtr >>= checkRC
                 reverse <$> readIORef ref)
            (freeHaskellFunPtr visitor)

{-|
  Save new document into collection under new generated identifier.
-}
putNew :: Aeson.ToJSON a
       => Database
       -> String -- ^ Collection name
       -> a -- ^ Document
       -> IO Int64 -- ^ New document identifier. Not zero

putNew (Database _ ejdb) collection obj = encode obj $
    \doc -> withCString collection $ \cCollection -> alloca $ \idPtr ->
    c_ejdb_put_new ejdb cCollection doc idPtr >>= checkRC >> peek idPtr
    >>= \(CIntMax int) -> return int

{-|
  Save a given document under specified id.
-}
put :: Aeson.ToJSON a
    => Database
    -> String -- ^ Collection name
    -> a -- ^ Document
    -> Int64 -- ^ Document identifier. Not zero
    -> IO ()
put (Database _ ejdb) collection obj id =
    encode obj $ \doc -> withCString collection $ \cCollection ->
    c_ejdb_put ejdb cCollection doc (CIntMax id) >>= checkRC

{-|
  Apply JSON merge patch (rfc7396) to the document identified by id or insert new document under specified id.

  /This is an atomic operation./
-}
mergeOrPut :: Aeson.ToJSON a
           => Database
           -> String -- ^ Collection name
           -> a -- ^ JSON merge patch conformed to rfc7396 specification
           -> Int64 -- ^ Document identifier. Not zero
           -> IO ()
mergeOrPut (Database _ ejdb) collection obj id = withCString collection $
    \cCollection -> BS.useAsCString (encodeToByteString obj) $ \jsonPatch ->
    c_ejdb_merge_or_put ejdb cCollection jsonPatch (CIntMax id) >>= checkRC

{-|
  Apply rfc6902\/rfc7396 JSON patch to the document identified by id.
-}
patch :: Aeson.ToJSON a
      => Database
      -> String -- ^ Collection name
      -> a -- ^ JSON patch conformed to rfc6902 or rfc7396 specification
      -> Int64 -- ^ Document identifier. Not zero
      -> IO ()
patch (Database _ ejdb) collection obj id = withCString collection $
    \cCollection -> BS.useAsCString (encodeToByteString obj) $ \jsonPatch ->
    c_ejdb_patch ejdb cCollection jsonPatch (CIntMax id) >>= checkRC

{-|
   Remove document identified by given id from collection coll.
-}
delete :: Database
       -> String -- ^ Collection name
       -> Int64 -- ^ Document identifier. Not zero
       -> IO ()
delete (Database _ ejdb) collection id = withCString collection $
    \cCollection -> c_ejdb_del ejdb cCollection (CIntMax id) >>= checkRC

{-|
  Create collection with given name if it has not existed before
-}
ensureCollection :: Database
                 -> String -- ^ Collection name
                 -> IO ()
ensureCollection (Database _ ejdb) collection =
    withCString collection (c_ejdb_ensure_collection ejdb >=> checkRC)

{-|
  Remove collection under the given name.
-}
removeCollection :: Database
                 -> String -- ^ Collection name
                 -> IO ()
removeCollection (Database _ ejdb) collection =
    withCString collection (c_ejdb_remove_collection ejdb >=> checkRC)

{-|
  Rename collection to new name.
-}
renameCollection :: Database
                 -> String -- ^ Old collection name
                 -> String -- ^ New collection name
                 -> IO ()
renameCollection (Database _ ejdb) collection newCollection =
    withCString collection $ \cCollection ->
    withCString newCollection
                (c_ejdb_rename_collection ejdb cCollection >=> checkRC)

{-|
  Returns JSON document describing database structure. You can use the convenient data 'Database.EJDB2.Meta.Meta'
-}
getMeta :: Aeson.FromJSON a
        => Database
        -> IO (Maybe a) -- ^ JSON object describing ejdb storage. See data 'Database.EJDB2.Meta.Meta'

getMeta (Database _ ejdb) = alloca $ \jblPtr -> c_ejdb_get_meta ejdb jblPtr
    >>= checkRC >> finally (peek jblPtr >>= decode) (c_jbl_destroy jblPtr)

{-|
  Create index with specified parameters if it has not existed before.

  /Index path must be fully specified as rfc6901 JSON pointer and must not countain unspecified *\/** element in middle sections./

  > ensureIndex database "mycoll" "/address/street" [uniqueIndexMode | strIndexMode]
-}
ensureIndex :: Database
            -> String -- ^ Collection name
            -> String -- ^ rfc6901 JSON pointer to indexed field
            -> [IndexMode] -- ^ Index mode
            -> IO ()
ensureIndex (Database _ ejdb) collection path indexMode =
    withCString collection $ \cCollection -> withCString path $
    \cPath -> c_ejdb_ensure_index ejdb cCollection cPath mode >>= checkRC
  where
    mode = IndexMode.unIndexMode $ IndexMode.combineIndexMode indexMode

{-|
  Remove index if it has existed before.
-}
removeIndex :: Database
            -> String -- ^ Collection name
            -> String -- ^ rfc6901 JSON pointer to indexed field
            -> [IndexMode] -- ^ Index mode
            -> IO ()
removeIndex (Database _ ejdb) collection path indexMode =
    withCString collection $ \cCollection -> withCString path $
    \cPath -> c_ejdb_remove_index ejdb cCollection cPath mode >>= checkRC
  where
    mode = IndexMode.unIndexMode $ IndexMode.combineIndexMode indexMode

{-|
  Creates an online database backup image and copies it into the specified target file.
  During online backup phase read/write database operations are allowed and not
  blocked for significant amount of time. Backup finish time is placed into result
  as number of milliseconds since epoch.

  Online backup guaranties what all records before timestamp will
  be stored in backup image. Later, online backup image can be
  opened as ordinary database file.

  /In order to avoid deadlocks: close all opened database cursors before calling this method or do call in separate thread./
-}
onlineBackup :: Database
             -> String -- ^ Backup file path
             -> IO Word64 -- ^ Backup completion timestamp

onlineBackup (Database _ ejdb) filePath = withCString filePath $ \cFilePath ->
    alloca $ \timestampPtr -> c_ejdb_online_backup ejdb timestampPtr cFilePath
    >>= checkRC >> peek timestampPtr >>= \(CUIntMax t) -> return t
