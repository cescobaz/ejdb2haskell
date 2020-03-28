{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings.EJDB2 where

import           Database.EJDB2.Bindings.IW
import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.JQL
import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBDoc
import           Database.EJDB2.Bindings.Types.EJDBExec
import           Database.EJDB2.Bindings.Types.EJDBOpts

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_init" c_ejdb_init :: IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_open" c_ejdb_open
    :: Ptr EJDBOpts -> Ptr EJDB -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_close" c_ejdb_close
    :: Ptr EJDB -> IO RC

foreign import ccall "wrapper" mkEJDBExecVisitor
    :: EJDBExecVisitor -> IO EJDB_EXEC_VISITOR

foreign import ccall "ejdb2/ejdb2.h ejdb_exec" c_ejdb_exec
    :: Ptr EJDBExec -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_get" c_ejdb_get
    :: EJDB -> CString -> CIntMax -> Ptr JBL -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_count" c_ejdb_count
    :: EJDB -> JQL -> Ptr CIntMax -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_put_new" c_ejdb_put_new
    :: EJDB -> CString -> JBL -> Ptr CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_put" c_ejdb_put
    :: EJDB -> CString -> JBL -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_merge_or_put" c_ejdb_merge_or_put
    :: EJDB -> CString -> CString -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_patch" c_ejdb_patch
    :: EJDB -> CString -> CString -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_del" c_ejdb_del
    :: EJDB -> CString -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_ensure_collection" c_ejdb_ensure_collection
    :: EJDB -> CString -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_remove_collection" c_ejdb_remove_collection
    :: EJDB -> CString -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_rename_collection" c_ejdb_rename_collection
    :: EJDB -> CString -> CString -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_ensure_index" c_ejdb_ensure_index
    :: EJDB -> CString -> CString -> CUChar -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_remove_index" c_ejdb_remove_index
    :: EJDB -> CString -> CString -> CUChar -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_get_meta" c_ejdb_get_meta
    :: EJDB -> Ptr JBL -> IO RC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_online_backup" c_ejdb_online_backup
    :: EJDB -> CString -> Ptr CUIntMax -> IO RC
