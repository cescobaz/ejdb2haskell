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
