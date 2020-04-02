{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings.JQL where

import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JQL = Ptr ()

foreign import ccall unsafe "ejdb2/jql.h jql_create" c_jql_create
    :: Ptr JQL -> CString -> CString -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_set_bool" c_jql_set_bool
    :: JQL -> CString -> CInt -> CBool -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_set_i64" c_jql_set_i64
    :: JQL -> CString -> CInt -> CIntMax -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_set_str" c_jql_set_str
    :: JQL -> CString -> CInt -> CString -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_destroy" c_jql_destroy
    :: Ptr JQL -> IO ()

foreign import ccall "finalizer.h &finalizerJQL" p_finalizerJQL
    :: FunPtr (Ptr JQL -> IO ())
