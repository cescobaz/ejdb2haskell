{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings.JQL where

import           Database.EJDB2.Bindings.IW
import           Database.EJDB2.Bindings.Types.C.String

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JQL = Ptr ()

foreign import ccall unsafe "ejdb2/jql.h jql_create" c_jql_create
    :: Ptr JQL -> CString -> CString -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_set_bool" c_jql_set_bool
    :: JQL -> CString -> CInt -> CBool -> IO RC

foreign import ccall unsafe "ejdb2/jql.h jql_destroy" c_jql_destroy
    :: Ptr JQL -> IO ()

foreign import ccall "finalizer.h &finalizerJQL" p_finalizerJQL
    :: FunPtr (Ptr JQL -> IO ())
