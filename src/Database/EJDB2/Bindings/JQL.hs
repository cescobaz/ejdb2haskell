{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings.JQL where

import           Data.ByteString.Char8

import           Database.EJDB2.Bindings.Types.C.String
import           Database.EJDB2.Bindings.Types.IWKVBase

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JQL = Ptr ()

foreign import ccall unsafe "ejdb2/jql.h jql_create" c_jql_create
    :: JQL -> CString -> CString -> IO IWRC

foreign import ccall unsafe "ejdb2/jql.h jql_destroy" c_jql_destroy
    :: JQL -> IO ()