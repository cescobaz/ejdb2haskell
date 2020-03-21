{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings where

import           Data.ByteString.Char8

import           Database.EJDB2.Bindings.Types

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_init" c_ejdb_init :: IO IWRC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_open" c_ejdb_open
    :: Ptr EJDB_OPTS -> Ptr EJDBPtr -> IO IWRC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_close" c_ejdb_close
    :: Ptr EJDBPtr -> IO IWRC
