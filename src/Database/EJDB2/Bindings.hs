{-# LANGUAGE ForeignFunctionInterface #-}

module Database.EJDB2.Bindings where

import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.Types.EJDBOpts
import           Database.EJDB2.Bindings.Types.IWKVBase

import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_init" c_ejdb_init :: IO IWRC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_open" c_ejdb_open
    :: Ptr EJDBOpts -> Ptr EJDB -> IO IWRC

foreign import ccall unsafe "ejdb2/ejdb2.h ejdb_close" c_ejdb_close
    :: Ptr EJDB -> IO IWRC
