module Database.EJDB2 where

import           Control.Monad

import           Database.EJDB2.Bindings
import           Database.EJDB2.Bindings.Types

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

open :: EJDB_OPTS -> IO ()
open opts = do
    c_ejdb_init
    ejdb <- malloc
    optsPtr <- calloc
    poke optsPtr opts
    rc <- c_ejdb_open optsPtr ejdb
    free optsPtr
    return ()

close :: Ptr EJDBPtr -> IO ()
close ejdb = void $ c_ejdb_close ejdb
