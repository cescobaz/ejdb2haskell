{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBOpts where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import Database.EJDB2.Bindings.Types.IWKVOpts as IWKVOpts
import Database.EJDB2.Bindings.Types.EJDBHttp as EJDBHttp

#include <ejdb2/ejdb2.h>

data EJDBOpts = EJDBOpts { kv :: !IWKVOpts
                         , http :: !EJDBHttp
                         , noWal :: !CBool
                         , sortBufferSz :: !CSize
                         , documentBufferSz :: !CSize }
instance Storable EJDBOpts where
        sizeOf _ = #{size EJDB_OPTS}
        alignment _ = #{alignment EJDB_OPTS}
        peek ptr = do
           kv <- #{peek EJDB_OPTS, kv} ptr
           http <- #{peek EJDB_OPTS, http} ptr
           no_wal <- #{peek EJDB_OPTS, no_wal} ptr
           sort_buffer_sz <- #{peek EJDB_OPTS, sort_buffer_sz} ptr
           document_buffer_sz <- #{peek EJDB_OPTS, document_buffer_sz} ptr
           return $ EJDBOpts kv http no_wal sort_buffer_sz document_buffer_sz
        poke ptr (EJDBOpts kv http no_wal sort_buffer_sz document_buffer_sz) = do
           #{poke EJDB_OPTS, kv} ptr kv
           #{poke EJDB_OPTS, http} ptr http
           #{poke EJDB_OPTS, no_wal} ptr no_wal
           #{poke EJDB_OPTS, sort_buffer_sz} ptr sort_buffer_sz
           #{poke EJDB_OPTS, document_buffer_sz} ptr document_buffer_sz

zero :: EJDBOpts
zero = EJDBOpts { kv = IWKVOpts.zero
                        , http = EJDBHttp.zero
                        , noWal = 0
                        , sortBufferSz = 0
                        , documentBufferSz = 0
                        }
