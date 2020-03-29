{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBOpts
        ( Options(..), zero, OptionsB, options, build ) where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import qualified Database.EJDB2.Bindings.Types.KV as KV
import qualified Database.EJDB2.Bindings.Types.EJDBHttp as EJDBHttp

#include <ejdb2/ejdb2.h>

data Options = Options { kv :: !KV.KVOptions -- ^ IWKV storage options
                       , http :: !EJDBHttp.EJDBHttp -- ^ HTTP/Websocket server options
                       , noWal :: !Bool -- ^ Do not use write-ahead-log. Default: false
                       , sortBufferSz :: !Word32 -- ^ Max sorting buffer size. If exceeded an overflow temp file for sorted data will created. Default 16Mb, min: 1Mb
                       , documentBufferSz :: !Word32 -- ^ Initial size of buffer in bytes used to process/store document during query execution. Default 64Kb, min: 16Kb
                       }

zero :: Options
zero = Options { kv = KV.zero
               , http = EJDBHttp.zero
               , noWal = False
               , sortBufferSz = 0
               , documentBufferSz = 0
               }

data OptionsB = OptionsB { options :: Options
                         , kvB :: !KV.KVOptionsB
                         }

build :: Options -> IO OptionsB
build options = do
        kvB <- KV.build (kv options)
        return $ OptionsB options kvB

instance Storable OptionsB where
        sizeOf _ = #{size EJDB_OPTS}
        alignment _ = #{alignment EJDB_OPTS}
        peek ptr = do
           kvB <- #{peek EJDB_OPTS, kv} ptr
           let kv = KV.options kvB
           http <- #{peek EJDB_OPTS, http} ptr
           (CBool no_wal) <- #{peek EJDB_OPTS, no_wal} ptr
           (CUInt sort_buffer_sz) <- #{peek EJDB_OPTS, sort_buffer_sz} ptr
           (CUInt document_buffer_sz) <- #{peek EJDB_OPTS, document_buffer_sz} ptr
           return $ OptionsB (Options kv http (toBool no_wal) sort_buffer_sz document_buffer_sz) kvB
        poke ptr (OptionsB (Options _ http noWal sort_buffer_sz document_buffer_sz) kvB) = do
           #{poke EJDB_OPTS, kv} ptr kvB
           #{poke EJDB_OPTS, http} ptr http
           #{poke EJDB_OPTS, no_wal} ptr (CBool $ fromBool noWal)
           #{poke EJDB_OPTS, sort_buffer_sz} ptr (CUInt sort_buffer_sz)
           #{poke EJDB_OPTS, document_buffer_sz} ptr (CUInt document_buffer_sz)

