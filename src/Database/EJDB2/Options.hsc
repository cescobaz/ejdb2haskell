{-# LANGUAGE CPP #-}

module Database.EJDB2.Options
        ( Options(..), zero, OptionsB, options, build ) where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import qualified Database.EJDB2.Bindings.Types.KV as KV
import qualified Database.EJDB2.HTTPOptions       as HTTPOptions

#include <ejdb2/ejdb2.h>

data Options = Options { kv :: !KV.KVOptions -- ^ IWKV storage options
                       , http :: !HTTPOptions.HTTPOptions -- ^ HTTP/Websocket server options
                       , noWal :: !Bool -- ^ Do not use write-ahead-log. Default: false
                       , sortBufferSz :: !Word32 -- ^ Max sorting buffer size. If exceeded an overflow temp file for sorted data will created. Default 16Mb, min: 1Mb
                       , documentBufferSz :: !Word32 -- ^ Initial size of buffer in bytes used to process/store document during query execution. Default 64Kb, min: 16Kb
                       }

zero :: Options
zero = Options { kv = KV.zero
               , http = HTTPOptions.zero
               , noWal = False
               , sortBufferSz = 0
               , documentBufferSz = 0
               }

data OptionsB = OptionsB { options :: Options
                         , kvB :: !KV.KVOptionsB
                         , httpB :: !HTTPOptions.HTTPOptionsB
                         }

build :: Options -> IO OptionsB
build options = do
        kvB <- KV.build (kv options)
        httpB <- HTTPOptions.build (http options)
        return $ OptionsB options kvB httpB

instance Storable OptionsB where
        sizeOf _ = #{size EJDB_OPTS}
        alignment _ = #{alignment EJDB_OPTS}
        peek ptr = do
           kvB <- #{peek EJDB_OPTS, kv} ptr
           let kv = KV.options kvB
           httpB <- #{peek EJDB_OPTS, http} ptr
           let http = HTTPOptions.options httpB
           (CBool no_wal) <- #{peek EJDB_OPTS, no_wal} ptr
           (CUInt sort_buffer_sz) <- #{peek EJDB_OPTS, sort_buffer_sz} ptr
           (CUInt document_buffer_sz) <- #{peek EJDB_OPTS, document_buffer_sz} ptr
           return $ OptionsB (Options kv http (toBool no_wal) sort_buffer_sz document_buffer_sz) kvB httpB
        poke ptr (OptionsB (Options _ http noWal sort_buffer_sz document_buffer_sz) kvB httpB) = do
           #{poke EJDB_OPTS, kv} ptr kvB
           #{poke EJDB_OPTS, http} ptr httpB
           #{poke EJDB_OPTS, no_wal} ptr (CBool $ fromBool noWal)
           #{poke EJDB_OPTS, sort_buffer_sz} ptr (CUInt sort_buffer_sz)
           #{poke EJDB_OPTS, document_buffer_sz} ptr (CUInt document_buffer_sz)
