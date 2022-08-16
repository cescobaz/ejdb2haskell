{-# LANGUAGE CPP #-}

module Database.EJDB2.Options
        ( Options(..), zero, OptionsB, options, build ) where

import           Foreign
import           Foreign.C.Types

import qualified Database.EJDB2.KV                as KV
import qualified Database.EJDB2.HTTP              as HTTP

#include <ejdb2/ejdb2.h>

-- | EJDB open options
data Options = Options { kv :: !KV.Options -- ^ IWKV storage options
                       , http :: !HTTP.Options -- ^ HTTP/Websocket server options
                       , noWal :: !Bool -- ^ Do not use write-ahead-log. Default: false
                       , sortBufferSz :: !Word32 -- ^ Max sorting buffer size. If exceeded an overflow temp file for sorted data will created. Default 16Mb, min: 1Mb
                       , documentBufferSz :: !Word32 -- ^ Initial size of buffer in bytes used to process/store document during query execution. Default 64Kb, min: 16Kb
                       }

-- | Create default Options
zero :: Options
zero = Options { kv = KV.zero
               , http = HTTP.zero
               , noWal = False
               , sortBufferSz = 0
               , documentBufferSz = 0
               }

-- | Storable version of 'Options'
data OptionsB = OptionsB { options :: Options
                         , kvB :: !KV.OptionsB
                         , httpB :: !HTTP.OptionsB
                         }

-- | Create Storable version of 'Options'
build :: Options -> IO OptionsB
build options = do
        kvB <- KV.build (kv options)
        httpB <- HTTP.build (http options)
        return $ OptionsB options kvB httpB

instance Storable OptionsB where
        sizeOf _ = #{size EJDB_OPTS}
        alignment _ = #{alignment EJDB_OPTS}
        peek ptr = do
           kvB <- #{peek EJDB_OPTS, kv} ptr
           let kv = KV.options kvB
           httpB <- #{peek EJDB_OPTS, http} ptr
           let http = HTTP.options httpB
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

