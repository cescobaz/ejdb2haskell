{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.IWKVWalOpts where


import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import Database.EJDB2.Bindings.IW

#include <ejdb2/ejdb2.h>

data IWKVWalOpts = IWKVWalOpts { enabled :: !CBool
                               , checkCRCOnCheckpoint :: !CBool
                               , savepointTimeoutSec :: !CUInt
                               , checkpointTimeoutSec :: !CUInt
                               , walBufferSz :: !CSize
                               , checkpointBufferSz :: !CUChar
                               , walLockInterceptor :: !(FunPtr (CBool -> Ptr () -> IO RC))
                               , walLockInterceptorOpaque :: !(Ptr ()) }
instance Storable IWKVWalOpts where
        sizeOf _ = #{size IWKV_WAL_OPTS}
        alignment _  = #{alignment IWKV_WAL_OPTS}
        peek ptr = do
          enabled <- #{peek IWKV_WAL_OPTS, enabled} ptr
          check_crc_on_checkpoint <- #{peek IWKV_WAL_OPTS, check_crc_on_checkpoint} ptr
          savepoint_timeout_sec <- #{peek IWKV_WAL_OPTS, savepoint_timeout_sec} ptr
          checkpoint_timeout_sec <- #{peek IWKV_WAL_OPTS, checkpoint_timeout_sec} ptr
          wal_buffer_sz <- #{peek IWKV_WAL_OPTS, wal_buffer_sz} ptr
          checkpoint_buffer_sz <- #{peek IWKV_WAL_OPTS, checkpoint_buffer_sz} ptr
          wal_lock_interceptor <- #{peek IWKV_WAL_OPTS, wal_lock_interceptor} ptr
          wal_lock_interceptor_opaque <- #{peek IWKV_WAL_OPTS, wal_lock_interceptor_opaque} ptr
          return $ IWKVWalOpts enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz wal_lock_interceptor wal_lock_interceptor_opaque
        poke ptr (IWKVWalOpts enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz wal_lock_interceptor wal_lock_interceptor_opaque) = do
          #{poke IWKV_WAL_OPTS, enabled} ptr enabled
          #{poke IWKV_WAL_OPTS, check_crc_on_checkpoint} ptr check_crc_on_checkpoint
          #{poke IWKV_WAL_OPTS, savepoint_timeout_sec} ptr savepoint_timeout_sec
          #{poke IWKV_WAL_OPTS, checkpoint_timeout_sec} ptr checkpoint_timeout_sec
          #{poke IWKV_WAL_OPTS, wal_buffer_sz} ptr wal_buffer_sz
          #{poke IWKV_WAL_OPTS, checkpoint_buffer_sz} ptr checkpoint_buffer_sz
          #{poke IWKV_WAL_OPTS, wal_lock_interceptor} ptr wal_lock_interceptor
          #{poke IWKV_WAL_OPTS, wal_lock_interceptor_opaque} ptr wal_lock_interceptor_opaque

zero :: IWKVWalOpts
zero = IWKVWalOpts { enabled = 0
                   , checkCRCOnCheckpoint = 0
                   , savepointTimeoutSec = 0
                   , checkpointTimeoutSec = 0
                   , walBufferSz = 0
                   , checkpointBufferSz = 0
                   , walLockInterceptor = nullFunPtr
                   , walLockInterceptorOpaque = nullPtr
                   }
