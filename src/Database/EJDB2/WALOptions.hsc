{-# LANGUAGE CPP #-}

module Database.EJDB2.WALOptions (WALOptions(..), zero) where


import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import Database.EJDB2.Result

#include <ejdb2/ejdb2.h>

-- | Write ahead log (WAL) options.
data WALOptions = WALOptions { enabled :: !Bool -- ^ WAL enabled
                             , checkCRCOnCheckpoint :: !Bool -- ^ Check CRC32 sum of data blocks during checkpoint. Default: false
                             , savepointTimeoutSec :: !Word32 -- ^ Savepoint timeout seconds. Default: 10 sec
                             , checkpointTimeoutSec :: !Word32 -- ^ Checkpoint timeout seconds. Default: 300 sec (5 min);
                             , walBufferSz :: !Word64 -- ^ WAL file intermediate buffer size. Default: 8Mb
                             , checkpointBufferSz :: !Word8 -- ^ Checkpoint buffer size in bytes. Default: 1Gb
                             , walLockInterceptor :: !(FunPtr (CBool -> Ptr () -> IO RC)) -- ^ Optional function called before acquiring and after releasing.
-- exclusive database lock byAL checkpoint thread.
-- In the case of 'before loc first argument will be set to true
                             , walLockInterceptorOpaque :: !(Ptr ()) -- ^ Opaque data for 'walLockInterceptor'
                             }

-- | Create default WALOptions
zero :: WALOptions
zero = WALOptions { enabled = False
                   , checkCRCOnCheckpoint = False
                   , savepointTimeoutSec = 0
                   , checkpointTimeoutSec = 0
                   , walBufferSz = 0
                   , checkpointBufferSz = 0
                   , walLockInterceptor = nullFunPtr
                   , walLockInterceptorOpaque = nullPtr
                   }

instance Storable WALOptions where
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
          return $ WALOptions enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz wal_lock_interceptor wal_lock_interceptor_opaque
        poke ptr (WALOptions enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz wal_lock_interceptor wal_lock_interceptor_opaque) = do
          #{poke IWKV_WAL_OPTS, enabled} ptr enabled
          #{poke IWKV_WAL_OPTS, check_crc_on_checkpoint} ptr check_crc_on_checkpoint
          #{poke IWKV_WAL_OPTS, savepoint_timeout_sec} ptr savepoint_timeout_sec
          #{poke IWKV_WAL_OPTS, checkpoint_timeout_sec} ptr checkpoint_timeout_sec
          #{poke IWKV_WAL_OPTS, wal_buffer_sz} ptr wal_buffer_sz
          #{poke IWKV_WAL_OPTS, checkpoint_buffer_sz} ptr checkpoint_buffer_sz
          #{poke IWKV_WAL_OPTS, wal_lock_interceptor} ptr wal_lock_interceptor
          #{poke IWKV_WAL_OPTS, wal_lock_interceptor_opaque} ptr wal_lock_interceptor_opaque
