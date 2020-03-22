{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.IWKVOpts where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Database.EJDB2.Bindings.Types.IWKVWalOpts as IWKVWalOpts
import           Database.EJDB2.Bindings.Types.C.String


#include <ejdb2/ejdb2.h>
type IWKVOpenFlags = CUChar

data IWKVOpts =
    IWKVOpts { path :: Maybe String
             , randomSeed :: !CUInt
             , fmtVersion :: !CInt
             , oflags :: !IWKVOpenFlags
             , fileLockFailFast :: !CBool
             , wal :: !IWKVWalOpts }
instance Storable IWKVOpts where
        sizeOf _ = #{size IWKV_OPTS}
        alignment _  = #{alignment IWKV_OPTS}
        peek ptr = do
                path <- #{peek IWKV_OPTS, path} ptr >>= cStringToMaybeString
                random_seed <- #{peek IWKV_OPTS, random_seed} ptr
                fmt_version <- #{peek IWKV_OPTS, fmt_version} ptr
                oflags <- #{peek IWKV_OPTS, oflags} ptr
                file_lock_fail_fast <- #{peek IWKV_OPTS, file_lock_fail_fast} ptr
                wal <- #{peek IWKV_OPTS, wal} ptr
                return $ IWKVOpts path random_seed fmt_version oflags file_lock_fail_fast wal
        poke ptr (IWKVOpts path random_seed fmt_version oflags file_lock_fail_fast wal) = do
                cPath <- maybeStringToCString path
                #{poke IWKV_OPTS, path} ptr cPath
                #{poke IWKV_OPTS, random_seed} ptr random_seed
                #{poke IWKV_OPTS, fmt_version} ptr fmt_version
                #{poke IWKV_OPTS, oflags} ptr oflags
                #{poke IWKV_OPTS, file_lock_fail_fast} ptr file_lock_fail_fast
                #{poke IWKV_OPTS, wal} ptr wal
zero :: IWKVOpts
zero = IWKVOpts { path = Nothing
                , randomSeed = 0
                , fmtVersion = 0
                , oflags = 0
                , fileLockFailFast = 0
                , wal = IWKVWalOpts.zero
                }
