{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.IWKVOpts where


import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Database.EJDB2.Bindings.Types.IWKVWalOpts as IWKVWalOpts
import           Database.EJDB2.Bindings.Types.C.String


#include <ejdb2/ejdb2.h>
newtype OpenFlags = OpenFlags { unOpenFlags :: CUChar }

#{enum OpenFlags, OpenFlags
  , readonlyOpenFlags         = IWKV_RDONLY
  , truncateOpenFlags         = IWKV_TRUNC
  , noTrimOnCloseOpenFlags    = IWKV_NO_TRIM_ON_CLOSE
  }

allOpenFlags :: [OpenFlags]
allOpenFlags = [readonlyOpenFlags, truncateOpenFlags, noTrimOnCloseOpenFlags]

combineOpenFlags :: [OpenFlags] -> OpenFlags
combineOpenFlags = OpenFlags . foldr ((.|.) . unOpenFlags) 0

unCombineOpenFlags :: OpenFlags -> [OpenFlags]
unCombineOpenFlags (OpenFlags (CUChar oflags)) = filter f allOpenFlags
          where
            f = \(OpenFlags (CUChar value)) -> value .&. oflags /= 0

data IWKVOpts =
    IWKVOpts { path :: Maybe String
             , randomSeed :: !CUInt
             , fmtVersion :: !CInt
             , oflags :: ![OpenFlags]
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
                return $ IWKVOpts
                              path
                              random_seed
                              fmt_version
                              (unCombineOpenFlags $ OpenFlags oflags)
                              file_lock_fail_fast
                              wal
        poke ptr (IWKVOpts path random_seed fmt_version oflags file_lock_fail_fast wal) = do
                cPath <- maybeStringToCString path
                #{poke IWKV_OPTS, path} ptr cPath
                #{poke IWKV_OPTS, random_seed} ptr random_seed
                #{poke IWKV_OPTS, fmt_version} ptr fmt_version
                #{poke IWKV_OPTS, oflags} ptr (unOpenFlags $ combineOpenFlags oflags)
                #{poke IWKV_OPTS, file_lock_fail_fast} ptr file_lock_fail_fast
                #{poke IWKV_OPTS, wal} ptr wal
zero :: IWKVOpts
zero = IWKVOpts { path = Nothing
                , randomSeed = 0
                , fmtVersion = 0
                , oflags = []
                , fileLockFailFast = 0
                , wal = IWKVWalOpts.zero
                }
