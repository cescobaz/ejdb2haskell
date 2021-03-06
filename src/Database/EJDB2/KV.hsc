{-# LANGUAGE CPP #-}

module Database.EJDB2.KV
        ( OpenFlags
        , readonlyOpenFlags
        , truncateOpenFlags
        , noTrimOnCloseOpenFlags
        , Options(..)
        , zero
        , OptionsB
        , build
        , options
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import qualified Database.EJDB2.WAL as WAL

#include <ejdb2/ejdb2.h>
-- | Database file open modes.
newtype OpenFlags = OpenFlags { unOpenFlags :: CUChar }

-- | Open storage file in read-only mode.
readonlyOpenFlags :: OpenFlags
readonlyOpenFlags = OpenFlags #{const IWKV_RDONLY}

-- | Truncate storage file on open.
truncateOpenFlags :: OpenFlags
truncateOpenFlags         = OpenFlags #{const IWKV_TRUNC}

noTrimOnCloseOpenFlags :: OpenFlags
noTrimOnCloseOpenFlags    = OpenFlags #{const IWKV_NO_TRIM_ON_CLOSE}

allOpenFlags :: [OpenFlags]
allOpenFlags = [readonlyOpenFlags, truncateOpenFlags, noTrimOnCloseOpenFlags]

combineOpenFlags :: [OpenFlags] -> OpenFlags
combineOpenFlags = OpenFlags . foldr ((.|.) . unOpenFlags) 0

unCombineOpenFlags :: OpenFlags -> [OpenFlags]
unCombineOpenFlags (OpenFlags (CUChar oflags)) = filter f allOpenFlags
          where
            f = \(OpenFlags (CUChar value)) -> value .&. oflags /= 0

-- | IWKV storage open options
data Options =
    Options { path :: Maybe String -- ^ Path to database file
            , randomSeed :: !Word32 -- ^ Random seed used for iwu random generator
            , fmtVersion :: !Int32 -- ^ Database storage format version. Leave it as zero for the latest supported format. Used only for newly created databases
            , oflags :: ![OpenFlags] -- ^ Database file open modes
            , fileLockFailFast :: !Bool -- ^ Do not wait and raise error if database is locked by another process
            , wal :: !WAL.Options
            }

-- | Create default Options
zero :: Options
zero = Options { path = Nothing
                 , randomSeed = 0
                 , fmtVersion = 0
                 , oflags = []
                 , fileLockFailFast = False
                 , wal = WAL.zero
                 }

-- | Storable version of Options
data OptionsB =
    OptionsB { options :: Options
             , pathPtr :: ForeignPtr CChar
             }

-- | Create Storable version of Options
build :: Options -> IO OptionsB
build options = do
        pathPtr <- maybeNew newCString (path options)
        pathFPtr <- newForeignPtr finalizerFree pathPtr
        return OptionsB { options = options
                        , pathPtr = pathFPtr
                        }

instance Storable OptionsB where
        sizeOf _ = #{size IWKV_OPTS}
        alignment _  = #{alignment IWKV_OPTS}
        peek ptr = do
                pathPtr <- #{peek IWKV_OPTS, path} ptr
                pathFPtr <- newForeignPtr finalizerFree nullPtr -- I'm just reading the pointer, I'm not responsable to free memory about this pointer.
                path <- maybePeek peekCString pathPtr
                random_seed <- #{peek IWKV_OPTS, random_seed} ptr
                fmt_version <- #{peek IWKV_OPTS, fmt_version} ptr
                oflags <- #{peek IWKV_OPTS, oflags} ptr
                file_lock_fail_fast <- #{peek IWKV_OPTS, file_lock_fail_fast} ptr
                wal <- #{peek IWKV_OPTS, wal} ptr
                let options = Options 
                                path
                                random_seed
                                fmt_version
                                (unCombineOpenFlags $ OpenFlags oflags)
                                file_lock_fail_fast
                                wal
                return $ OptionsB options pathFPtr
        poke ptr (OptionsB (Options path random_seed fmt_version oflags file_lock_fail_fast wal) pathPtr) = do
                withForeignPtr pathPtr $ \cPath ->
                  #{poke IWKV_OPTS, path} ptr cPath
                #{poke IWKV_OPTS, random_seed} ptr random_seed
                #{poke IWKV_OPTS, fmt_version} ptr fmt_version
                #{poke IWKV_OPTS, oflags} ptr (unOpenFlags $ combineOpenFlags oflags)
                #{poke IWKV_OPTS, file_lock_fail_fast} ptr file_lock_fail_fast
                #{poke IWKV_OPTS, wal} ptr wal

