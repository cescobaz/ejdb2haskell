{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.IndexMode where


import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Database.EJDB2.Bindings.Types.IWKVWalOpts as IWKVWalOpts
import           Database.EJDB2.Bindings.Types.C.String


#include <ejdb2/ejdb2.h>
newtype IndexMode = IndexMode { unIndexMode :: CUChar }

#{enum IndexMode, IndexMode
  , uniqueIndexMode    = EJDB_IDX_UNIQUE
  , strIndexMode    = EJDB_IDX_STR
  , i64IndexMode    = EJDB_IDX_I64
  , f64IndexMode    = EJDB_IDX_F64
  }

allIndexMode :: [IndexMode]
allIndexMode = [uniqueIndexMode, strIndexMode, i64IndexMode , f64IndexMode]

combineIndexMode :: [IndexMode] -> IndexMode
combineIndexMode = IndexMode . foldr ((.|.) . unIndexMode) 0

unCombineIndexMode :: IndexMode -> [IndexMode]
unCombineIndexMode (IndexMode (CUChar oflags)) = filter f allIndexMode
          where
            f = \(IndexMode (CUChar value)) -> value .&. oflags /= 0
