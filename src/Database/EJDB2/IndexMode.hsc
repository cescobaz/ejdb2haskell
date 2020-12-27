{-# LANGUAGE CPP #-}

module Database.EJDB2.IndexMode where


import           Foreign
import           Foreign.C.Types


#include <ejdb2/ejdb2.h>
-- | Index creation mode.
newtype IndexMode = IndexMode { unIndexMode :: CUChar }

-- | Marks index is unique, no duplicated values allowed.
uniqueIndexMode    = IndexMode #{const EJDB_IDX_UNIQUE}
-- | Index values have string type.
--
-- Type conversion will be performed on atempt to save value with other type.
strIndexMode    = IndexMode #{const EJDB_IDX_STR}
-- | Index values have signed integer 64 bit wide type.
--
-- Type conversion will be performed on atempt to save value with other type.
i64IndexMode    = IndexMode #{const EJDB_IDX_I64}
-- | Index value have floating point type.
--  /Internally floating point numbers are converted to string with precision of 6 digits after decimal point./
f64IndexMode    = IndexMode #{const EJDB_IDX_F64}

allIndexMode :: [IndexMode]
allIndexMode = [uniqueIndexMode, strIndexMode, i64IndexMode , f64IndexMode]

combineIndexMode :: [IndexMode] -> IndexMode
combineIndexMode = IndexMode . foldr ((.|.) . unIndexMode) 0

unCombineIndexMode :: IndexMode -> [IndexMode]
unCombineIndexMode (IndexMode (CUChar oflags)) = filter f allIndexMode
          where
            f = \(IndexMode (CUChar value)) -> value .&. oflags /= 0
