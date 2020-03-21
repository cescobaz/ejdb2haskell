{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>
type IWRC = CUIntMax

type EJDBPtr = Ptr Int

data Error = ErrorOK
           | ErrorInvalidCollectionName
           | ErrorInvalidCollectionMeta
           | ErrorInvalidCollectionIndexMeta
           | ErrorInvalidIndexMode
           | ErrorMismatchedIndexUniquenessMode
           | ErrorUniqueIndexConstraintViolated
           | ErrorCollectionNotFound
           | ErrorTargetCollectionExists
           | ErrorPatchJsonNotObject
    deriving ( Eq, Show )
