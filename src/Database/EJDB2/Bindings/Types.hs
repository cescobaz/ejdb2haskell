module Database.EJDB2.Bindings.Types where

import           Foreign

type EJDBPtr = IntPtr

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

