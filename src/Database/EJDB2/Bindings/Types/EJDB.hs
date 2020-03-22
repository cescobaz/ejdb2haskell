module Database.EJDB2.Bindings.Types.EJDB where

import           Foreign

type EJDB = Ptr ()

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

