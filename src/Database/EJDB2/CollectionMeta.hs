
{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.CollectionMeta where

import           Data.Int

import           Database.EJDB2.IndexMeta
import           Database.EJDB2.JBL

import           GHC.Generics

-- | Metadata about collection.
data CollectionMeta =
    CollectionMeta { name    :: String        -- ^ Collection name
                   , dbid    :: Int64         -- ^ Collection database ID
                   , rnum    :: Int64         -- ^ Number of documents in collection
                   , indexes :: [IndexMeta]   -- ^ List of collections indexes
                   }
    deriving ( Eq, Generic, Show )

instance FromJBL CollectionMeta

