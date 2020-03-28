
{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.CollectionMeta where

import           Data.Aeson               ( FromJSON )
import           Data.Int

import           Database.EJDB2.IndexMeta

import           GHC.Generics

data CollectionMeta =
    CollectionMeta { name    :: String        -- ^ Collection name
                   , dbid    :: Int64         -- ^ Collection database ID
                   , rnum    :: Int64         -- ^ Number of documents in collection
                   , indexes :: [IndexMeta]   -- ^ List of collections indexes
                   }
    deriving ( Eq, Generic, Show )

instance FromJSON CollectionMeta

