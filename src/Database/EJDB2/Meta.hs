{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.Meta where

import           Data.Int

import           Database.EJDB2.CollectionMeta
import           Database.EJDB2.JBL

import           GHC.Generics                  hiding ( Meta )

-- | Metadata about database.
data Meta =
    Meta { version     :: String            -- ^ EJDB engine version
         , file        :: String            -- ^ Path to storage file
         , size        :: Int64             -- ^ Storage file size in bytes
         , collections :: [CollectionMeta]  -- ^ List of collections
         }
    deriving ( Eq, Generic, Show )

instance FromJBL Meta

