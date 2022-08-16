{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.IndexMeta where

import           Data.Int

import           Database.EJDB2.JBL

import           GHC.Generics

-- | Metadata abount collection index.
data IndexMeta =
    IndexMeta { ptr  :: String     -- ^ rfc6901 JSON pointer to indexed field
              , mode :: Int64      -- ^ Index mode
              , idbf :: Int64      -- ^ Index flags
              , dbid :: Int64      -- ^ Index database ID
              , rnum :: Int64      -- ^ Number records stored in index database
              }
    deriving ( Eq, Generic, Show )

instance FromJBL IndexMeta

