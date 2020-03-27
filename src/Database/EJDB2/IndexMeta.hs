{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.IndexMeta where

import           Data.Aeson   ( FromJSON )
import           Data.Int

import           GHC.Generics

data IndexMeta =
    IndexMeta { ptr  :: String     -- rfc6901 JSON pointer to indexed field
              , mode :: Int64      -- Index mode. Here is EJDB_IDX_I64
              , idbf :: Int64      -- Index flags. See iwdb_flags_t
              , dbid :: Int64      -- Index database ID
              , rnum :: Int64      -- Number records stored in index database
              }
    deriving ( Eq, Generic, Show )

instance FromJSON IndexMeta

