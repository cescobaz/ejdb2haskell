{-# LANGUAGE DeriveGeneric #-}

module Database.EJDB2.Meta where

import           Data.Aeson                    ( FromJSON )
import           Data.Int

import           Database.EJDB2.CollectionMeta

import           GHC.Generics                  hiding ( Meta )

data Meta =
    Meta { version     :: String            -- EJDB engine version
         , file        :: String            -- Path to storage file
         , size        :: Int64             -- Storage file size in bytes
         , collections :: [CollectionMeta]  -- List of collections
         }
    deriving ( Eq, Generic, Show )

instance FromJSON Meta

