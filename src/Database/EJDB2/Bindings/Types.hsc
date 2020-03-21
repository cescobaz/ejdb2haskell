{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>
type IWRC = CUIntMax

type EJDBPtr = Ptr Int
