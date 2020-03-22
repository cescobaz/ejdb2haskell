{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.JQL where

import           Data.ByteString.Char8

import           Database.EJDB2.Bindings.Types.C.String

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>
type JQL = Ptr ()
