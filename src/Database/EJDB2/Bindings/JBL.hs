{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBL where

import           Foreign
import           Foreign.C.String

type JBL = Ptr ()

type JBLNode = Ptr ()

foreign import ccall unsafe "ejdb2/jbl.h jbl_get_str" c_jbl_get_str
    :: JBL -> IO CString
