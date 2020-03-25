{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBL where

import           Database.EJDB2.Bindings.IW

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JBL = Ptr ()

type JBLNode = Ptr ()

type JBLPrintFlags = CUChar

type JBLJSONPrinter = Ptr CChar -> CInt -> CChar -> CInt -> Ptr () -> IO RC

foreign import ccall "wrapper" mkJBLJSONPrinter
    :: JBLJSONPrinter -> IO (FunPtr JBLJSONPrinter)

foreign import ccall "ejdb2/jbl.h jbl_as_json" c_jbl_as_json
    :: JBL -> FunPtr JBLJSONPrinter -> Ptr () -> JBLPrintFlags -> IO RC

foreign import ccall unsafe "ejdb2/jbl.h jbl_destroy" c_jbl_destroy
    :: Ptr JBL -> IO ()

foreign import ccall "ejdb2/jbl.h jbl_from_json" c_jbl_from_json
    :: Ptr JBL -> CString -> IO RC

