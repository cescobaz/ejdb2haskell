{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBL where

import           Database.EJDB2.Bindings.Types.IWKVBase

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JBL = Ptr ()

type JBLNode = Ptr ()

type JBLPrintFlags = CUChar

type JBLJSONPrinter = Ptr CChar -> CInt -> CChar -> CInt -> Ptr () -> IO IWRC

foreign import ccall "wrapper" mkJBLJSONPrinter
    :: JBLJSONPrinter -> IO (FunPtr JBLJSONPrinter)

foreign import ccall "ejdb2/jbl.h jbl_as_json" c_jbl_as_json
    :: JBL -> FunPtr JBLJSONPrinter -> Ptr () -> JBLPrintFlags -> IO IWRC
