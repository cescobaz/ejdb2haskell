{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBL where

import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JBL = Ptr ()

type JBLIterator = Ptr ()

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

foreign import ccall "ejdb2/jbl.h jbl_create_empty_object" c_jbl_create_empty_object
    :: Ptr JBL -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_create_empty_array" c_jbl_create_empty_array
    :: Ptr JBL -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_int64" c_jbl_set_int64
    :: JBL -> CString -> CLong -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_f64" c_jbl_set_f64
    :: JBL -> CString -> CDouble -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_string" c_jbl_set_string
    :: JBL -> CString -> CString -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_bool" c_jbl_set_bool
    :: JBL -> CString -> CBool -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_null" c_jbl_set_null
    :: JBL -> CString -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_empty_array" c_jbl_set_empty_array
    :: JBL -> CString -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_empty_object" c_jbl_set_empty_object
    :: JBL -> CString -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_set_nested" c_jbl_set_nested
    :: JBL -> CString -> JBL -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_create_iterator_holder" c_jbl_create_iterator_holder
    :: Ptr JBL -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_iterator_init" c_jbl_iterator_init
    :: JBL -> Ptr JBLIterator -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_iterator_next" c_jbl_iterator_next
    :: Ptr JBLIterator -> JBL -> Ptr CString -> Ptr CInt -> IO CBool
