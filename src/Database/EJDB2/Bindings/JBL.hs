{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBL where

import           Database.EJDB2.Bindings.JBLStruct
import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

type JBL = Ptr ()

type JBLNode = Ptr ()

type JBLPrintFlags = CUChar

type JBLJSONPrinter = Ptr CChar -> CInt -> CChar -> CInt -> Ptr () -> IO RC

createJBLObject :: IO (Ptr JBL)
createJBLObject = do
    jblPtr <- malloc
    c_jbl_create_empty_object jblPtr >>= checkRC
    return jblPtr

createJBLArray :: IO (Ptr JBL)
createJBLArray = do
    jblPtr <- malloc
    c_jbl_create_empty_array jblPtr >>= checkRC
    return jblPtr

freeJBLObject :: Ptr JBL -> IO ()
freeJBLObject jblPtr = c_jbl_destroy jblPtr >> free jblPtr

createJBLIterator :: JBL -> IO (JBLIteratorPtr, Ptr JBL)
createJBLIterator jbl = do
    jblpPtr <- malloc
    jblIteratorPtr <- mallocJBLIterator
    c_jbl_create_iterator_holder jblpPtr >>= checkRC
    c_jbl_iterator_init jbl jblIteratorPtr >>= checkRC
    return (jblIteratorPtr, jblpPtr)

freeJBLIterator :: (JBLIteratorPtr, Ptr JBL) -> IO ()
freeJBLIterator (jblIteratorPtr, jblPtr) =
    free jblIteratorPtr >> c_jbl_destroy jblPtr >> free jblPtr

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
        :: JBL -> JBLIteratorPtr -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_iterator_next" c_jbl_iterator_next
        :: JBLIteratorPtr -> JBL -> Ptr CString -> Ptr CInt -> IO CBool

foreign import ccall "ejdb2/jbl.h jbl_type" c_jbl_type :: JBL -> IO JBLTypeT

foreign import ccall "ejdb2/jbl.h jbl_get_i64" c_jbl_get_i64
        :: JBL -> IO CIntMax

foreign import ccall "ejdb2/jbl.h jbl_get_i32" c_jbl_get_i32 :: JBL -> IO CInt

foreign import ccall "ejdb2/jbl.h jbl_get_f64" c_jbl_get_f64
        :: JBL -> IO CDouble

foreign import ccall "ejdb2/jbl.h jbl_get_str" c_jbl_get_str
        :: JBL -> IO CString

foreign import ccall "ejdb2/jbl.h jbl_object_get_type" c_jbl_object_get_type
        :: JBL -> CString -> IO JBLTypeT

foreign import ccall "ejdb2/jbl.h jbl_object_get_i64" c_jbl_object_get_i64
        :: JBL -> CString -> Ptr CIntMax -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_object_get_f64" c_jbl_object_get_f64
        :: JBL -> CString -> Ptr CDouble -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_object_get_bool" c_jbl_object_get_bool
        :: JBL -> CString -> Ptr CBool -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_object_get_str" c_jbl_object_get_str
        :: JBL -> CString -> Ptr CString -> IO RC

foreign import ccall "ejdb2/jbl.h jbl_object_get_fill_jbl" c_jbl_object_get_fill_jbl
        :: JBL -> CString -> JBL -> IO RC
