module Database.EJDB2.JBL ( decodeJBL, decodeJBLPtr ) where

import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy                   as BS
import           Data.IORef

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.Types.IWKVBase
import           Database.EJDB2.IWKV

import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Array

decodeJBL :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decodeJBL jbl = do
    ref <- newIORef BS.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= checkIWRCFinally (freeHaskellFunPtr thePrinter)
    string <- readIORef ref
    return $ Aeson.decode (BS.reverse string)

decodeJBLPtr :: Aeson.FromJSON a => Ptr JBL -> IO (Maybe a)
decodeJBLPtr jblPtr = peek jblPtr >>= decodeJBL

printer :: IORef BS.ByteString -> JBLJSONPrinter
printer ref _ 0 (CChar ch) _ _ = do
    modifyIORef' ref $ \string -> BS.cons word string
    return 0
  where
    word = fromIntegral ch
printer ref buffer size _ _ _
    | size > 0 = do
        array <- peekArray (fromIntegral size) buffer
        printerArray ref array
    | otherwise = do
        array <- peekArray0 (CChar 0) buffer
        printerArray ref array

printerArray :: IORef BS.ByteString -> [CChar] -> IO IWRC
printerArray ref array = do
    modifyIORef' ref $ \string ->
        foldl (\result (CChar ch) -> BS.cons (fromIntegral ch) result)
              string
              array
    return 0

