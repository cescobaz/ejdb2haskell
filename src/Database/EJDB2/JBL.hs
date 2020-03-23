module Database.EJDB2.JBL ( decode, decodePtr ) where

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BS
import           Data.IORef

import qualified Database.EJDB2.Bindings.IW  as IW
import           Database.EJDB2.Bindings.JBL

import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Array

decode :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decode jbl = do
    ref <- newIORef BS.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= IW.checkRCFinally (freeHaskellFunPtr thePrinter)
    string <- readIORef ref
    return $ Aeson.decode (BS.reverse string)

decodePtr :: Aeson.FromJSON a => Ptr JBL -> IO (Maybe a)
decodePtr jblPtr = peek jblPtr >>= decode

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

printerArray :: IORef BS.ByteString -> [CChar] -> IO IW.RC
printerArray ref array = do
    modifyIORef' ref $ \string ->
        foldl (\result (CChar ch) -> BS.cons (fromIntegral ch) result)
              string
              array
    return 0

