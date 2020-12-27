module Database.EJDB2.JBL ( decode, encode, encodeToByteString ) where

import           Control.Exception

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.IORef

import           Database.EJDB2.Bindings.JBL
import qualified Database.EJDB2.Result       as Result

import           Foreign
import           Foreign.C.Types

decode :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decode jbl = Aeson.decode' <$> decodeToByteString jbl

decodeToByteString :: JBL -> IO BSL.ByteString
decodeToByteString jbl = do
    ref <- newIORef BSL.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= Result.checkRCFinally (freeHaskellFunPtr thePrinter)
    BSL.reverse <$> readIORef ref

printer :: IORef BSL.ByteString -> JBLJSONPrinter
printer ref _ 0 (CChar ch) _ _ = do
    modifyIORef' ref (BSL.cons' (fromIntegral ch))
    return 0
printer ref buffer size _ _ _
    | size > 0 = peekArray (fromIntegral size) buffer >>= printerArray ref
    | otherwise = peekArray0 (CChar 0) buffer >>= printerArray ref

printerArray :: IORef BSL.ByteString -> [CChar] -> IO Result.RC
printerArray ref array = do
    modifyIORef' ref $ \string ->
        foldl (\result (CChar ch) -> BSL.cons' (fromIntegral ch) result)
              string
              array
    return 0

encode :: Aeson.ToJSON a => a -> (JBL -> IO b) -> IO b
encode obj f = do
    let byteString = encodeToByteString obj
    BS.useAsCString byteString $ \string -> alloca $ \jblPtr ->
        finally (c_jbl_from_json jblPtr string >>= Result.checkRC >> peek jblPtr
                 >>= f)
                (c_jbl_destroy jblPtr)

encodeToByteString :: Aeson.ToJSON a => a -> BS.ByteString
encodeToByteString obj = BSL.toStrict $ Aeson.encode obj
