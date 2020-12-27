module Database.EJDB2.JBL ( decode, encode, encodeToByteString ) where

import           Control.Exception

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Foldable
import           Data.IORef

import           Database.EJDB2.Bindings.JBL
import qualified Database.EJDB2.Result       as Result

import           Foreign
import           Foreign.C.Types

decode :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decode jbl = Aeson.decode' . B.toLazyByteString <$> decodeToBuilder jbl

decodeToBuilder :: JBL -> IO B.Builder
decodeToBuilder jbl = do
    ref <- newIORef mempty
    thePrinter <- mkJBLJSONPrinter (builderPrinter ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= Result.checkRCFinally (freeHaskellFunPtr thePrinter)
    readIORef ref

builderPrinter :: IORef B.Builder -> JBLJSONPrinter
builderPrinter ref _ 0 (CChar ch) _ _ = do
    modifyIORef' ref $ \builder -> builder <> B.word8 (fromIntegral ch)
    return 0
builderPrinter ref buffer size _ _ _
    | size > 0 = do
        array <- peekArray (fromIntegral size) buffer
        builderPrinterArray ref array
    | otherwise = do
        array <- peekArray0 (CChar 0) buffer
        builderPrinterArray ref array

builderPrinterArray :: IORef B.Builder -> [CChar] -> IO Result.RC
builderPrinterArray ref array = do
    modifyIORef' ref $ \builder ->
        foldl' (\result (CChar ch) -> result <> B.word8 (fromIntegral ch))
               builder
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
