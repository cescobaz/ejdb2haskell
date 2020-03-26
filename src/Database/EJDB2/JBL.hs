{-# LANGUAGE OverloadedStrings #-}

module Database.EJDB2.JBL ( decode, decode', encode, encodeToByteString ) where

import           Control.Exception

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Strict         as Map
import           Data.IORef
import           Data.Int

import qualified Database.EJDB2.Bindings.IW  as IW
import           Database.EJDB2.Bindings.JBL

import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Array

decode :: Aeson.FromJSON a => JBL -> IO (Maybe a)
decode jbl = Aeson.decode <$> decodeToByteString jbl

decode' :: Aeson.FromJSON a => JBL -> Int64 -> IO (Maybe a)
decode' jbl id = parse . setId id <$> decode jbl

decodeToByteString :: JBL -> IO BSL.ByteString
decodeToByteString jbl = do
    ref <- newIORef BSL.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= IW.checkRCFinally (freeHaskellFunPtr thePrinter)
    BSL.reverse <$> readIORef ref

parse :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parse Nothing = Nothing
parse (Just value) = case Aeson.fromJSON value of
    Aeson.Success v -> Just v
    Aeson.Error _ -> Nothing

setId :: Int64 -> Maybe Aeson.Value -> Maybe Aeson.Value
setId id (Just (Aeson.Object map)) =
    Just (Aeson.Object (Map.insert "id" (Aeson.Number $ fromIntegral id) map))
setId _ Nothing = Nothing
setId _ value = value

printer :: IORef BSL.ByteString -> JBLJSONPrinter
printer ref _ 0 (CChar ch) _ _ = do
    modifyIORef' ref $ \string -> BSL.cons word string
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

printerArray :: IORef BSL.ByteString -> [CChar] -> IO IW.RC
printerArray ref array = do
    modifyIORef' ref $ \string ->
        foldl (\result (CChar ch) -> BSL.cons (fromIntegral ch) result)
              string
              array
    return 0

encode :: Aeson.ToJSON a => a -> (JBL -> IO b) -> IO b
encode obj f = do
    let byteString = encodeToByteString obj
    BS.useAsCString byteString $ \string -> alloca $ \jblPtr ->
        finally (c_jbl_from_json jblPtr string >>= IW.checkRC >> peek jblPtr
                 >>= f)
                (c_jbl_destroy jblPtr)

encodeToByteString :: Aeson.ToJSON a => a -> BS.ByteString
encodeToByteString obj = BSL.toStrict $ Aeson.encode obj
