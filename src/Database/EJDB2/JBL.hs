{-# LANGUAGE OverloadedStrings #-}

module Database.EJDB2.JBL ( decode, decode' ) where

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BS
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

decodeToByteString :: JBL -> IO BS.ByteString
decodeToByteString jbl = do
    ref <- newIORef BS.empty
    thePrinter <- mkJBLJSONPrinter (printer ref)
    c_jbl_as_json jbl thePrinter nullPtr 0
        >>= IW.checkRCFinally (freeHaskellFunPtr thePrinter)
    BS.reverse <$> readIORef ref

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

