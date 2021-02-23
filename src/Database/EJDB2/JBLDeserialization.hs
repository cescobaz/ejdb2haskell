{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.JBLDeserialization
    ( FromJBL
    , FromJBL
    , decode
    , encode
    , encodeToByteString
    ) where

import           Control.Monad.State.Lazy

import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import           Data.Typeable

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Bindings.JBLStruct
import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           GHC.Float
import           GHC.Generics

data DeserializationInfo =
    DeserializationInfo { jbl :: JBL, keys :: [String], key :: Maybe String }

headOrNothing :: [a] -> Maybe a
headOrNothing [] = Nothing
headOrNothing (h : _) = Just h

createJBLIterator :: JBL -> IO (JBLIteratorPtr, Ptr JBL)
createJBLIterator jbl = do
    jblpPtr <- malloc
    jblIteratorPtr <- mallocJBLIterator
    c_jbl_create_iterator_holder jblpPtr >>= checkRC
    c_jbl_iterator_init jbl jblIteratorPtr >>= checkRC
    return (jblIteratorPtr, jblpPtr)

getJBLKeys :: JBL
           -> [String]
           -> Bool
           -> Maybe (JBLIteratorPtr, Ptr JBL, Ptr CString, Ptr CInt)
           -> IO [String]
getJBLKeys jbl keys _ Nothing = do
    (jblIteratorPtr, holder) <- createJBLIterator jbl
    cKeyPtr <- malloc
    cKeyLengthPtr <- malloc
    exists <- toBool
        <$> c_jbl_iterator_next jblIteratorPtr jbl cKeyPtr cKeyLengthPtr
    getJBLKeys jbl
               keys
               exists
               (Just (jblIteratorPtr, holder, cKeyPtr, cKeyLengthPtr))
getJBLKeys jbl
           keys
           False
           (Just (jblIteratorPtr, holder, cKeyPtr, cKeyLengthPtr)) =
    free jblIteratorPtr >> free holder >> free cKeyPtr >> free cKeyLengthPtr
    >> return keys
getJBLKeys jbl
           keys
           True
           iterator@(Just (jblIteratorPtr, holder, cKeyPtr, cKeyLengthPtr)) = do
    cKey <- peek cKeyPtr
    cKeyLength <- peek cKeyLengthPtr
    key <- peekCStringLen (cKey, fromIntegral cKeyLength)
    exists <- toBool
        <$> c_jbl_iterator_next jblIteratorPtr jbl cKeyPtr cKeyLengthPtr
    getJBLKeys jbl (key : keys) exists iterator

class FromJBL a where
    deserialize :: JBL -> IO a
    default deserialize :: (Generic a, GFromJBL (Rep a)) => JBL -> IO a
    deserialize jbl = do
        keys <- getJBLKeys jbl [] False Nothing
        to <$> gdeserialize (DeserializationInfo jbl keys Nothing)

class GFromJBL f where
    gdeserialize :: DeserializationInfo -> IO (f a)

instance GFromJBL U1 where
    gdeserialize _ = return U1

instance (GFromJBL f) => GFromJBL (D1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f) => GFromJBL (C1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f, Selector c) => GFromJBL (S1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL a, GFromJBL b) => GFromJBL (a :*: b) where
    gdeserialize info = do
        a <- gdeserialize info
        b <- gdeserialize info
        return (a :*: b)

instance (FromJBL c, Typeable c) => GFromJBL (Rec0 c) where
    gdeserialize info = K1 <$> gdeserialize info

instance FromJBL c => FromJBL (Maybe c) where
    deserialize Nothing = setJBLPropertyNull
    deserialize (Just a) = deserialize a

instance FromJBL Int where
    deserialize = setJBLIntegral

instance FromJBL Integer where
    deserialize = setJBLIntegral

instance FromJBL Double where
    deserialize = setJBLDouble

instance FromJBL Float where
    deserialize = setJBLDouble . float2Double

instance FromJBL Bool where
    deserialize = setJBLBool

instance FromJBL String where
    deserialize = setJBLString

instance {-# OVERLAPPABLE #-}FromJBL c => FromJBL [c] where
    deserialize array = do
        jblPtr <- getJBLPtr
        liftIO createJQLArray >>= \jblArrayPtr -> do
            liftIO (peek jblArrayPtr) >>= setJBLNested
            pushJBLPtr jblArrayPtr
        mapM_ deserialize array
        setJBLPtr jblPtr

decode :: FromJBL a => JBL -> IO (Maybe a)
decode jbl = undefined

encode :: FromJBL a => a -> (JBL -> IO b) -> IO b
encode obj f = undefined

encodeToByteString :: Aeson.ToJSON a => a -> BS.ByteString
encodeToByteString obj = BSL.toStrict $ Aeson.encode obj
