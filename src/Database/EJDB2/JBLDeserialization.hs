{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.JBLDeserialization ( FromJBL ) where

import           Control.Monad

import           Data.Maybe
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
    DeserializationInfo { jbl :: JBL, key :: Maybe String }

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
    deserialize :: DeserializationInfo -> IO a
    default deserialize
        :: (Generic a, GFromJBL (Rep a)) => DeserializationInfo -> IO a
    deserialize info = do
        to <$> gdeserialize (DeserializationInfo (jbl info) Nothing)

class GFromJBL f where
    gdeserialize :: DeserializationInfo -> IO (f a)

instance GFromJBL U1 where
    gdeserialize _ = return U1

instance (GFromJBL f) => GFromJBL (D1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f) => GFromJBL (C1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f, Selector c) => GFromJBL (S1 c f) where
    gdeserialize info = do
        let key = getKey $ selName (undefined :: S1 c f p)
        M1 <$> gdeserialize info { key = key }
      where
        getKey "" = Nothing
        getKey k = Just k

instance (GFromJBL a, GFromJBL b) => GFromJBL (a :*: b) where
    gdeserialize info = do
        a <- gdeserialize info
        b <- gdeserialize info
        return (a :*: b)

instance (FromJBL c, Typeable c) => GFromJBL (Rec0 c) where
    gdeserialize info = K1 <$> deserialize info

instance FromJBL c => FromJBL (Maybe c) where
    deserialize info = undefined

getJBLPropertyValue
    :: (Storable a, FromJBL b)
    => JBL
    -> Maybe String
    -> JBLType
    -> (JBL -> CString -> Ptr a -> IO RC)
    -> (a -> b)
    -> IO (Maybe b)
getJBLPropertyValue _ Nothing _ _ _ = return Nothing
getJBLPropertyValue jbl (Just key) jblType reader parser = do
    fmap parser
        <$> alloca (\valuePtr ->
                    withCString key
                                (\cKey -> c_jbl_object_get_type jbl cKey
                                 >>= guard . (jblType ==) . decodeJBLTypeT
                                 >> reader jbl cKey valuePtr >>= checkRC
                                 >> peekOrNothing valuePtr))

peekOrNothing :: Storable a => Ptr a -> IO (Maybe a)
peekOrNothing ptr
    | ptr == nullPtr = return Nothing
    | otherwise = Just <$> peek ptr

instance FromJBL String where
    deserialize (DeserializationInfo _ Nothing) = return ""
    deserialize (DeserializationInfo jbl (Just key)) = fromMaybe ""
        <$> alloca (\valuePtr ->
                    withCString key
                                (\cKey -> c_jbl_object_get_type jbl cKey
                                 >>= guard . (JBVStr ==) . decodeJBLTypeT
                                 >> c_jbl_object_get_str jbl cKey valuePtr
                                 >>= checkRC >> peekOrNothing valuePtr
                                 >>= maybe (return Nothing)
                                           (fmap Just . peekCString)))

instance FromJBL Int where
    deserialize (DeserializationInfo jbl key) = fromMaybe 0
        <$> getJBLPropertyValue jbl
                                key
                                JBVI64
                                c_jbl_object_get_i64
                                fromIntegral

instance FromJBL Integer where
    deserialize (DeserializationInfo jbl key) = fromMaybe 0
        <$> getJBLPropertyValue jbl
                                key
                                JBVI64
                                c_jbl_object_get_i64
                                fromIntegral

instance FromJBL Double where
    deserialize (DeserializationInfo jbl key) = fromMaybe 0
        <$> getJBLPropertyValue jbl
                                key
                                JBVF64
                                c_jbl_object_get_f64
                                (\(CDouble v) -> v)

instance FromJBL Float where
    deserialize (DeserializationInfo jbl key) = fromMaybe 0
        <$> getJBLPropertyValue jbl
                                key
                                JBVF64
                                c_jbl_object_get_f64
                                (\(CDouble v) -> double2Float v)

instance FromJBL Bool where
    deserialize (DeserializationInfo jbl key) = fromMaybe False
        <$> getJBLPropertyValue jbl key JBVBool c_jbl_object_get_f64 toBool

instance {-# OVERLAPPABLE #-}FromJBL c => FromJBL [c] where
    deserialize info = undefined

