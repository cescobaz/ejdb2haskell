{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.JBLDeserialization ( FromJBL ) where

import           Control.Monad.State.Lazy

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
    deserialize :: DeserializationInfo -> IO a
    default deserialize
        :: (Generic a, GFromJBL (Rep a)) => DeserializationInfo -> IO a
    deserialize info = do
        keys <- getJBLKeys (jbl info) [] False Nothing
        to <$> gdeserialize (DeserializationInfo (jbl info) keys Nothing)

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
        let key = selName (undefined :: S1 c f p)
        M1 <$> gdeserialize info { key = Just key }

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
    -> b -- default value
    -> (a -> b)
    -> (JBL -> CString -> Ptr a -> IO RC)
    -> IO b
getJBLPropertyValue _ Nothing defaultValue _ _ = return defaultValue
getJBLPropertyValue jbl (Just key) _ parser reader = do
    parser <$> alloca (\valuePtr ->
                       withCString key
                                   (\cKey -> reader jbl cKey valuePtr
                                    >>= checkRC >> peek valuePtr))

instance FromJBL Int where
    deserialize (DeserializationInfo jbl _ key) =
        getJBLPropertyValue jbl key 0 fromIntegral c_jbl_object_get_i64

instance FromJBL Integer where
    deserialize (DeserializationInfo jbl _ key) =
        getJBLPropertyValue jbl key 0 fromIntegral c_jbl_object_get_i64

instance FromJBL Double where
    deserialize (DeserializationInfo jbl _ key) =
        getJBLPropertyValue jbl key 0 (\(CDouble v) -> v) c_jbl_object_get_f64

instance FromJBL Float where
    deserialize (DeserializationInfo jbl _ key) =
        getJBLPropertyValue jbl
                            key
                            0
                            (\(CDouble v) -> double2Float v)
                            c_jbl_object_get_f64

instance FromJBL Bool where
    deserialize (DeserializationInfo jbl _ key) =
        getJBLPropertyValue jbl key False toBool c_jbl_object_get_f64

instance FromJBL String where
    deserialize info = undefined

instance {-# OVERLAPPABLE #-}FromJBL c => FromJBL [c] where
    deserialize info = undefined

