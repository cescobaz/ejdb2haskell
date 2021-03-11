{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.FromJBL ( FromJBL(..), DeserializationInfo(..) ) where

import           Control.Exception

import qualified Data.HashSet                      as HashSet
import qualified Data.Hashable                     as Hashable
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

data DeserializationInfo = DeserializationInfo JBL (Maybe String)

class FromJBL a where
    deserialize :: DeserializationInfo -> IO a
    default deserialize
        :: (Generic a, GFromJBL (Rep a)) => DeserializationInfo -> IO a
    deserialize info = to <$> gdeserialize info

instance {-# OVERLAPPABLE #-}FromJBL a => FromJBL (Maybe a) where
    deserialize info = handle (\(_ :: IOException) -> return Nothing)
                              (Just <$> deserialize info)

class GFromJBL f where
    gdeserialize :: DeserializationInfo -> IO (f a)

instance GFromJBL U1 where
    gdeserialize _ = return U1

instance (GFromJBL f) => GFromJBL (D1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f) => GFromJBL (C1 c f) where
    gdeserialize info = M1 <$> gdeserialize info

instance (GFromJBL f, Selector c) => GFromJBL (S1 c f) where
    gdeserialize (DeserializationInfo jbl _) = do
        let key = getKey $ selName (undefined :: S1 c f p)
        M1 <$> gdeserialize (DeserializationInfo jbl key)
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

getJBLPropertyValue
    :: (Storable a, FromJBL b)
    => JBL
    -> Maybe String
    -> JBLType
    -> (JBL -> CString -> Ptr a -> IO RC)
    -> (a -> b)
    -> IO (Maybe b)
getJBLPropertyValue _ Nothing _ _ _ = return Nothing
getJBLPropertyValue jbl (Just key) jblType reader parser =
    handle (\(_ :: IOException) -> return Nothing)
           (fmap parser
            <$> withCString key
                            (\cKey ->
                             checkJBLPropertyType jbl
                                                  cKey
                                                  jblType
                                                  (getJBLPropertyValue' jbl
                                                                        cKey
                                                                        reader)))

checkJBLPropertyType
    :: JBL -> CString -> JBLType -> IO (Maybe a) -> IO (Maybe a)
checkJBLPropertyType jbl cKey jblType computation =
    c_jbl_object_get_type jbl cKey
    >>= (\t -> if t == jblType
               then computation
               else if t == JBVNull || t == JBVNone
                    then return Nothing
                    else peekCString cKey >>= \k ->
                        fail ("incorrect type (" ++ show t ++ "for key " ++ k))
    . decodeJBLTypeT

getJBLPropertyValue' :: Storable a
                     => JBL
                     -> CString
                     -> (JBL -> CString -> Ptr a -> IO RC)
                     -> IO (Maybe a)
getJBLPropertyValue' jbl cKey reader =
    alloca (\valuePtr ->
            reader jbl cKey valuePtr >>= checkRC >> peekOrNothing valuePtr)

getJBLValue :: (Storable a, FromJBL b)
            => JBL
            -> JBLType
            -> (JBL -> IO a)
            -> (a -> b)
            -> IO (Maybe b)
getJBLValue jbl jblType reader parser =
    handle (\(_ :: IOException) -> return Nothing)
           (fmap parser <$> checkJBLValueType jbl jblType (Just <$> reader jbl))

checkJBLValueType :: JBL -> JBLType -> IO (Maybe a) -> IO (Maybe a)
checkJBLValueType jbl jblType computation = c_jbl_type jbl
    >>= (\t -> if t == JBVNull
               then return Nothing
               else if t == jblType then computation else fail "ciao")
    . decodeJBLTypeT

peekOrNothing :: Storable a => Ptr a -> IO (Maybe a)
peekOrNothing ptr
    | ptr == nullPtr = return Nothing
    | otherwise = Just <$> peek ptr

instance FromJBL String where
    deserialize info = fromMaybe "" <$> deserialize info

instance FromJBL (Maybe String) where
    deserialize (DeserializationInfo jbl Nothing) =
        checkJBLValueType jbl
                          JBVStr
                          (c_jbl_get_str jbl >>= (fmap Just . peekCString))
    deserialize (DeserializationInfo jbl (Just key)) =
        withCString key
                    (\cKey -> checkJBLPropertyType jbl cKey JBVStr $
                     alloca (\valuePtr ->
                             c_jbl_object_get_str jbl cKey valuePtr >>= checkRC
                             >> peekOrNothing valuePtr
                             >>= maybe (return Nothing)
                                       (fmap Just . peekCString)))

convertJBLValue :: (Num a, Eq a) => Maybe a -> Maybe a
convertJBLValue (Just 0) = Nothing
convertJBLValue a = a

instance FromJBL Int where
    deserialize info = fromMaybe 0 <$> deserialize info

instance FromJBL (Maybe Int) where
    deserialize (DeserializationInfo jbl Nothing) =
        convertJBLValue <$> getJBLValue jbl JBVI64 c_jbl_get_i64 fromIntegral
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl key JBVI64 c_jbl_object_get_i64 fromIntegral

instance FromJBL Int64 where
    deserialize info = fromMaybe 0 <$> deserialize info

instance FromJBL (Maybe Int64) where
    deserialize (DeserializationInfo jbl Nothing) =
        convertJBLValue <$> getJBLValue jbl JBVI64 c_jbl_get_i64 fromIntegral
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl key JBVI64 c_jbl_object_get_i64 fromIntegral

instance FromJBL Integer where
    deserialize info = fromMaybe 0 <$> deserialize info

instance FromJBL (Maybe Integer) where
    deserialize (DeserializationInfo jbl Nothing) =
        convertJBLValue <$> getJBLValue jbl JBVI64 c_jbl_get_i64 fromIntegral
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl key JBVI64 c_jbl_object_get_i64 fromIntegral

instance FromJBL Double where
    deserialize info = fromMaybe 0 <$> deserialize info

cDoubleToDouble :: CDouble -> Double
cDoubleToDouble (CDouble a) = a

instance FromJBL (Maybe Double) where
    deserialize (DeserializationInfo jbl Nothing) = convertJBLValue
        <$> getJBLValue jbl JBVF64 c_jbl_get_f64 cDoubleToDouble
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl key JBVF64 c_jbl_object_get_f64 cDoubleToDouble

instance FromJBL Float where
    deserialize info = fromMaybe 0 <$> deserialize info

instance FromJBL (Maybe Float) where
    deserialize (DeserializationInfo jbl Nothing) = convertJBLValue
        <$> getJBLValue jbl
                        JBVF64
                        c_jbl_get_f64
                        (double2Float . cDoubleToDouble)
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl
                            key
                            JBVF64
                            c_jbl_object_get_f64
                            (double2Float . cDoubleToDouble)

instance FromJBL Bool where
    deserialize info = fromMaybe False <$> deserialize info

instance FromJBL (Maybe Bool) where
    deserialize (DeserializationInfo jbl Nothing) =
        fmap toBool . convertJBLValue
        <$> (getJBLValue jbl JBVI64 c_jbl_get_i32 fromIntegral
                 :: IO (Maybe Int))
    deserialize (DeserializationInfo jbl key) =
        getJBLPropertyValue jbl key JBVBool c_jbl_object_get_bool toBool

instance {-# OVERLAPPABLE #-}FromJBL c => FromJBL [c] where
    deserialize info = fromMaybe [] <$> deserialize info

instance {-# OVERLAPPABLE #-}FromJBL c => FromJBL (Maybe [c]) where
    deserialize (DeserializationInfo _ Nothing) = return Nothing
    deserialize (DeserializationInfo jbl (Just key)) =
        handle (\(_ :: IOException) -> return Nothing)
               (withCString key
                            (\cKey ->
                             checkJBLPropertyType jbl cKey JBVArray $ do
                                 jblPtr <- createJBLArray
                                 finally (do
                                              jblOut <- peek jblPtr
                                              c_jbl_object_get_fill_jbl jbl
                                                                        cKey
                                                                        jblOut
                                                  >>= checkRC
                                              Just <$> deserializeArray jblOut)
                                         (freeJBLObject jblPtr)))

deserializeArray :: FromJBL a => JBL -> IO [a]
deserializeArray jbl = do
    (jblIteratorPtr, holder) <- createJBLIterator jbl
    finally (deserializeArray' jblIteratorPtr holder)
            (freeJBLIterator (jblIteratorPtr, holder))

deserializeArray' :: FromJBL a => JBLIteratorPtr -> Ptr JBL -> IO [a]
deserializeArray' jblIteratorPtr jblPtr = do
    jbl <- peek jblPtr
    stop <- not . toBool
        <$> c_jbl_iterator_next jblIteratorPtr jbl nullPtr nullPtr
    if stop
        then return []
        else (do
                  element <- deserialize (DeserializationInfo jbl Nothing)
                  (element :) <$> deserializeArray' jblIteratorPtr jblPtr)

instance (Eq c, Hashable.Hashable c, FromJBL c)
    => FromJBL (Maybe (HashSet.HashSet c)) where
    deserialize info = fmap HashSet.fromList <$> deserialize info

instance (Eq c, Hashable.Hashable c, FromJBL c)
    => FromJBL (HashSet.HashSet c) where
    deserialize info = HashSet.fromList <$> deserialize info

