{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.ToJBL
    ( ToJBL(..)
    , SerializationM
    , SerializationState(..)
    , execSerialize
    , initState
    ) where

import           Control.Monad.State.Lazy

import qualified Data.HashSet                as HashSet
import           Data.Typeable

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           GHC.Float
import           GHC.Generics

data SerializationState =
    SerializationState { currentJBLPtr :: Ptr JBL
                       , jblPtrs       :: [Ptr JBL]
                       , cStrings      :: [CString]
                       , key           :: Maybe String
                       }

initState :: Ptr JBL -> SerializationState
initState jblPtr = SerializationState { currentJBLPtr = jblPtr
                                      , jblPtrs       = []
                                      , cStrings      = []
                                      , key           = Nothing
                                      }

type SerializationM a = StateT SerializationState IO a

execSerialize
    :: SerializationM a -> SerializationState -> IO SerializationState
execSerialize = execStateT

getJBLPtr :: SerializationM (Ptr JBL)
getJBLPtr = currentJBLPtr <$> get

getJBL :: SerializationM JBL
getJBL = get >>= liftIO . peek . currentJBLPtr

pushJBLPtr :: Ptr JBL -> SerializationM ()
pushJBLPtr jblPtr =
    modify (\s -> s { currentJBLPtr = jblPtr, jblPtrs = jblPtr : jblPtrs s })

setJBLPtr :: Ptr JBL -> SerializationM ()
setJBLPtr jblPtr = modify (\s -> s { currentJBLPtr = jblPtr })

pushCString :: String -> SerializationM CString
pushCString string = liftIO (newCString string) >>= \cString ->
    modify (\s -> s { cStrings = cString : cStrings s }) >> return cString

getKey :: SerializationM (Maybe String)
getKey = key <$> get

setKey :: Maybe String -> SerializationM ()
setKey (Just "") = setKey Nothing
setKey k = modify (\s -> s { key = k })

setJBLProperty :: (JBL -> CString -> a -> IO RC) -> a -> SerializationM ()
setJBLProperty f value = getJBL >>= \jbl -> getKey
    >>= maybe (liftIO (f jbl nullPtr value >>= checkRC))
              (\k -> pushCString k
               >>= (\cKey -> liftIO (f jbl cKey value >>= checkRC)))

setJBLPropertyNull :: SerializationM ()
setJBLPropertyNull = setJBLProperty (\jbl k _ -> c_jbl_set_null jbl k) ()

setJBLNested :: JBL -> SerializationM ()
setJBLNested = setJBLProperty c_jbl_set_nested

setJBLBool :: Bool -> SerializationM ()
setJBLBool = setJBLProperty c_jbl_set_bool . CBool . fromBool

setJBLDouble :: Double -> SerializationM ()
setJBLDouble = setJBLProperty c_jbl_set_f64 . CDouble

setJBLIntegral :: (Integral a) => a -> SerializationM ()
setJBLIntegral = setJBLProperty c_jbl_set_int64 . fromIntegral

setJBLString :: String -> SerializationM ()
setJBLString string = pushCString string >>= setJBLProperty c_jbl_set_string

class ToJBL a where
    serialize :: a -> SerializationM ()
    default serialize :: (Generic a, GToJBL (Rep a)) => a -> SerializationM ()
    serialize a = do
        jblPtr <- getJBLPtr
        if jblPtr == nullPtr
            then (do
                      liftIO createJBLObject
                          >>= \jblObjectPtr -> pushJBLPtr jblObjectPtr
                      gserialize (from a))
            else (do
                      liftIO createJBLObject
                          >>= (\childJblPtr -> liftIO (peek childJblPtr)
                               >>= setJBLNested >> pushJBLPtr childJblPtr)
                      gserialize (from a)
                      setJBLPtr jblPtr)

class GToJBL f where
    gserialize :: f a -> SerializationM ()

instance GToJBL V1 where
    gserialize _ = return ()

instance GToJBL U1 where
    gserialize _ = return ()

instance (GToJBL f, Datatype c) => GToJBL (D1 c f) where
    gserialize d1 = gserialize (unM1 d1)

instance (GToJBL f) => GToJBL (C1 c f) where
    gserialize (M1 x) = gserialize x

instance (GToJBL f, Selector c) => GToJBL (S1 c f) where
    gserialize s1 = do
        currentKey <- getKey
        setKey (Just (selName s1))
        gserialize (unM1 s1)
        setKey currentKey

instance (GToJBL a, GToJBL b) => GToJBL (a :*: b) where
    gserialize (a :*: b) = gserialize a >> gserialize b

instance (ToJBL c, Typeable c) => GToJBL (Rec0 c) where
    gserialize (K1 x) = serialize x

instance ToJBL c => ToJBL (Maybe c) where
    serialize Nothing = setJBLPropertyNull
    serialize (Just a) = serialize a

instance ToJBL Int where
    serialize = setJBLIntegral

instance ToJBL Int64 where
    serialize = setJBLIntegral

instance ToJBL Integer where
    serialize = setJBLIntegral

instance ToJBL Double where
    serialize = setJBLDouble

instance ToJBL Float where
    serialize = setJBLDouble . float2Double

instance ToJBL Bool where
    serialize = setJBLBool

instance ToJBL String where
    serialize = setJBLString

instance {-# OVERLAPPABLE #-}ToJBL c => ToJBL [c] where
    serialize array = do
        jblPtr <- getJBLPtr
        currentKey <- getKey
        jblArrayPtr <- liftIO createJBLArray
        pushJBLPtr jblArrayPtr
        setKey Nothing
        mapM_ serialize array
        setKey currentKey
        setJBLPtr jblPtr
        liftIO (peek jblArrayPtr) >>= setJBLNested

instance ToJBL c => ToJBL (HashSet.HashSet c) where
    serialize = serialize . HashSet.toList


