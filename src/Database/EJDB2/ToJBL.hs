{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.ToJBL ( ToJBL(..) ) where

import           Control.Monad.State.Lazy

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

type SerializationM a = StateT SerializationState IO a

iserialize :: SerializationM a -> SerializationState -> IO SerializationState
iserialize = execStateT

getJBLPtr :: SerializationM (Ptr JBL)
getJBLPtr = currentJBLPtr <$> get

getJBL :: SerializationM JBL
getJBL = get >>= liftIO . peek . currentJBLPtr

pushJBLPtr :: Ptr JBL -> SerializationM ()
pushJBLPtr jblPtr =
    modify (\state ->
            state { currentJBLPtr = jblPtr, jblPtrs = jblPtr : jblPtrs state })

setJBLPtr :: Ptr JBL -> SerializationM ()
setJBLPtr jblPtr = modify (\state -> state { currentJBLPtr = jblPtr })

pushCString :: String -> SerializationM CString
pushCString string = liftIO (newCString string) >>= \cString ->
    modify (\state -> state { cStrings = cString : cStrings state })
    >> return cString

getKey :: SerializationM (Maybe String)
getKey = key <$> get

setKey :: Maybe String -> SerializationM ()
setKey (Just "") = setKey Nothing
setKey key = modify (\state -> state { key = key })

setJBLProperty :: (JBL -> CString -> a -> IO RC) -> a -> SerializationM ()
setJBLProperty f value = getKey
    >>= maybe (return ())
              (\key -> getJBL >>= \jbl -> liftIO $
               withCString key (\cKey -> f jbl cKey value) >>= checkRC)

setJBLPropertyNull :: SerializationM ()
setJBLPropertyNull = setJBLProperty (\jbl key _ -> c_jbl_set_null jbl key) ()

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

createJBLObject :: IO (Ptr JBL)
createJBLObject = do
    jblPtr <- malloc
    c_jbl_create_empty_object jblPtr >>= checkRC
    return jblPtr

createJBLArray :: IO (Ptr JBL)
createJBLArray = do
    jblPtr <- malloc
    c_jbl_create_empty_array jblPtr >>= checkRC
    return jblPtr

class ToJBL a where
    serialize :: a -> SerializationM ()
    default serialize :: (Generic a, GToJBL (Rep a)) => a -> SerializationM ()
    serialize a = do
        jblPtr <- getJBLPtr
        liftIO createJBLObject >>= \jblObjectPtr -> do
            liftIO (peek jblObjectPtr) >>= setJBLNested
            pushJBLPtr jblObjectPtr
        gserialize (from a)
        setJBLPtr jblPtr

class GToJBL f where
    gserialize :: f a -> SerializationM ()

instance GToJBL V1 where
    gserialize _ = return ()

instance GToJBL U1 where
    gserialize _ = return ()

instance (GToJBL f, Datatype c) => GToJBL (D1 c f) where
    gserialize d1 = liftIO (putStrLn ("D1 " ++ t ++ " "))
        >> gserialize (unM1 d1)
      where
        t = datatypeName d1

instance (GToJBL f) => GToJBL (C1 c f) where
    gserialize (M1 x) = liftIO (putStrLn "C1") >> gserialize x

instance (GToJBL f, Selector c) => GToJBL (S1 c f) where
    gserialize s1 = do
        liftIO (putStr "S1 " >> putStrLn key)
        currentKey <- getKey
        setKey (Just key)
        gserialize (unM1 s1)
        setKey currentKey
      where
        key = selName s1

instance (GToJBL a, GToJBL b) => GToJBL (a :*: b) where
    gserialize (a :*: b) = liftIO (putStrLn "prodotto") >> gserialize a
        >> gserialize b

instance (ToJBL c, Typeable c) => GToJBL (Rec0 c) where
    gserialize (K1 x) = liftIO (putStrLn (" K1 " ++ show (typeOf x)))
        >> serialize x

instance ToJBL c => ToJBL (Maybe c) where
    serialize Nothing = setJBLPropertyNull
    serialize (Just a) = serialize a

instance ToJBL Int where
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
        liftIO createJBLArray >>= \jblArrayPtr -> do
            liftIO (peek jblArrayPtr) >>= setJBLNested
            pushJBLPtr jblArrayPtr
        mapM_ serialize array
        setJBLPtr jblPtr


