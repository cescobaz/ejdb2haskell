module Database.EJDB2.JBL ( ToJBL, encode, FromJBL, decode ) where

import           Control.Exception           ( finally )
import           Control.Monad.IO.Class

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.FromJBL
import           Database.EJDB2.ToJBL

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

encode :: ToJBL a => a -> (JBL -> IO b) -> IO b
encode obj f = do
    state <- liftIO $ execSerialize (serialize obj) (initState nullPtr)
    jbl <- peek (currentJBLPtr state)
    finally (f jbl)
            (mapM_ freeJBLObject (jblPtrs state) >> mapM_ free (cStrings state))

decode :: FromJBL a => JBL -> IO (Maybe a)
decode jbl = Just <$> deserialize (DeserializationInfo jbl Nothing)
