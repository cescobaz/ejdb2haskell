module Database.EJDB2.JBL ( ToJBL, encode, FromJBL, decode ) where

import           Control.Monad.IO.Class

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.FromJBL
import           Database.EJDB2.ToJBL

import           Foreign.Ptr
import           Foreign.Storable

encode :: ToJBL a => a -> (JBL -> IO b) -> IO b
encode obj f = do
    state <- liftIO $ execSerialize (serialize obj) (initState nullPtr)
    jbl <- peek (currentJBLPtr state)
    f jbl

decode :: FromJBL a => JBL -> IO (Maybe a)
decode jbl = Just <$> deserialize (DeserializationInfo jbl Nothing)
