{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}

module Database.EJDB2.JBL ( ToJBL, encode, FromJBL, decode ) where

import           Database.EJDB2.Bindings.JBL
import           Database.EJDB2.FromJBL
import           Database.EJDB2.ToJBL

encode :: ToJBL a => a -> (JBL -> IO b) -> IO b
encode obj f = undefined

decode :: FromJBL a => JBL -> IO (Maybe a)
decode jbl = Just <$> deserialize (DeserializationInfo jbl Nothing)
