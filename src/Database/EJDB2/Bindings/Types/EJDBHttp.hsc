{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBHttp where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Database.EJDB2.Bindings.Types.C.String

#include <ejdb2/ejdb2.h>

data EJDBHttp = EJDBHttp { enabled :: !CBool
                         , port :: !CInt
                         , bind :: Maybe String
                         , accessToken :: Maybe String
                         , blocking :: !CBool
                         , readAnon :: !CBool
                         , maxBodySize :: !CSize }
instance Storable EJDBHttp where
        sizeOf _ = #{size EJDB_HTTP}
        alignment _ = #{alignment EJDB_HTTP}
        peek ptr = do
           enabled <- #{peek EJDB_HTTP, enabled} ptr
           port <- #{peek EJDB_HTTP, port} ptr
           bind <- #{peek EJDB_HTTP, bind} ptr >>= cStringToMaybeString
           access_token <- #{peek EJDB_HTTP, access_token} ptr
           access_token_len <- #{peek EJDB_HTTP, access_token_len} ptr
           accessToken <- cStringLenToMaybeString (access_token, access_token_len)
           blocking <- #{peek EJDB_HTTP, blocking} ptr
           read_anon <- #{peek EJDB_HTTP, read_anon} ptr
           max_body_size <- #{peek EJDB_HTTP, max_body_size} ptr
           return $ EJDBHttp enabled port bind accessToken blocking read_anon max_body_size
        poke ptr (EJDBHttp enabled port bind accessToken blocking read_anon max_body_size) = do
           #{poke EJDB_HTTP, enabled} ptr enabled
           #{poke EJDB_HTTP, port} ptr port
           maybeStringToCString bind >>= (#{poke EJDB_HTTP, bind} ptr)
           (access_token, access_token_len) <- maybeStringToCStringLen accessToken
           #{poke EJDB_HTTP, access_token} ptr access_token
           #{poke EJDB_HTTP, access_token_len} ptr access_token_len
           #{poke EJDB_HTTP, blocking} ptr blocking
           #{poke EJDB_HTTP, read_anon} ptr read_anon
           #{poke EJDB_HTTP, max_body_size} ptr max_body_size


zero :: EJDBHttp
zero = EJDBHttp { enabled = 0
                , port = 0
                , bind = Nothing
                , accessToken = Nothing
                , blocking = 0
                , readAnon = 0
                , maxBodySize = 0
                }
