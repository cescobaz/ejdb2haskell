{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBHttp
        ( HTTPOptions(..)
        , zero
        , HTTPOptionsB
        , options
        , build
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Database.EJDB2.Bindings.Types.C.String

#include <ejdb2/ejdb2.h>

data HTTPOptions = HTTPOptions { enabled :: !CBool
                         , port :: !CInt
                         , bind :: Maybe String
                         , accessToken :: Maybe String
                         , blocking :: !CBool
                         , readAnon :: !CBool
                         , maxBodySize :: !CSize }

zero :: HTTPOptions
zero = HTTPOptions { enabled = 0
                , port = 0
                , bind = Nothing
                , accessToken = Nothing
                , blocking = 0
                , readAnon = 0
                , maxBodySize = 0
                }

data HTTPOptionsB = HTTPOptionsB { options :: HTTPOptions
                                 , bindPtr :: ForeignPtr CChar
                                 , accessTokenPtr :: ForeignPtr CChar
                                 , accessTokenLen :: CSize
                                 }

build :: HTTPOptions -> IO HTTPOptionsB
build options = do
        bindPtr <-  maybeNew newCString (bind options)
          >>= newForeignPtr finalizerFree
        (accessTokenPtr, accessTokenLen) <- case (accessToken options) of
                                              Nothing -> return (nullPtr, 0)
                                              Just value -> newCStringLen value
        accessTokenFPtr <- newForeignPtr finalizerFree accessTokenPtr
        return HTTPOptionsB { options = options
                            , bindPtr = bindPtr
                            , accessTokenPtr = accessTokenFPtr
                            , accessTokenLen = CSize $ fromIntegral accessTokenLen
                            }


instance Storable HTTPOptionsB where
        sizeOf _ = #{size EJDB_HTTP}
        alignment _ = #{alignment EJDB_HTTP}
        peek ptr = do
           enabled <- #{peek EJDB_HTTP, enabled} ptr
           port <- #{peek EJDB_HTTP, port} ptr
           bindPtr <- #{peek EJDB_HTTP, bind} ptr
           bindFPtr <- newForeignPtr finalizerFree nullPtr
           bind <- maybePeek peekCString bindPtr
           access_token <- #{peek EJDB_HTTP, access_token} ptr
           access_token_len <- #{peek EJDB_HTTP, access_token_len} ptr
           accessTokenFPtr <- newForeignPtr finalizerFree nullPtr
           accessToken <- maybePeek (\ptr -> peekCStringLen (ptr, access_token_len)) access_token
           blocking <- #{peek EJDB_HTTP, blocking} ptr
           read_anon <- #{peek EJDB_HTTP, read_anon} ptr
           max_body_size <- #{peek EJDB_HTTP, max_body_size} ptr
           return $ HTTPOptionsB
                      (HTTPOptions enabled port bind accessToken blocking read_anon max_body_size)
                      bindFPtr accessTokenFPtr (CSize $ fromIntegral access_token_len)
        poke ptr (HTTPOptionsB
                   (HTTPOptions enabled port _ _ blocking read_anon max_body_size)
                    bindPtr accessTokenPtr accessTokenLen) = do
           #{poke EJDB_HTTP, enabled} ptr enabled
           #{poke EJDB_HTTP, port} ptr port
           withForeignPtr bindPtr $ \cBind ->
             #{poke EJDB_HTTP, bind} ptr cBind
           withForeignPtr accessTokenPtr $ \access_token ->
             #{poke EJDB_HTTP, access_token} ptr access_token
           #{poke EJDB_HTTP, access_token_len} ptr accessTokenLen
           #{poke EJDB_HTTP, blocking} ptr blocking
           #{poke EJDB_HTTP, read_anon} ptr read_anon
           #{poke EJDB_HTTP, max_body_size} ptr max_body_size


