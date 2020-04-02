{-# LANGUAGE CPP #-}

module Database.EJDB2.HTTPOptions
        ( HTTPOptions(..)
        , zero
        , HTTPOptionsB
        , options
        , build
        ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           Data.Int
import           Database.EJDB2.Bindings.Types.C.String

#include <ejdb2/ejdb2.h>

-- | EJDB HTTP\/Websocket Server options.
data HTTPOptions = HTTPOptions { enabled :: !Bool -- ^ If HTTP\/Websocket endpoint enabled. Default: false
                               , port :: !Int32 -- ^ Listen port number, required
                               , bind :: Maybe String -- ^ Listen IP\/host. Default: /localhost/
                               , accessToken :: Maybe String -- ^ Server access token passed in /X-Access-Token/ header. Default: zero
                               , blocking :: !Bool -- ^ Block 'open' thread until http service finished.
-- Otherwise HTTP server will be started in background.

                               , readAnon :: !Bool -- ^ Allow anonymous read-only database access
                               , maxBodySize :: !Word64 -- ^ Maximum WS\/HTTP API body size. Default: 64Mb, Min: 512K
                               }

-- | Create default 'HTTPOptions'
zero :: HTTPOptions
zero = HTTPOptions { enabled = False
                , port = 0
                , bind = Nothing
                , accessToken = Nothing
                , blocking = False
                , readAnon = False
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
           enabled <- #{peek EJDB_HTTP, enabled} ptr :: IO CInt
           port <- #{peek EJDB_HTTP, port} ptr
           bindPtr <- #{peek EJDB_HTTP, bind} ptr
           bindFPtr <- newForeignPtr finalizerFree nullPtr
           bind <- maybePeek peekCString bindPtr
           access_token <- #{peek EJDB_HTTP, access_token} ptr
           access_token_len <- #{peek EJDB_HTTP, access_token_len} ptr
           accessTokenFPtr <- newForeignPtr finalizerFree nullPtr
           accessToken <- maybePeek (\ptr -> peekCStringLen (ptr, access_token_len)) access_token
           blocking <- #{peek EJDB_HTTP, blocking} ptr :: IO CInt
           read_anon <- #{peek EJDB_HTTP, read_anon} ptr :: IO CInt
           max_body_size <- #{peek EJDB_HTTP, max_body_size} ptr
           return $ HTTPOptionsB
                      (HTTPOptions
                        (toBool enabled)
                        port
                        bind
                        accessToken
                        (toBool blocking)
                        (toBool read_anon)
                         max_body_size)
                      bindFPtr accessTokenFPtr (CSize $ fromIntegral access_token_len)
        poke ptr (HTTPOptionsB
                   (HTTPOptions enabled port _ _ blocking read_anon max_body_size)
                    bindPtr accessTokenPtr accessTokenLen) = do
           #{poke EJDB_HTTP, enabled} ptr (fromBool enabled :: CInt)
           #{poke EJDB_HTTP, port} ptr port
           withForeignPtr bindPtr $ \cBind ->
             #{poke EJDB_HTTP, bind} ptr cBind
           withForeignPtr accessTokenPtr $ \access_token ->
             #{poke EJDB_HTTP, access_token} ptr access_token
           #{poke EJDB_HTTP, access_token_len} ptr accessTokenLen
           #{poke EJDB_HTTP, blocking} ptr (fromBool blocking :: CInt)
           #{poke EJDB_HTTP, read_anon} ptr (fromBool read_anon :: CInt)
           #{poke EJDB_HTTP, max_body_size} ptr max_body_size


