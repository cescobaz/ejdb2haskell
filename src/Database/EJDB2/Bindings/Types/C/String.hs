module Database.EJDB2.Bindings.Types.C.String where

import           Control.Applicative

import           Foreign
import           Foreign.C.String

maybeStringToCString :: Maybe String -> IO CString
maybeStringToCString Nothing = return nullPtr
maybeStringToCString (Just string) = newCString string

cStringToMaybeString :: CString -> IO (Maybe String)
cStringToMaybeString cstring
    | cstring == nullPtr = return Nothing
    | otherwise = Just <$> peekCString cstring

maybeStringToCStringLen :: Maybe String -> IO CStringLen
maybeStringToCStringLen Nothing = return (nullPtr, 0)
maybeStringToCStringLen (Just string) = newCStringLen string

cStringLenToMaybeString :: CStringLen -> IO (Maybe String)
cStringLenToMaybeString (cstring, len)
    | cstring == nullPtr = return Nothing
    | otherwise = Just <$> peekCStringLen (cstring, len)
