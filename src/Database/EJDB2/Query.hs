module Database.EJDB2.Query where

import qualified Data.Bool                   as Bool

import           Database.EJDB2.Bindings.IW
import           Database.EJDB2.Bindings.JQL

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc

import           System.IO.Unsafe

data Query = Query { jql :: JQL, jqlPtr :: ForeignPtr JQL }

-- | Create query object from specified text query. Collection must be specified in query.
fromString :: String -- ^ Query text
           -> IO Query
fromString string = do
    jqlPtr <- malloc
    withCString string $ \cString -> do
        c_jql_create jqlPtr nullPtr cString >>= checkRC
        jql <- peek jqlPtr
        jqlFPtr <- newForeignPtr p_finalizerJQL jqlPtr
        return $ Query jql jqlFPtr

-- | Bind bool to query placeholder
setBool :: Bool
        -> String -- ^ Placeholder
        -> Query
        -> IO ()
setBool bool placeholder (Query jql _) =
    withCString placeholder $ \cPlaceholder ->
    c_jql_set_bool jql cPlaceholder 0 (CBool (Bool.bool 0 1 bool)) >>= checkRC


