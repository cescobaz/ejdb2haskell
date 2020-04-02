module Database.EJDB2.Query where

import qualified Data.Bool                   as Bool
import           Data.IORef
import           Data.Int

import           Database.EJDB2.Bindings.JQL
import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc

import           System.IO.Unsafe

data Query = Query { jql     :: JQL
                   , jqlPtr  :: ForeignPtr JQL
                   , strings :: IORef [ForeignPtr CChar]
                   }

-- | Create query object from specified text query. Collection must be specified in query.
fromString :: String -- ^ Query text
           -> IO Query
fromString string = do
    jqlPtr <- malloc
    withCString string $ \cString -> do
        c_jql_create jqlPtr nullPtr cString >>= checkRC
        jql <- peek jqlPtr
        jqlFPtr <- newForeignPtr p_finalizerJQL jqlPtr
        ioRef <- newIORef []
        return $ Query jql jqlFPtr ioRef

-- | Bind bool to query placeholder
setBool :: Bool
        -> String -- ^ Placeholder
        -> Query
        -> IO ()
setBool bool placeholder (Query jql _ _) =
    withCString placeholder $ \cPlaceholder ->
    c_jql_set_bool jql cPlaceholder 0 (CBool (Bool.bool 0 1 bool)) >>= checkRC

-- | Bind bool to query at specified index
setBoolAtIndex :: Bool
               -> Int -- ^ Index
               -> Query
               -> IO ()
setBoolAtIndex bool index (Query jql _ _) =
    c_jql_set_bool jql
                   nullPtr
                   (CInt $ fromIntegral index)
                   (CBool (Bool.bool 0 1 bool)) >>= checkRC

-- | Bind number to query placeholder
setI64 :: Int64
       -> String -- ^ Placeholder
       -> Query
       -> IO ()
setI64 number placeholder (Query jql _ _) = withCString placeholder $
    \cPlaceholder -> c_jql_set_i64 jql cPlaceholder 0 (CIntMax number)
    >>= checkRC

-- | Bind number to query at specified index
setI64AtIndex :: Int64
              -> Int -- ^ Index
              -> Query
              -> IO ()
setI64AtIndex number index (Query jql _ _) =
    c_jql_set_i64 jql nullPtr (CInt $ fromIntegral index) (CIntMax number)
    >>= checkRC

-- | Bind string to query placeholder
setString :: String
          -> String -- ^ Placeholder
          -> Query
          -> IO ()
setString string placeholder (Query jql _ _) =
    withCString placeholder $ \cPlaceholder -> withCString string $
    \cString -> c_jql_set_str jql cPlaceholder 0 cString >>= checkRC

-- | Bind string to query at specified index
setStringAtIndex :: String
                 -> Int -- ^ Index
                 -> Query
                 -> IO ()
setStringAtIndex string index (Query jql _ _) = withCString string $ \cString ->
    c_jql_set_str jql nullPtr (CInt $ fromIntegral index) cString >>= checkRC
