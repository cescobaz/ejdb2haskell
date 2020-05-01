module Database.EJDB2.Query
    ( Query(..)
    , BindM
    , withQuery
    , noBind
    , setBool
    , setBoolAtIndex
    , setI64
    , setI64AtIndex
    , setF64
    , setF64AtIndex
    , setString
    , setStringAtIndex
    , setRegex
    , setRegexAtIndex
    , setNull
    , setNullAtIndex
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy

import qualified Data.Bool                   as Bool
import           Data.IORef
import           Data.Int

import           Database.EJDB2.Bindings.JQL
import           Database.EJDB2.Result

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc

-- | Query data with binding. Collection must be specified in query.
data Query a = Query String -- ^ Query text with collection
                     (BindM a)

data BindState = BindState JQL [CString]

-- | Monad to apply binding to 'Query'
type BindM a = StateT BindState IO a

bind :: BindM a -> BindState -> IO BindState
bind bindM state = execStateT bindM state

getJQL :: BindM JQL
getJQL = get >>= \(BindState jql _) -> return jql

noBind :: BindM ()
noBind = return ()

freeBindState :: BindState -> IO BindState
freeBindState (BindState jql cStrings) = mapM_ free cStrings
    >> return (BindState jql [])

withQuery :: Query a -> (JQL -> IO b) -> IO b
withQuery (Query query bindM) f = do
    (jqlPtr, jql) <- createQuery query
    bindState <- bind bindM (BindState jql [])
    result <- f jql
    destroyQuery jqlPtr
    freeBindState bindState
    return result

createQuery :: String -- ^ Query text
            -> IO (Ptr JQL, JQL)
createQuery string = do
    jqlPtr <- malloc
    withCString string $ \cString -> do
        c_jql_create jqlPtr nullPtr cString >>= checkRC
        jql <- peek jqlPtr
        return (jqlPtr, jql)

destroyQuery :: Ptr JQL -> IO ()
destroyQuery jqlPtr = c_jql_destroy jqlPtr >> free jqlPtr

-- | Bind bool to query placeholder
setBool :: Bool
        -> String -- ^ Placeholder
        -> BindM ()
setBool bool placeholder =
    getJQL >>= \jql -> liftIO $ withCString placeholder $ \cPlaceholder ->
    c_jql_set_bool jql cPlaceholder 0 (CBool (Bool.bool 0 1 bool)) >>= checkRC

-- | Bind bool to query at specified index
setBoolAtIndex :: Bool
               -> Int -- ^ Index
               -> BindM ()
setBoolAtIndex bool index = getJQL >>= \jql -> liftIO $
    c_jql_set_bool jql
                   nullPtr
                   (CInt $ fromIntegral index)
                   (CBool (Bool.bool 0 1 bool)) >>= checkRC

-- | Bind number to query placeholder
setI64 :: Int64
       -> String -- ^ Placeholder
       -> BindM ()
setI64 number placeholder =
    getJQL >>= \jql -> liftIO $ withCString placeholder $ \cPlaceholder ->
    c_jql_set_i64 jql cPlaceholder 0 (CIntMax number) >>= checkRC

-- | Bind number to query at specified index
setI64AtIndex :: Int64
              -> Int -- ^ Index
              -> BindM ()
setI64AtIndex number index = getJQL >>= \jql -> liftIO $
    c_jql_set_i64 jql nullPtr (CInt $ fromIntegral index) (CIntMax number)
    >>= checkRC

-- | Bind 'Double' to query placeholder
setF64 :: Double
       -> String -- ^ Placeholder
       -> BindM ()
setF64 number placeholder =
    getJQL >>= \jql -> liftIO $ withCString placeholder $ \cPlaceholder ->
    c_jql_set_f64 jql cPlaceholder 0 (CDouble number) >>= checkRC

-- | Bind 'Double' to query at specified index
setF64AtIndex :: Double
              -> Int -- ^ Index
              -> BindM ()
setF64AtIndex number index = getJQL >>= \jql -> liftIO $
    c_jql_set_f64 jql nullPtr (CInt $ fromIntegral index) (CDouble number)
    >>= checkRC

newCStringInBindState :: String -> BindM CString
newCStringInBindState string = get >>= \(BindState jql strings) ->
    liftIO (newCString string) >>= \cString ->
    put (BindState jql (cString : strings)) >> return cString

-- | Bind string to query placeholder
setString :: String
          -> String -- ^ Placeholder
          -> BindM ()
setString string placeholder = newCStringInBindState string
    >>= \cString -> getJQL >>= \jql -> liftIO $ withCString placeholder $
    \cPlaceholder -> c_jql_set_str jql cPlaceholder 0 cString >>= checkRC

-- | Bind string to query at specified index
setStringAtIndex :: String
                 -> Int -- ^ Index
                 -> BindM ()
setStringAtIndex string index =
    newCStringInBindState string >>= \cString -> getJQL >>= \jql -> liftIO $
    c_jql_set_str jql nullPtr (CInt $ fromIntegral index) cString >>= checkRC

-- | Bind regex to query placeholder
setRegex :: String -- ^ Regex
         -> String -- ^ Placeholder
         -> BindM ()
setRegex string placeholder = newCStringInBindState string
    >>= \cString -> getJQL >>= \jql -> liftIO $ withCString placeholder $
    \cPlaceholder -> c_jql_set_regexp jql cPlaceholder 0 cString >>= checkRC

-- | Bind regex to query at specified index
setRegexAtIndex :: String -- ^ Regex
                -> Int -- ^ Index
                -> BindM ()
setRegexAtIndex string index =
    newCStringInBindState string >>= \cString -> getJQL >>= \jql -> liftIO $
    c_jql_set_regexp jql nullPtr (CInt $ fromIntegral index) cString >>= checkRC

-- | Bind /null/ value to query placeholder
setNull :: String -- ^ Placeholder
        -> BindM ()
setNull placeholder = getJQL >>= \jql -> liftIO $ withCString placeholder $
    \cPlaceholder -> c_jql_set_null jql cPlaceholder 0 >>= checkRC

-- | Bind /null/ value to query at specified index
setNullAtIndex :: Int -- ^ Index
               -> BindM ()
setNullAtIndex index = getJQL >>= \jql -> liftIO $
    c_jql_set_null jql nullPtr (CInt $ fromIntegral index) >>= checkRC
