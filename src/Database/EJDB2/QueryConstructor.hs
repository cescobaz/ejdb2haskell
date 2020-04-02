module Database.EJDB2.QueryConstructor ( Query(..) ) where

import           Data.IORef

import           Database.EJDB2.Bindings.JQL

import           Foreign
import           Foreign.C.Types

-- | Query handle
data Query = Query { jql     :: JQL
                   , jqlPtr  :: ForeignPtr JQL
                   , strings :: IORef [ForeignPtr CChar]
                   }
