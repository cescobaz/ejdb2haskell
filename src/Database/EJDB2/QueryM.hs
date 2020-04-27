module Database.EJDB2.QueryM where

import Control.Monad.State.Lazy
import Control.Monad.Identity
import qualified Database.EJDB2.Query as Q
import Control.Monad.IO.Class

data Query a = Query String (BindM a)

type BindM a = StateT Q.Query IO a

bind :: BindM a -> Q.Query -> IO ()
bind bindM query = execStateT bindM query >> return ()

withQuery :: Query b -> (Q.Query -> IO a) -> IO a
withQuery (Query query bindM) f = Q.fromString query
    >>= \q -> bind bindM q >> f q >>= \value -> return value

setBool :: Bool -> String -> BindM ()
setBool bool placeholder = get >>= liftIO . Q.setBool bool placeholder
