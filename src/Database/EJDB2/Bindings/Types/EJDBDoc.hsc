{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBDoc where

import           Foreign
import           Foreign.C.Types

import           Database.EJDB2.Bindings.Types.JBL

#include <ejdb2/ejdb2.h>

data EJDBDoc = EJDBDoc { id :: !CIntMax
                       , raw :: !JBL
                       , node :: !JBLNode
                       , next :: !(Ptr EJDBDoc)
                       , prev :: !(Ptr EJDBDoc) }

instance Storable EJDBDoc where
        sizeOf _ = #{size struct _EJDB_DOC}
        alignment _ = #{alignment struct _EJDB_DOC}
        peek ptr = do
           id <- #{peek struct _EJDB_DOC, id} ptr
           raw <- #{peek struct _EJDB_DOC, raw} ptr
           node <- #{peek struct _EJDB_DOC, node} ptr
           next <- #{peek struct _EJDB_DOC, next} ptr
           prev <- #{peek struct _EJDB_DOC, prev} ptr
           return $ EJDBDoc id raw node next prev
        poke ptr (EJDBDoc id raw node next prev) = do
           #{poke struct _EJDB_DOC, id} ptr id
           #{poke struct _EJDB_DOC, raw} ptr raw
           #{poke struct _EJDB_DOC, node} ptr node
           #{poke struct _EJDB_DOC, next} ptr next
           #{poke struct _EJDB_DOC, prev} ptr prev
