{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types.EJDBExec where

import           Data.ByteString.Char8

import           Prelude           hiding ( log )

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Database.EJDB2.Bindings.Types.EJDB
import           Database.EJDB2.Bindings.JQL
import           Database.EJDB2.Bindings.Types.EJDBDoc
import           Database.EJDB2.Bindings.Types.IWKVBase

#include <ejdb2/ejdb2.h>

type EJDBExecVisitor = Ptr EJDBExec -> Ptr EJDBDoc -> Ptr CIntMax -> IO IWRC
type EJDB_EXEC_VISITOR = FunPtr EJDBExecVisitor

type IWXSTR = Ptr ()
type IWPOOL = Ptr ()

data EJDBExec = EJDBExec { db :: !EJDB
                         , q :: !JQL
                         , visitor :: !EJDB_EXEC_VISITOR
                         , opaque :: !(Ptr ())
                         , skip :: !CIntMax
                         , limit :: !CIntMax
                         , cnt :: !CIntMax
                         , log :: !IWXSTR
                         , pool :: !IWPOOL }

minimal :: EJDB -> JQL -> EJDB_EXEC_VISITOR -> EJDBExec
minimal db q visitor = EJDBExec { db = db
                                , q = q
                                , visitor = visitor
                                , opaque = nullPtr
                                , skip = 0
                                , limit = 0
                                , cnt = 0
                                , log = nullPtr
                                , pool = nullPtr }

zero :: EJDBExec
zero = EJDBExec { db = nullPtr
                , q = nullPtr
                , visitor = nullFunPtr
                , opaque = nullPtr
                , skip = 0
                , limit = 0
                , cnt = 0
                , log = nullPtr
                , pool = nullPtr }

instance Storable EJDBExec where
        sizeOf _ = #{size EJDB_EXEC}
        alignment _ = #{alignment EJDB_EXEC}
        peek ptr = do
           db <- #{peek EJDB_EXEC, db} ptr
           q <- #{peek EJDB_EXEC, q} ptr
           visitor <- #{peek EJDB_EXEC, visitor} ptr
           opaque <- #{peek EJDB_EXEC, opaque} ptr
           skip <- #{peek EJDB_EXEC, skip} ptr
           limit <- #{peek EJDB_EXEC, limit} ptr
           cnt <- #{peek EJDB_EXEC, cnt} ptr
           log <- #{peek EJDB_EXEC, log} ptr
           pool <- #{peek EJDB_EXEC, pool} ptr
           return $ EJDBExec db q visitor opaque skip limit cnt log pool
        poke ptr (EJDBExec db q visitor opaque skip limit cnt log pool) = do
           #{poke EJDB_EXEC, db} ptr db
           #{poke EJDB_EXEC, q} ptr q
           #{poke EJDB_EXEC, visitor} ptr visitor
           #{poke EJDB_EXEC, opaque} ptr opaque
           #{poke EJDB_EXEC, skip} ptr skip
           #{poke EJDB_EXEC, limit} ptr limit
           #{poke EJDB_EXEC, cnt} ptr cnt
           #{poke EJDB_EXEC, log} ptr log
           #{poke EJDB_EXEC, pool} ptr pool
