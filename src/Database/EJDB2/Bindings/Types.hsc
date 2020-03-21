{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.Types where

import           Data.ByteString.Char8

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>
type IWRC = CUIntMax

type IWKVOpenFlags = CUChar

data IWKV_WAL_OPTS = IWKV_WAL_OPTS !CBool  !CBool  !CUInt  !CUInt !CUIntMax  !CUChar 
instance Storable IWKV_WAL_OPTS where
        sizeOf _ = #{size IWKV_WAL_OPTS}
        alignment _  = #{alignment IWKV_WAL_OPTS}
        peek ptr = do
          enabled <- #{peek IWKV_WAL_OPTS, enabled} ptr
          check_crc_on_checkpoint <- #{peek IWKV_WAL_OPTS, check_crc_on_checkpoint} ptr
          savepoint_timeout_sec <- #{peek IWKV_WAL_OPTS, savepoint_timeout_sec} ptr
          checkpoint_timeout_sec <- #{peek IWKV_WAL_OPTS, checkpoint_timeout_sec} ptr
          wal_buffer_sz <- #{peek IWKV_WAL_OPTS, wal_buffer_sz} ptr
          checkpoint_buffer_sz <- #{peek IWKV_WAL_OPTS, checkpoint_buffer_sz} ptr
          return $ IWKV_WAL_OPTS enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz
        poke ptr (IWKV_WAL_OPTS enabled check_crc_on_checkpoint savepoint_timeout_sec checkpoint_timeout_sec wal_buffer_sz checkpoint_buffer_sz) = do
          #{poke IWKV_WAL_OPTS, enabled} ptr enabled
          #{poke IWKV_WAL_OPTS, check_crc_on_checkpoint} ptr check_crc_on_checkpoint
          #{poke IWKV_WAL_OPTS, savepoint_timeout_sec} ptr savepoint_timeout_sec
          #{poke IWKV_WAL_OPTS, checkpoint_timeout_sec} ptr checkpoint_timeout_sec
          #{poke IWKV_WAL_OPTS, wal_buffer_sz} ptr wal_buffer_sz
          #{poke IWKV_WAL_OPTS, checkpoint_buffer_sz} ptr checkpoint_buffer_sz


data IWKV_OPTS =
    IWKV_OPTS !(Ptr CString) !CUInt !CInt !IWKVOpenFlags !CBool !IWKV_WAL_OPTS
instance Storable IWKV_OPTS where
        sizeOf _ = #{size IWKV_OPTS}
        alignment _  = #{alignment IWKV_OPTS}
        peek ptr = do
                path <- #{peek IWKV_OPTS, path} ptr
                random_seed <- #{peek IWKV_OPTS, random_seed} ptr
                fmt_version <- #{peek IWKV_OPTS, fmt_version} ptr
                oflags <- #{peek IWKV_OPTS, oflags} ptr
                file_lock_fail_fast <- #{peek IWKV_OPTS, file_lock_fail_fast} ptr
                wal <- #{peek IWKV_OPTS, wal} ptr
                return $ IWKV_OPTS path random_seed fmt_version oflags file_lock_fail_fast wal
        poke ptr (IWKV_OPTS path random_seed fmt_version oflags file_lock_fail_fast wal) = do
                #{poke IWKV_OPTS, path} ptr path
                #{poke IWKV_OPTS, random_seed} ptr random_seed
                #{poke IWKV_OPTS, fmt_version} ptr fmt_version
                #{poke IWKV_OPTS, oflags} ptr oflags
                #{poke IWKV_OPTS, file_lock_fail_fast} ptr file_lock_fail_fast
                #{poke IWKV_OPTS, wal} ptr wal

type EJDBPtr = Ptr Int

data EJDB_HTTP = EJDB_HTTP !CBool !CInt !CString !CString !CBool !CBool !CUIntMax
instance Storable EJDB_HTTP where
        sizeOf _ = #{size EJDB_HTTP}
        alignment _ = #{alignment EJDB_HTTP}
        peek ptr = do
           enabled <- #{peek EJDB_HTTP, enabled} ptr
           port <- #{peek EJDB_HTTP, port} ptr
           bind <- #{peek EJDB_HTTP, bind} ptr
           access_token <- #{peek EJDB_HTTP, access_token} ptr
           blocking <- #{peek EJDB_HTTP, blocking} ptr
           read_anon <- #{peek EJDB_HTTP, read_anon} ptr
           max_body_size <- #{peek EJDB_HTTP, max_body_size} ptr
           return $ EJDB_HTTP enabled port bind access_token blocking read_anon max_body_size
        poke ptr (EJDB_HTTP enabled port bind access_token blocking read_anon max_body_size) = do
           #{poke EJDB_HTTP, enabled} ptr enabled
           #{poke EJDB_HTTP, port} ptr port
           #{poke EJDB_HTTP, bind} ptr bind
           #{poke EJDB_HTTP, access_token} ptr access_token
           #{poke EJDB_HTTP, blocking} ptr blocking
           #{poke EJDB_HTTP, read_anon} ptr read_anon
           #{poke EJDB_HTTP, max_body_size} ptr max_body_size

data EJDB_OPTS = EJDB_OPTS {
        ptr :: !(Ptr EJDB_OPTS)
      , iwkvOpts :: !IWKV_OPTS
      , ejdbHttp :: !EJDB_HTTP
      , noWal :: !CBool
      , sortBufferSz :: !CUInt
      , documentBufferSz :: !CUInt }
instance Storable EJDB_OPTS where
        sizeOf _ = #{size EJDB_OPTS}
        alignment _ = #{alignment EJDB_OPTS}
        peek ptr = do
           kv <- #{peek EJDB_OPTS, kv} ptr
           http <- #{peek EJDB_OPTS, http} ptr
           no_wal <- #{peek EJDB_OPTS, no_wal} ptr
           sort_buffer_sz <- #{peek EJDB_OPTS, sort_buffer_sz} ptr
           document_buffer_sz <- #{peek EJDB_OPTS, document_buffer_sz} ptr
           return $ EJDB_OPTS ptr kv http no_wal sort_buffer_sz document_buffer_sz
        poke ptr (EJDB_OPTS _ kv http no_wal sort_buffer_sz document_buffer_sz) = do
            #{poke EJDB_OPTS, kv} ptr kv
            #{poke EJDB_OPTS, http} ptr http
            #{poke EJDB_OPTS, no_wal} ptr no_wal
            #{poke EJDB_OPTS, sort_buffer_sz} ptr sort_buffer_sz
            #{poke EJDB_OPTS, document_buffer_sz} ptr document_buffer_sz
