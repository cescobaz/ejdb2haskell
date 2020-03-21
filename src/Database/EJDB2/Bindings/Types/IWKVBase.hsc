{-# LANGUAGE CPP #-}
module Database.EJDB2.Bindings.Types.IWKVBase where

import           Foreign
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>

type IWRC = CUIntMax

data IWResult =
      Ok                          -- No error.
    | ErrorFail                   -- Unspecified error.
    | ErrorErrno                  -- Error with expected errno status set.
    | ErrorIoErrno                -- IO error with expected errno status set.
    | ErrorNotExists              -- Resource is not exists.
    | ErrorReadonly               -- Resource is readonly.
    | ErrorAlreadyOpened          -- Resource is already opened.
    | ErrorThreading              -- Threading error.
    | ErrorThreadingErrno         -- Threading error with errno status set.
    | ErrorAssertion              -- Generic assertion error.
    | ErrorInvalidHandle          -- Invalid HANDLE value.
    | ErrorOutOfBounds            -- Invalid bounds specified.
    | ErrorNotImplemented         -- Method is not implemented.
    | ErrorAlloc                  -- Memory allocation failed.
    | ErrorInvalidState           -- Illegal state error.
    | ErrorNotAligned             -- Argument is not aligned properly.
    | ErrorFalse                  -- Request rejection/false response.
    | ErrorInvalidArgs            -- Invalid function arguments.
    | ErrorOverflow               -- Overflow.
    | ErrorInvalidValue           -- Invalid value.
    deriving ( Eq, Show )

decodeResult :: IWRC -> IWResult
decodeResult iwrc = case iwrc of
  #{const IW_OK}   ->                    Ok
  #{const IW_ERROR_FAIL}   ->            ErrorFail
  #{const IW_ERROR_ERRNO}   ->           ErrorErrno
  #{const IW_ERROR_IO_ERRNO}   ->        ErrorIoErrno
  #{const IW_ERROR_NOT_EXISTS}   ->      ErrorNotExists
  #{const IW_ERROR_READONLY}   ->        ErrorReadonly
  #{const IW_ERROR_ALREADY_OPENED}   ->  ErrorAlreadyOpened
  #{const IW_ERROR_THREADING}   ->       ErrorThreading
  #{const IW_ERROR_THREADING_ERRNO}   -> ErrorThreadingErrno
  #{const IW_ERROR_ASSERTION}   ->       ErrorAssertion
  #{const IW_ERROR_INVALID_HANDLE}   ->  ErrorInvalidHandle
  #{const IW_ERROR_OUT_OF_BOUNDS}   ->   ErrorOutOfBounds
  #{const IW_ERROR_NOT_IMPLEMENTED}   -> ErrorNotImplemented
  #{const IW_ERROR_ALLOC}   ->           ErrorAlloc
  #{const IW_ERROR_INVALID_STATE}   ->   ErrorInvalidState
  #{const IW_ERROR_NOT_ALIGNED}   ->     ErrorNotAligned
  #{const IW_ERROR_FALSE}   ->           ErrorFalse
  #{const IW_ERROR_INVALID_ARGS}   ->    ErrorInvalidArgs
  #{const IW_ERROR_OVERFLOW}   ->        ErrorOverflow
  #{const IW_ERROR_INVALID_VALUE}   ->   ErrorInvalidValue
  _                           ->       error $ "decodeError " ++ show iwrc
