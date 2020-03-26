{-# LANGUAGE CPP #-}
module Database.EJDB2.Bindings.IW where

import           Foreign
import           Foreign.C.Types

#include <ejdb2/ejdb2.h>

type RC = CUIntMax

checkRC :: RC -> IO ()
checkRC rc = do
    let result = decodeRC rc
    if result == Ok then return () else fail $ show result

checkRCFinally :: IO a -> RC -> IO a
checkRCFinally computation rc = do
    let result = decodeRC rc
    if result == Ok
        then computation
        else do
            computation
            fail $ show result

data Result =
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
    | ErrorNotFound                  -- Key not found (IWKV_ERROR_NOTFOUND) */
    | ErrorKeyExists                 -- Key already exists (IWKV_ERROR_KEY_EXISTS) */
    | ErrorMaxkvsz                   -- Size of Key+value must be not greater than 0xfffffff bytes (IWKV_ERROR_MAXKVSZ) */
    | ErrorCorrupted                 -- Database file invalid or corrupted (IWKV_ERROR_CORRUPTED) */
    | ErrorDupValueSize              -- Value size is not compatible for insertion into sorted values array (IWKV_ERROR_DUP_VALUE_SIZE) */
    | ErrorKeyNumValueSize           -- Given key is not compatible to storage as number (IWKV_ERROR_KEY_NUM_VALUE_SIZE)  */
    | ErrorIncompatibleDbMode        -- Incorpatible database open mode (IWKV_ERROR_INCOMPATIBLE_DB_MODE) */
    | ErrorIncompatibleDbFormat      -- Incompatible database format version, please migrate database data (IWKV_ERROR_INCOMPATIBLE_DB_FORMAT) */
    | ErrorCorruptedWalFile          -- Corrupted WAL file (IWKV_ERROR_CORRUPTED_WAL_FILE) */
    | ErrorValueCannotBeIncremented  -- Stored value cannot be incremented/descremented (IWKV_ERROR_VALUE_CANNOT_BE_INCREMENTED) */
    | ErrorWalModeRequired           -- Operation requires WAL enabled database. (IWKV_ERROR_WAL_MODE_REQUIRED) */
    | ErrorBackupInProgress          -- Backup operation in progress. (IWKV_ERROR_BACKUP_IN_PROGRESS) */
    | ErrorInvalidBuffer             -- Invalid JBL buffer (JBLERRORINVALIDBUFFER) */
    | ErrorCreation                   -- Cannot create JBL object (JBLERRORCREATION) */
    | ErrorInvalid                    -- Invalid JBL object (JBLERRORINVALID) */
    | ErrorParseJson                 -- Failed to parse JSON string (JBLERRORPARSEJSON) */
    | ErrorParseUnquotedString      -- Unquoted JSON string (JBLERRORPARSEUNQUOTEDSTRING) */
    | ErrorParseInvalidCodepoint    -- Invalid unicode codepoint/escape sequence (JBLERRORPARSEINVALIDCODEPOINT) */
    | ErrorParseInvalidUtf8         -- Invalid utf8 string (JBLERRORPARSEINVALIDUTF8) */
    | ErrorJsonPointer               -- Invalid JSON pointer (rfc6901) path (JBLERRORJSONPOINTER) */
    | ErrorPathNotFound              -- JSON object not matched the path specified (JBLERRORPATHNOTFOUND) */
    | ErrorPatchInvalid              -- Invalid JSON patch specified (JBLERRORPATCHINVALID) */
    | ErrorPatchInvalidOp           -- Invalid JSON patch operation specified (JBLERRORPATCHINVALIDOP) */
    | ErrorPatchNovalue              -- No value specified in JSON patch (JBLERRORPATCHNOVALUE) */
    | ErrorPatchTargetInvalid       -- Could not find target object to set value (JBLERRORPATCHTARGETINVALID) */
    | ErrorPatchInvalidValue        -- Invalid value specified by patch (JBLERRORPATCHINVALIDVALUE) */
    | ErrorPatchInvalidArrayIndex  -- Invalid array index in JSON patch path (JBLERRORPATCHINVALIDARRAYINDEX) */
    | ErrorNotAnObject              -- JBL is not an object (JBLERRORNOTANOBJECT) */
    | ErrorPatchTestFailed          -- JSON patch test operation failed (JBLERRORPATCHTESTFAILED) */
    deriving ( Eq, Show )

decodeRC :: RC -> Result
decodeRC rc = case rc of
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
  #{const IWKV_ERROR_NOTFOUND}                     ->  ErrorNotFound
  #{const IWKV_ERROR_KEY_EXISTS}                   ->  ErrorKeyExists
  #{const IWKV_ERROR_MAXKVSZ}                      ->  ErrorMaxkvsz
  #{const IWKV_ERROR_CORRUPTED}                    ->  ErrorCorrupted
  #{const IWKV_ERROR_DUP_VALUE_SIZE}               ->  ErrorDupValueSize
  #{const IWKV_ERROR_KEY_NUM_VALUE_SIZE}           ->  ErrorKeyNumValueSize
  #{const IWKV_ERROR_INCOMPATIBLE_DB_MODE}         ->  ErrorIncompatibleDbMode
  #{const IWKV_ERROR_INCOMPATIBLE_DB_FORMAT}       ->  ErrorIncompatibleDbFormat
  #{const IWKV_ERROR_CORRUPTED_WAL_FILE}           ->  ErrorCorruptedWalFile
  #{const IWKV_ERROR_VALUE_CANNOT_BE_INCREMENTED}  ->  ErrorValueCannotBeIncremented
  #{const IWKV_ERROR_WAL_MODE_REQUIRED}            ->  ErrorWalModeRequired
  #{const IWKV_ERROR_BACKUP_IN_PROGRESS}           ->  ErrorBackupInProgress
  #{const JBL_ERROR_INVALID_BUFFER}             ->  ErrorInvalidBuffer
  #{const JBL_ERROR_CREATION}                   ->  ErrorCreation
  #{const JBL_ERROR_INVALID}                    ->  ErrorInvalid
  #{const JBL_ERROR_PARSE_JSON}                 ->  ErrorParseJson
  #{const JBL_ERROR_PARSE_UNQUOTED_STRING}      ->  ErrorParseUnquotedString
  #{const JBL_ERROR_PARSE_INVALID_CODEPOINT}    ->  ErrorParseInvalidCodepoint
  #{const JBL_ERROR_PARSE_INVALID_UTF8}         ->  ErrorParseInvalidUtf8
  #{const JBL_ERROR_JSON_POINTER}               ->  ErrorJsonPointer
  #{const JBL_ERROR_PATH_NOTFOUND}              ->  ErrorPathNotFound
  #{const JBL_ERROR_PATCH_INVALID}              ->  ErrorPatchInvalid
  #{const JBL_ERROR_PATCH_INVALID_OP}           ->  ErrorPatchInvalidOp
  #{const JBL_ERROR_PATCH_NOVALUE}              ->  ErrorPatchNovalue
  #{const JBL_ERROR_PATCH_TARGET_INVALID}       ->  ErrorPatchTargetInvalid
  #{const JBL_ERROR_PATCH_INVALID_VALUE}        ->  ErrorPatchInvalidValue
  #{const JBL_ERROR_PATCH_INVALID_ARRAY_INDEX}  ->  ErrorPatchInvalidArrayIndex
  #{const JBL_ERROR_NOT_AN_OBJECT}              ->  ErrorNotAnObject
  #{const JBL_ERROR_PATCH_TEST_FAILED}          ->  ErrorPatchTestFailed
  _                                                ->  error $ "Database.EJDB2.Bindings.IW.decodeRC " ++ show rc
