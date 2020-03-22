module Database.EJDB2.IWKV ( checkIWRC, checkIWRCFinally ) where

import           Database.EJDB2.Bindings.Types.IWKVBase

checkIWRC :: IWRC -> IO ()
checkIWRC iwrc = do
    let result = decodeResult iwrc
    if result == Ok then return () else fail $ show result

checkIWRCFinally :: IO a -> IWRC -> IO a
checkIWRCFinally computation iwrc = do
    let result = decodeResult iwrc
    if result == Ok
        then computation
        else do
            computation
            fail $ show result

