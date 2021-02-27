{-# LANGUAGE CPP #-}

module Database.EJDB2.Bindings.JBLStruct where


import           Foreign
import           Foreign.C.Types

#include <ejdb2/jbl.h>

type JBLIteratorPtr = Ptr ()

mallocJBLIterator :: IO (JBLIteratorPtr)
mallocJBLIterator = mallocBytes #{size JBL_iterator}

type JBLTypeT = CInt

data JBLType = JBVNone     -- Do not reorder
             | JBVNull
             | JBVBool     -- Do not reorder
             | JBVI64
             | JBVF64
             | JBVStr
             | JBVObject   -- Do not reorder
             | JBVArray
             deriving (Eq)

decodeJBLTypeT :: JBLTypeT -> JBLType
decodeJBLTypeT jblTypeT = case jblTypeT of
  #{const JBV_NONE} ->   JBVNone 
  #{const JBV_NULL} ->   JBVNull
  #{const JBV_BOOL} ->   JBVBool
  #{const JBV_I64} ->    JBVI64
  #{const JBV_F64} ->    JBVF64
  #{const JBV_STR} ->    JBVStr
  #{const JBV_OBJECT} -> JBVObject
  #{const JBV_ARRAY} ->  JBVArray
