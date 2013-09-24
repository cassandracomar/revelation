{-# LANGUAGE ForeignFunctionInterface #-}
#include <bindings.dsl.h>
#include <types.hpp>
module Revelation.Bindings.Types where
#strict_import

import Revelation.Bindings.RawTypes
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#ccall std_create_string , CString -> CInt -> IO (Ptr <string>)
#ccall std_create_vector_int , Ptr CInt -> CSize -> IO (Ptr <vector_int>)
#ccall cv_create_Mat , IO (Ptr <Mat>)

toStdString :: String -> IO (Ptr C'string)
toStdString s = do  (cs, l) <- newCStringLen s
                    c'std_create_string cs (fromIntegral l)
