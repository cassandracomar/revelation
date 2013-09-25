{-# LANGUAGE ForeignFunctionInterface #-}
#include <bindings.dsl.h>
#include <cpptypes.hpp>
module Revelation.Bindings.CppTypes where
#strict_import

{- Provides wrappers for core c++ types that Haskell doesn't have access to 
 - but opencv requires. -}
import Revelation.Bindings.RawTypes
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#ccall std_create_string , CString -> CInt -> IO (Ptr <string>)
#ccall std_create_vector_int , Ptr CInt -> CSize -> IO (Ptr <vector_int>)

toStdString :: String -> IO (Ptr C'string)
toStdString s = do  (cs, l) <- newCStringLen s
                    c'std_create_string cs (fromIntegral l)
