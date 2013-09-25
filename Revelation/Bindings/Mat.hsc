{-# LANGUAGE ForeignFunctionInterface #-}
#include <bindings.dsl.h>
#include <mat.hpp>
module Revelation.Bindings.Mat where
#strict_import

import Revelation.Bindings.RawTypes
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#ccall cv_create_Mat , IO (Ptr <Mat>)
